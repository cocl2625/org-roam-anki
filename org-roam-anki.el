;;; org-roam-anki.el --- Flashcard generator for org-roam

;; Copyright (C) 2022 Connor Clark

;; Author: Connor Clark <cocl2625@gmail.com>
;; Version: 0.1.0
;; Package-Requires ((anki-editor "0.3.3") (org-roam "2.2.2"))
;; Keywords: org-roam, anki
;; URL: https://github.com/cocl2625/org-roam-anki

;;; Commentary:

;; Org-roam-anki is an emacs package that allows the user to export notes taken
;; in org-roam to the flashcard application Anki without excessive formatting
;; requirements that would subtract from the natural note-taking process.

;; Further documentation can be found in the README in the project's git repo.

;;; Code:

(require 'org)
(require 'org-roam)
(require 'anki-editor)

(defgroup org-roam-anki nil
  "Customizations for org-roam-anki"
  :group 'org)

(defcustom org-roam-anki-standard-model
  "Basic (and reversed card)"
  "Model to use when exporting standard anki cards"
  :type 'string
  :group 'org-roam-anki)

(defcustom org-roam-anki-cloze-model
  "Cloze"
  "Model to use when exporting cloze anki cards"
  :type 'string
  :group 'org-roam-anki)

(defcustom org-roam-anki-decks
  '()
  "List of deck names to attempt to match with tags in org-roam"
  :type '(repeat string)
  :group 'org-roam-anki)

(defcustom org-roam-anki-fallback-deck
  "Default"
  "Deck to use if no matches are found in org-roam-anki-decks"
  :type 'string
  :group 'org-roam-anki)

(defcustom org-roam-anki-trigger-tag
  "anki"
  "Tag to trigger anki export"
  :type 'string
  :group 'org-roam-anki)

(defcustom org-roam-anki-mask-tag
  "anki_mask"
  "Tag to mask anki trigger tag (needed due to tag inheritance)"
  :type 'string
  :group 'org-roam-anki)

(defcustom org-roam-anki-export-extra-tags
  t
  "Whether to export additional org tags as anki tags"
  :type 'boolean
  :group 'org-roam-anki)

(defcustom org-roam-anki-include-tags
  '()
  "List of tags to include when exporting to anki -- empty list means include all"
  :type '(repeat string)
  :group 'org-roam-anki)

(defcustom org-roam-anki-exclude-tags
  '()
  "List of tags to exclude when exporting to anki -- empty list means exclude none"
  :type '(repeat string)
  :group 'org-roam-anki)

(defcustom org-roam-anki-standard-headings
  '("Definition" "Meaning" "Translation")
  "List of heading titles to signify a standard anki card"
  :type '(repeat string)
  :group 'org-roam-anki)

; TODO Add org-roam-anki-cloze-object

(defun org-roam-anki--clean-text (text)
  "Removes text properties and leading/trailing whitespace"
  (string-trim (substring-no-properties text)))

(defun org-roam-anki--format-text (text)
  "Converts org formatting to html formatting"
  (org-export-string-as text 'html t))

(defun org-roam-anki--get-deck (element)
  "Returns first matching deck in element's taglist"
  (or (seq-find
       (lambda (tag)
         (member tag org-roam-anki-decks))
       (org-get-tags element))
      org-roam-anki-fallback-deck))

(defun org-roam-anki--get-taglist (element)
  "Returns taglist of element, minus extras"
  (cond ((equal org-roam-anki-export-extra-tags nil) '())
        (t (let ((taglist '()))
             (seq-do
              (lambda (tag)
                (or (equal tag org-roam-anki-trigger-tag)
                    (equal tag org-roam-anki-mask-tag)
                    (member tag org-roam-anki-decks)
                    (member tag org-roam-anki-exclude-tags)
                    (not (or (equal org-roam-anki-include-tags '())
                             (member tag )))
                    (push (org-roam-anki--clean-text tag) taglist)))
              (org-get-tags element))
             taglist))))

(defun org-roam-anki--get-cardlist (node &optional element)
  "Backend function for preparing to export org-roam notes. It will operate on the
entire node and all sub-elements unless a specific element is provided, in which case
it will operate on that element and all sub-elements"
  (let ((cardlist '()))
      (org-element-map (org-element-parse-buffer) 'paragraph
        (lambda (content)
          (let* ((lineage (org-element-lineage content))
                 (heading (org-element-property
                           :raw-value
                           (seq-find
                            (lambda (ancestor)
                              (equal (car ancestor) 'headline))
                            lineage)))
                 (tags (org-get-tags content)))
            (and (member org-roam-anki-trigger-tag tags)
                 (not (member org-roam-anki-mask-tag tags))
                 (or (equal element nil)
                     (equal element content)
                     (member element lineage))
                 (cond ((member heading org-roam-anki-standard-headings)
                        (push `((type . "Standard")
                                (topic . ,(org-roam-node-title node))
                                (content . ,(org-roam-anki--format-text
                                             (org-roam-anki--clean-text
                                              (org-element-interpret-data content))))
                                (deck . ,(org-roam-anki--get-deck content))
                                (model . ,org-roam-anki-standard-model)
                                (taglist . ,(org-roam-anki--get-taglist content)))
                              cardlist))
                       (t nil))))))
      cardlist))

; TODO Make this function return different values based on success/failure
(defun org-roam-anki--export-cardlist (cardlist)
  (seq-do
   (lambda (card)
     (anki-editor--anki-connect-invoke "addNote"
       `((note . ((deckName . ,(alist-get 'deck card))
                  (modelName . ,(alist-get 'model card))
                  (fields . ((Front . ,(alist-get 'topic card))
                             (Back . ,(alist-get 'content card))))
                  (tags . ,(alist-get 'taglist card)))))))
   cardlist))

(defun org-roam-anki-export-heading ()
  "Export current heading and all subheadings as anki flashcards"
  (interactive)
  (org-roam-anki--export-cardlist
   (org-roam-anki--get-cardlist
    (org-roam-node-at-point) (org-element-at-point))))

(defun org-roam-anki-export-buffer ()
  "Export current buffer and all subheadings as anki flashcards"
  (interactive)
  (org-roam-anki--export-cardlist
   (org-roam-anki--get-cardlist (org-roam-node-at-point))))

(provide 'org-roam-anki)

;;; org-roam-anki.el ends here
