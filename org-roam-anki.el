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

; TODO add org-roam-anki-cloze-object

(defun org-roam-anki--get-matching-deck (element)
  "Returns first matching deck in element's taglist"
  (cond (seq-find
         (lambda (tag)
           (member tag org-roam-anki-decks))
         (org-get-tags element))
        (t org-roam-anki-fallback-deck)))

(defun org-roam-anki--get-export-taglist (element)
  "Returns taglist of element, minus extras"
  (cond ((equal org-roam-anki-export-extra-tags nil) nil)
        (t (let ((edited-taglist
                  (seq-remove
                   (lambda (tag)
                     (or (equal org-roam-anki-trigger-tag)
                         (equal org-roam-anki-mask-tag)
                         (member tag org-roam-anki-decks)
                         (member tag org-roam-anki-exclude-tags)))
                  (org-get-tags element))))
             (cond ((equal org-roam-anki-include-tags '()) edited-taglist)
                   (t (seq-remove
                       (lambda (tag)
                         (not (member tag org-roam-anki-include-tags))))
                       'edited-taglist))))))

(defun org-roam-anki--get-possible-pairs (element node)
  "Backend function for preparing to export org-roam notes. It will operate on the provided element or any sub-elements. If element is nil it will operate on the entire current buffer."
  (let ((cardlist '()))
      (org-element-map (org-element-parse-buffer) 'paragraph
        (lambda (paragraph)
          (let* ((lineage (org-element-lineage paragraph))
                 (heading (org-element-property
                           :raw-value
                           (seq-find (lambda (ancestor) (equal (car ancestor) 'headline))
                                     lineage)))
                 (tags (org-get-tags paragraph)))
            (and (member org-roam-anki-trigger-tag tags)
                 (not (member org-roam-anki-mask-tag tags))
                 (or (equal element nil)
                     (equal element paragraph)
                     (member element lineage))
                 (cond ((member heading org-roam-anki-standard-headings)
                        (push (cons (org-roam-node-title node)
                                    (org-element-interpret-data paragraph))
                              cardlist)))))))
      cardlist))

(defun org-roam-anki-export-heading ()
  "Export current heading and all subheadings as anki flashcards"
  (interactive)
  (org-roam-anki--get-possible-pairs (org-element-at-point) (org-roam-node-at-point)))

(defun org-roam-anki-export-buffer ()
  "Export current buffer and all subheadings as anki flashcards"
  (interactive)
  (org-roam-anki--get-possible-pairs nil (org-roam-node-at-point)))

(provide 'org-roam-anki)

;;; org-roam-anki.el ends here
