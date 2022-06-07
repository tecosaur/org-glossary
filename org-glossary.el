;;; org-glossary.el --- Defined terms and abbreviations in Org -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 TEC
;;
;; Author: TEC <tec@tecosaur.com>
;; Maintainer: TEC <tec@tecosaur.com>
;; Created: June 05, 2022
;; Modified: June 05, 2022
;; Version: 0.0.1
;; Keywords: abbrev docs tools
;; Homepage: https://github.com/tecosaur/org-glossary
;; Package-Requires: ((emacs "27.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Defined terms and abbreviations in Org
;;
;;; Plan:
;;
;; DONE extract term definitions from curent document
;;
;; DONE identify term references in the document
;;
;; DONE turn term references into numbered links
;;
;; DONE generate glossary section etc. based on used terms
;;
;; DONE add exporters for the glossary etc. links
;;
;; TODO make He export formatting/style customisable
;;
;; TODO load terms from #+include'd files
;;
;; TODO fontification of terms
;;
;; TODO jump to definition/usages
;;
;; TODO support term-links with multiple targets
;;
;; TODO M-x org-glossary-list-defined-terms
;; something vertico + maginalia style
;;
;; TODO M-x org-glossary-find-expanded-terms
;; this would be primaraly useful for acronyms.
;;
;; REVIEW maybe support generating the glossary/acronym etc.
;; in the file, like org-toc.?
;; This is complicated by the way we treat * Glossary sections etc.
;;
;; (long term)
;;
;; TODO abstract the short<->long/expansion part of this package into
;; its own thing
;;
;; TODO support looking for usages of a term in other files, maybe?
;;
;;; Code:

(require 'org)

(defgroup org-glossary nil
  "Defined terms and abbreviations in Org."
  :group 'org
  :prefix "org-glossary-")

(defcustom org-glossary-section "Glossary"
  "Outline heading containing term and acronym definitions.

During export, all subtrees starting with this heading will be removed."
  :type 'string)

(defcustom org-glossary-acronym-section "Acronyms"
  "Outline heading containing term and acronym definitions.

During export, all subtrees starting with this heading will be removed."
  :type 'string)

(defcustom org-glossary-index-section "Index Terms"
  "Outline heading containing a list of terms to be indexed.

During export, all subtrees starting with this heading will be removed."
  :type 'string)

(defcustom org-glossary-substitution-section "Text Substitutions"
  "Outline heading containing text substitution definitions.

During export, all subtrees starting with this heading will be removed."
  :type 'string)

(defcustom org-glossary-toplevel-only t
  "Whether all Glossary/Acronym definition sections must be toplevel."
  :type 'boolean)

(defcustom org-glossary-automatic t
  "Pick up on terms in plain text."
  :type 'boolean)

(defcustom org-glossary-acronym-plural-suffix "s"
  "The usual plural suffix, applied to acronyms."
  :type 'string)

(defcustom org-glossary-plural-function #'org-glossary-english-plural
  "A function which generates the plural form of a word."
  :type 'function)

(defvar-local org-glossary--terms nil
  "The currently known terms.")

(defun org-glossary--extract-terms (&optional parse-tree)
  "Find all terms defined in the current buffer.
Note that this removes definition values from PARSE-TREE by
side-effect when it is provided."
  (apply #'nconc
         (org-element-map
             (or parse-tree (org-element-parse-buffer))
             'headline
           (lambda (heading)
             (and (member (org-element-property :raw-value heading)
                          (list org-glossary-section
                                org-glossary-acronym-section
                                org-glossary-index-section
                                org-glossary-substitution-section))
                  (apply #'nconc
                         (org-element-map
                             (org-element-contents heading)
                             'plain-list
                           (lambda (lst)
                             (org-element-map
                                 (org-element-contents lst)
                                 'item
                               #'org-glossary--entry-from-item
                               nil nil 'item))))))
           nil nil
           (and org-glossary-toplevel-only 'headline))))

(defun org-glossary--entry-from-item (item)
  "Destructively build a glossary entry from a ITEM."
  (let* ((term-str (substring-no-properties
                    (or (car (org-element-property :tag item))
                        (string-trim
                         (org-element-interpret-data
                          (org-element-contents item))))))
         (keys-terms (split-string term-str "[ \t]*=[ \t]*"))
         (term-and-plural (split-string (car (last keys-terms)) "[ \t]*,[ \t]*"))
         (term (car term-and-plural))
         (plural (or (cadr term-and-plural)
                     (funcall org-glossary-plural-function term)))
         (key-and-plural (split-string (car keys-terms) "[ \t]*,[ \t]*"))
         (key (car key-and-plural))
         (key-plural (or (cadr key-and-plural)
                         (funcall org-glossary-plural-function key)))
         (type (org-glossary--entry-type
                (org-element-lineage item '(headline))))
         (value (pcase type
                  ('acronym
                   (org-element-contents
                    (org-element-extract-element
                     (car (org-element-contents item)))))
                  ('index nil)
                  (_ (mapcar
                      #'org-element-extract-element
                      (org-element-contents item))))))
    (list :key key
          :key-plural key-plural
          :term term
          :term-plural plural
          :type type
          :value value
          :uses nil)))

(defun org-glossary--entry-type (datum)
  "Determine whether DATUM is a glossary or acronym entry."
  (unless (or (null datum) (eq 'org-data (org-element-type datum)))
    (or (and (eq 'headline (org-element-type datum))
             (let ((rawval (org-element-property :raw-value datum)))
               (cond ((string= rawval org-glossary-section)
                      'glossary)
                     ((string= rawval org-glossary-acronym-section)
                      'acronym)
                     ((string= rawval org-glossary-index-section)
                      'index)
                     ((string= rawval org-glossary-substitution-section)
                      'substitution))))
        (org-glossary--entry-type (org-element-lineage datum '(headline))))))

(defun org-glossary--construct-regexp (terms)
  "Create a regexp to find all occurances of TERMS.
The first match group is the non-plural form of the term,
the second match group indicates plurality, as specified with
`org-glossary-acronym-plural-suffix'."
  (let ((terms-collect
         (lambda (terms key)
           (apply #'nconc
                  (mapcar
                   (lambda (trm)
                     (if (eq 'acronym (plist-get trm :type))
                         (list (plist-get trm key))
                       (let* ((term-str (plist-get trm key))
                              (term-letter1 (aref term-str 0)))
                         (if (eq term-letter1 (upcase term-letter1))
                             (list term-str)
                           (list term-str (concat (string (upcase term-letter1))
                                                  (substring term-str 1)))))))
                   terms)))))
    (concat "\\<"
            (regexp-opt (funcall terms-collect terms :key-plural) t)
            "\\|"
            (regexp-opt (funcall terms-collect terms :key) t)
            "\\>")))

(defun org-glossary--apply-terms (terms &optional no-modify)
  "Replace occurances of the TERMS with links.
This modifies TERMS to record uses of each term.

When NO-MODIFY is non-nil, the reference will be lodged in
TERMS but the buffer content left unmodified."
  (let ((terms-rx (org-glossary--construct-regexp terms))
        (search-spaces-regexp "[ \t\n][ \t]*")
        (case-fold-search nil)
        terms-used element-context)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward terms-rx nil t)
        (setq element-context
              (save-match-data (org-element-context (org-element-at-point))))
        (cond
         ((org-glossary--within-definition-p element-context) nil) ; skip
         ((eq 'link (org-element-type element-context))
          (push (plist-get (org-glossary--update-link
                            terms element-context no-modify)
                           :key)
                terms-used))
         ((and org-glossary-automatic
               (memq 'link (org-element-restriction element-context)))
          (push (plist-get (org-glossary--update-plain
                            terms no-modify)
                           :key)
                terms-used)))))
    (setq terms-used (cl-delete-duplicates (delq nil terms-used) :test #'string=))
    (delq nil
          (mapcar
           (lambda (trm)
             (when (member (plist-get trm :key) terms-used)
               trm))
           terms))))

(defun org-glossary--within-definition-p (datum)
  "Whether DATUM exists within a term definition subtree."
  (when datum
    (if (and (eq 'headline (org-element-type datum))
             (org-glossary--definition-heading-p datum))
        t
      (org-glossary--within-definition-p
       (org-element-lineage datum '(headline))))))

(defun org-glossary--definition-heading-p (heading)
  "Whether HEADING is recognised as a definition heading."
  (and (member (org-element-property :raw-value heading)
               (list org-glossary-section
                     org-glossary-acronym-section
                     org-glossary-index-section
                     org-glossary-substitution-section))
       (or (= 1 (org-element-property :level heading))
           (not org-glossary-toplevel-only))))

(defun org-glossary--update-link (terms link &optional no-modify)
  "Register LINK's reference to a term in TERMS, and update numbering.
When NO-MODIFY is non-nil, the reference will be lodged in
TERMS but the buffer content left unmodified."
  (when (member (org-element-property :type link)
                '("gls" "glspl" "Gls" "Glspl"))
    (let* ((trm (replace-regexp-in-string
                 "^.+?:" ""
                 (org-element-property :path link)))
           (term-entry (org-glossary--find-term-entry terms trm :key))
           (index (org-glossary--record-term-usage term-entry link)))
      (org-element-put-property
       link :path (concat (number-to-string index) ":" trm))
      (unless no-modify
        (replace-region-contents
         (org-element-property :begin link)
         (org-element-property :end link)
         (org-element-link-interpreter link nil)))
      term-entry)))

(defun org-glossary--update-plain (terms &optional no-modify)
  "Register a reference to a term in TERMS, and convert to a link.
It is assumed that the term reference has just been matched with
a regexp of the form given by `org-glossary--construct-regexp'
and the match data is intact.

When NO-MODIFY is non-nil, the reference will be lodged in
TERMS but the buffer content left unmodified."
  (let ((term-str
         (replace-regexp-in-string
          "[ \n\t]+" " "
          (substring-no-properties
           (or (match-string 1) (match-string 2)))))
        (plural-p (match-string 1))
        (case-fold-search nil)
        capitalized-p term-entry)
    (setq term-entry
          (org-glossary--find-term-entry
           terms term-str (if plural-p :key-plural :key)))
    (unless term-entry
      (setq term-entry
            (org-glossary--find-term-entry
             terms
             (concat (string (downcase (aref term-str 0)))
                     (substring term-str 1))
             (if plural-p :key-plural :key)))
      (when term-entry
        (setq capitalized-p t)))
    (when term-entry
      (unless no-modify
        (replace-match
         (org-glossary--term-replacement
          term-entry (1+ (length (plist-get term-entry :uses)))
          plural-p capitalized-p)
         t t))
      (org-glossary--record-term-usage
       term-entry (org-element-context (org-element-at-point)))
      term-entry)))

(defun org-glossary--find-term-entry (terms term-key key)
  "Find any term in TERMS where KEY is TERM-KEY."
  (cl-some (lambda (trm)
             (when (string= term-key (plist-get trm key))
               trm))
           terms))

(defun org-glossary--record-term-usage (term-entry record)
  "Record TERM-ENTRY's usage with RECORD, and give the use index."
  (let* ((uses (plist-get term-entry :uses))
         (index (1+ (length uses))))
    (plist-put term-entry :uses (nconc uses (list (cons index record))))
    uses))

(defun org-glossary--clear-term-usage (term-entry)
  "Clear the :uses slot of TERM-ENTRY."
  (plist-put term-entry :uses nil))

(defun org-glossary--term-replacement (term-entry &optional index plural-p capitalized-p)
  "Construct a string refering to the TERM-ENTRY"
  (pcase (plist-get term-entry :type)
    ((or 'glossary 'acronym 'index)
     (org-element-interpret-data
      `(link
        (:type ,(cond
                 ((and plural-p capitalized-p) "Glspl")
                 (capitalized-p "Gls")
                 (plural-p "glspl")
                 (t "gls"))
         :path ,(if index
                    (concat (number-to-string index)
                            ":" (plist-get term-entry :key))
                  (plist-get term-entry :key))
         :format bracket))))
    ('substitution
     (let ((text (string-trim
                  (save-match-data (org-element-interpret-data
                                    (plist-get term-entry :value))))))
       (concat (when capitalized-p (string (upcase (aref text 0))))
               (if capitalized-p (substring text 1) text)
               (when plural-p org-glossary-acronym-plural-suffix))))
    (_ (match-string 0))))

(defun org-glossary--print-terms (terms &optional types level)
  "Produce an org-mode AST defining TERMS.
Do this for each of TYPES (by default: glossary, acronym, and index),
producing a headline of level LEVEL (by default: 1)."
  (let ((assembled-terms (org-glossary--assemble-terms terms types)))
    (mapcar
     (lambda (type)
       (let ((secname (pcase type
                        ('glossary "Glossary")
                        ('acronym "Acronyms")
                        ('index "Index"))))
         `(headline
           (:raw-value ,secname :level ,(or level 1) :title ,secname :post-blank 1)
           (section
            nil
            ,@(org-glossary--print-terms-by-letter
               (alist-get type assembled-terms))))))
     (or types '(glossary acronym index)))))

(defun org-glossary--print-terms-by-letter (assembled-terms)
  "Produce an org-mode AST with definitions for ASSEMBLED-TERMS."
  (let* ((terms-per-letter
          (mapcar (lambda (tms) (length (cdr tms)))
                  assembled-terms))
         (use-letters-p
          (and (> (apply #'+ terms-per-letter) 15)
               (> (apply #'max terms-per-letter) 3))))
    (mapcar
     (lambda (letter-terms)
       (let ((letter (car letter-terms))
             (terms (cdr letter-terms)))
         (apply #'nconc
                (when use-letters-p
                  `((paragraph (:post-blank 1) (bold nil ,(string (upcase letter))) "\n")))
                (mapcar
                 (lambda (trm)
                   `((paragraph (:post-blank nil)
                                (link (:type "glsdef"
                                       :path ,(plist-get trm :key)
                                       :format bracket)
                                      (bold nil ,(plist-get trm :key)))
                                (entity (:name "emsp" :use-brackets-p t)))
                     ,@(plist-get trm :value)
                     (paragraph (:post-blank 1)
                                ,@(cl-subseq
                                   (apply #'nconc
                                          (mapcar
                                           (lambda (use)
                                             `((link
                                                (:type "glsuse"
                                                 :path ,(concat
                                                         (number-to-string (car use))
                                                         ":"
                                                         (plist-get trm :key))
                                                 :format bracket
                                                 :post-blank nil))
                                               ", "))
                                           (plist-get trm :uses)))
                                   0 -1))))
                 terms))))
     assembled-terms)))

(defun org-glossary--assemble-terms (terms &optional types)

  "Collect TERMS into the form ((type . (first-char . sorted-terms)...)...).
When a list of TYPES is provided, only terms which are of one of the provided
types will be used."
  (mapcar
   (lambda (type)
     (cons type
           (let ((type-terms
                  (sort
                   (delq nil
                         (mapcar
                          (lambda (trm)
                            (when (eq type (plist-get trm :type))
                              trm))
                          terms))
                   (lambda (t1 t2)
                     (string< (plist-get t1 :key)
                              (plist-get t2 :key))))))
             (mapcar
              (lambda (first-char)
                (cons first-char
                      (delq nil
                            (mapcar
                             (lambda (trm)
                               (when (eq first-char
                                         (aref (plist-get trm :key) 0))
                                 trm))
                             type-terms))))
              (cl-delete-duplicates
               (mapcar (lambda (trm) (aref (plist-get trm :key) 0))
                       type-terms))))))
   (or types (cl-delete-duplicates
              (mapcar (lambda (trm) (plist-get trm :type)) terms)))))


(defun org-glossary--strip-headings (&optional data _backend info remove-from-buffer)
  "Remove glossary headlines."
  (let ((data (or data (org-element-parse-buffer)))
        regions-to-delete)
    (org-element-map
        data
        'headline
      (lambda (heading)
        (when (org-glossary--definition-heading-p heading)
          (if remove-from-buffer
              (push (list (org-element-property :begin heading)
                          (org-element-property :end heading))
                    regions-to-delete)
            (org-element-extract-element heading))))
      info
      nil
      (and org-glossary-toplevel-only 'headline))
    (mapc
     (lambda (region)
       (apply #'delete-region region))
     regions-to-delete)
    data))

;;; Link definitions

(org-link-set-parameters
 "gls" :export #'org-glossary--link-export-gls)
(org-link-set-parameters
 "glspl" :export #'org-glossary--link-export-glspl)
(org-link-set-parameters
 "Gls" :export #'org-glossary--link-export-Gls)
(org-link-set-parameters
 "Glspl" :export #'org-glossary--link-export-Glspl)

(defun org-glossary--link-export-gls (it _description backend info)
  "Export a gls link to term IT with BACKEND."
  (org-glossary--link-export it backend info nil nil))

(defun org-glossary--link-export-glspl (it _description backend info)
  "Export a glspl link to term IT with BACKEND."
  (org-glossary--link-export it backend info t nil))

(defun org-glossary--link-export-Gls (it _description backend info)
  "Export a Gls link to term IT with BACKEND."
  (org-glossary--link-export it backend info nil t))

(defun org-glossary--link-export-Glspl (it _description backend info)
  "Export a Glspl link to term IT with BACKEND."
  (org-glossary--link-export it backend info t t))

(defun org-glossary--link-export (index-term backend info &optional plural-p capitalized-p)
  "Export a link to TERM with BACKEND, respecting PLURAL-P and CAPITALIZED-P."
  (if-let ((index (if (seq-contains-p index-term ?:)
                      (string-to-number (car (split-string index-term ":")))
                    1))
           (trm (replace-regexp-in-string "^.+?:" "" index-term))
           (term-entry (org-glossary--find-term-entry
                        org-glossary--terms trm :key)))
      (let* ((term-text (plist-get term-entry
                                   (if plural-p :term-plural :term)))
             (term-text-c (if capitalized-p (capitalize term-text) term-text))
             (value
              (cond
               ((org-export-derived-backend-p backend 'latex)
                (format "\\hyperlink{gls-%s}{\\label{gls-%s-use-%s}%s}"
                        trm trm index term-text-c))
               ((org-export-derived-backend-p backend 'html)
                ;; TODO use title="definition...",
                ;; `org-export-data' seems handy but would need to escape quotes etc.
                (format "<a class=\"org-gls\" href=\"#gls.%s\" id=\"glsr.%s.%s\">%s</a>"
                        trm trm index term-text-c))
               (t term-text-c))))
        (if (and (eq 'acronym (plist-get term-entry :type))
                 (eq 1 index))
            (format "%s (%s)"
                    (concat
                     (string-trim
                      (org-export-data (plist-get term-entry :value) info))
                     (when plural-p
                       org-glossary-acronym-plural-suffix))
                    value)
          value))
    (funcall (if capitalized-p #'capitalize #'identity)
             (funcall (if plural-p org-glossary-plural-function #'identity)
                      trm))))

(org-link-set-parameters
 "glsdef" :export #'org-glossary--link-export-glsdef)
(org-link-set-parameters
 "glsuse" :export #'org-glossary--link-export-glsuse)

(defun org-glossary--link-export-glsdef (key term backend _info)
  (let* ((term-entry (org-glossary--find-term-entry
                      org-glossary--terms key :key))
         (key (plist-get term-entry :key)))
    (cond
     ((org-export-derived-backend-p backend 'latex)
      (format "\\hypertarget{gls-%s}{%s}" key term))
     ((org-export-derived-backend-p backend 'html)
      (format "<span class=\"org-glsdef\" id=\"gls.%s\">%s</span>"
              key term))
     (t term))))

(defun org-glossary--link-export-glsuse (index-term _desc backend _info)
  (let* ((index (if (seq-contains-p index-term ?:)
                    (string-to-number (car (split-string index-term ":")))
                  1))
         (trm (replace-regexp-in-string "^.+?:" "" index-term)))
    (cond
     ((org-export-derived-backend-p backend 'latex)
      (format "\\pageref{gls-%s-use-%s}" trm index))
     ((org-export-derived-backend-p backend 'html)
      (format "<a class=\"org-glsdef\" href=\"#glsr.%s.%s\">%s</a>"
              trm index index))
     (t index))))

;;; Pluralisation

(defun org-glossary-english-plural (word)
  "Generate the plural form of WORD."
  (let (case-fold-search)
    (cond
     ((string-match-p "[^aeiou]o$" word)
      (concat word "es"))
     ((string-match-p "\\(?:is\\|ss\\|sh\\|ch\\|x\\|z\\)$" word)
      (concat word "es"))
     ((string-match-p "us$" word)
      (concat (substring word 0 -2) "i"))
     ((string-match-p "on$" word)
      (concat (substring word 0 -2) "a"))
     ((string-match-p "^[a-z].*[^aeiou]y$" word)
      (concat (substring word 0 -1) "ies"))
     ((string-match-p "^[a-z].*[aeiou]y$" word)
      (concat word "s"))
     (t (concat word "s")))))

;;; Hooking into org-mode


(defun org-glossary--prepare-buffer (&optional _backend)
  "Modify the buffer to resolve all defined terms, prepearing it for export.
This should only be run as an export hook."
  (setq org-glossary--terms (org-glossary--extract-terms))
  (org-glossary--strip-headings nil nil nil t)
  (let* ((used-terms (org-glossary--apply-terms org-glossary--terms))
         (glossary-section (org-glossary--print-terms used-terms)))
    (save-excursion
      (goto-char (point-max))
      (insert "\n" (org-element-interpret-data glossary-section)))))

(defun org-glossary-update-terms ()
  "Update the currently known terms."
  (interactive)
  (setq org-glossary--terms (org-glossary--extract-terms))
  (org-glossary--apply-terms org-glossary--terms t))

(add-hook 'org-export-before-parsing-hook #'org-glossary--prepare-buffer)

(provide 'org-glossary)
;;; org-glossary.el ends here
