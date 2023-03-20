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
;; Package-Requires: ((emacs "28.1") (org "9.6"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Defined terms and abbreviations in Org
;;
;;; Plan:
;;
;; TODO jump to usages
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
;; TODO support references inside glossary definitions, being careful
;; to avoid circular references.
;;
;; (long term)
;;
;; TODO abstract the short<->long/expansion part of this package into
;; its own thing
;;
;; TODO support looking for usages of a term in other files, maybe?
;;
;; TODO support plural forms in substitution display.
;;
;;; Code:

(require 'org)
(require 'org-element)

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x)
  (require 'ox))

(defgroup org-glossary nil
  "Defined terms and abbreviations in Org."
  :group 'org
  :prefix "org-glossary-")

(defvar org-glossary--heading-names) ; For the byte-compiler.
(defvar org-glossary-mode)           ; For the byte-compiler.

(defcustom org-glossary-headings
  '(("Glossary" . glossary)
    ("Acronyms" . acronym)
    ("Index" . index)
    ("Text Substitutions" . substitution))
  "An alist of special heading names and their correspanding type.
During export, all matching headings will be removed in their
entirity (including subtrees).

If setting this outside of the customisation interface, be sure
to update `org-glossary--heading-names' appropriately."
  :type '(alist :key-type (string :tag "Heading title")
                :value-type (symbol :tag "Entry type"))
  :set (lambda (symbol value)
         (setq org-glossary--heading-names
               (mapcar #'car value))
         (set-default-toplevel-value symbol value)))

(defvar org-glossary--heading-names
  (mapcar #'car org-glossary-headings)
  "The heading names which correspand to a glossary type.")

(defcustom org-glossary-toplevel-only t
  "Whether all glossary definition sections must be toplevel.
If nil, they will be recognised anywhere in the document."
  :type 'boolean)

(defcustom org-glossary-automatic t
  "Pick up on terms in plain text."
  :type 'boolean)

(defcustom org-glossary-plural-function #'org-glossary-english-plural
  "A function which generates the plural form of a word."
  :type 'function)

(defcustom org-glossary-canonicalise-aliases nil
  "Whether aliases should be canonicalised."
  :type 'boolean)

(defcustom org-glossary-group-ui t
  "Group term definitions by type.

In practice, if using Emacs 28, this allows you to turn off
grouping, and add the target type to the annotation instead."
  :type 'boolean)

(defcustom org-glossary-global-terms nil
  "A list of globally availible term sources.

Each term should either be a string interpreted as an #+include
keyword's value, or a plist of the form emitted by
`org-glossary--parse-include-value'."
  :type '(repeat (choice string plist)))

(defcustom org-glossary-collection-root nil
  "A base path prefixed to any per-document glossary sources.
If this is set to a directory, ensure that you include the trailing slash."
  :type '(choice (const nil) (string :tag "Path")))

(defvar-local org-glossary--extra-term-sources nil
  "A list of locations outside the current document that should be sourced from.
This is constructed using `org-glossary--get-extra-term-sources'.
See its docstring for more information.")

(defcustom org-glossary-default-print-parameters
  '(:type (glossary acronym index)
    :level 0
    :consume nil
    :all nil
    :only-contents nil)
  "The default print parameters.
These can be set by #+print_glossary in babel :key value style."
  :type 'plist)

(defcustom org-glossary-print-letter-minimums '(12 . 3)
  "The minimum number of terms with distinct and the same letter to print letters.
More specifically, a cons cell containing:
- The minimum number of distinct first letters among the used terms
- The minimum maximum number of terms with the same letter
before the :letter-heading templates of
`org-glossary-export-specs' should be applied.

For instance, with the default value of \\=(12 . 3) the terms
must start with at least 12 different letters, and there must be
at least three terms that start with the same letter."
  :type '(cons integer integer))

(defcustom org-glossary-export-specs
  '((t (t :use "%t"
          :first-use "%u"
          :definition "%t"
          :backref "%r"
          :backref-seperator ", "
          :heading ""
          :category-heading "* %c\n"
          :letter-heading "*%L*\n"
          :definition-structure-preamble ""
          :definition-structure "*%d*\\emsp{}%v\\ensp{}%b\n"
          :alias-value "See [[gls:0:%k]].")
       (glossary :heading "* Glossary")
       (acronym :heading "* Acronyms"
                :first-use "%v (%u)")
       (index :heading "* Index"
              :definition-structure "%d\\ensp{}%b\n")
       (substitution :heading ""
                     :use "%v"
                     :definition-structure ""
                     :category-heading ""
                     :letter-heading ""))
    (latex (t :use "\\protect\\hyperlink{gls-%K}{\\label{gls-%K-use-%r}%t}"
              :definition "\\hypertarget{gls-%K}{%t}"
              :backref "\\pageref{gls-%K-use-%r}"))
    (html (t :use "<a class=\"org-gls\" href=\"#gls.%K\" id=\"glsr.%K.%r\">%t</a>"
             :definition "<span class=\"org-glsdef\" id=\"gls.%K\">%t</span>"
             :backref "<a class=\"org-glsdef\" href=\"#glsr.%K.%r\">%r</a>"))
    (ascii (t :definition-structure "*%d* %v [%n uses]\n")
           (index :definition-structure "%d [%n uses]\n"))
    (org (t :use "<<gr;%K;%r>>[[g;%K][%t]]"
            :backref "[[gr;%K;%r][%r]]"
            :definition-structure "- <<g;%K>>%t :: %v\\ensp{}%b")
         (index :definition-structure "- <<g;%K>>%t\\ensp{}%b")))
  "Alist of export backends and template set alists.
Each template set alist has the term type (e.g. acronym) as the
car, and the templates set as the cdr.

The backend set associated with t is used as the default backend,
and likewise the template set associated with t used as the the
default template set.

Each template set is a plist with term forms as the keys and
the templates for the forms as the the values.

The following term forms as recognised for all template specs:
  :use
  :first-use
  :backref
  :definition
There are also four special forms for the default template spec:
  :definition-structure
  :category-heading
  :letter-heading
  :alias-value

Within each template, the following format specs are applied:
  %t the term
  %v the term value
  %k the term key
  %K the term key nonce
  %r the term reference index (applicable to :use, :first-use,
       :backref, and :alias-value)
  %n the number of term references (i.e. max %r)
  %c the category of the term

In :use and :first-use, %t/%v are pluralised and capitalised as
appropriate. The :first-use template can also use %u to refer to
the value of :use.

The default backend defines four special forms, expanded at the
start of the export process:
- The :definition-structure form is used as the template for the
  whole definition entry, and uses the format specs %d, %v, %b
  for the definition term, value, and backreferences respectively.
- The :letter-heading form is inserted before a block of terms
  starting with the letter, given by the format spec %l and %L in
  lower and upper case respectively.
- The :category-heading form is inserted before a block of terms
  all assigned a particular category, given by the format spec %c.
- The :alias-value form is used as the value (%v) when expanding an alias
  definition. Most other values are inherited from the canonical form.

Instead of a format string, one can also provide a function as a
template spec so long as it matches the function signature of
`org-glossary--export-template'.

The literal content of :definition-structure-preamble is inserted
before the first :definition-structure in each block of
definitions.

If using cleverref with LaTeX, making use of the \\labelcpageref
command like so is recommended:

  (org-glossary-set-export-spec 'latex t
    :backref \"gls-%k-use-%r\"
    :backref-seperator \",\"
    :definition-structure
    \"*%d*\\emsp{}%v\\ensp{}@@latex:\\ifnum%n>0 \\labelcpageref{@@%b@@latex:}\\fi@@\n\")

TODO rewrite for clarity."
  :type '(alist :key-type (symbol :tag "Backend")
                :value-type
                (alist :key-type (symbol :tag "Type")
                       :value-type
                       (plist :value-type
                              (string :tag "Template")))))

(defcustom org-glossary-fontify-types-differently t
  "Whether to use the org-glossary-TYPE-term faces.
Or just use the org-glossary-term face for everything."
  :type 'boolean)

(defcustom org-glossary-fontify-type-faces
  '((glossary . org-glossary-glossary-term)
    (acronym . org-glossary-acronym-term)
    (index . org-glossary-index-term)
    (substitute . org-glossary-substitution-term))
  "An alist of types and the faces that should be used.
This only applies when `org-glossary-fontify-types-differently'
is non-nil."
  :type '(alist :key-type (symbol :tag "Type")
                :value-type face))

(defcustom org-glossary-snippet-fontication-hooks
  ;; Known desirable hooks, if they exist.
  (cl-remove-if-not
   (lambda (h)
     (memq h '(org-toggle-pretty-entities +org-pretty-mode
               org-modern-mode org-glossary-mode)))
   org-mode-hook)
  "A stripped down version of `org-mode-hook' for fontification.
This is bound as `org-mode-hook' during snippet/substitution fontification.
It allows for expensive but unneeded hooks to be skipped.

See also `org-glossary-fontify-displayed-substitute'."
  :type '(repeat function))

(defcustom org-glossary-display-substitute-value t
  "Whether to display substitutions as their value.
Requires `org-glossary-fontify-types-differently' to be non-nil.

See also `org-glossary-fontify-displayed-substitute'."
  :type 'boolean)

(defcustom org-glossary-fontify-displayed-substitute t
  "Whether to fontify displayed substitutions values.
Requires `org-glossary-display-substitute-value' to be non-nil.

See also `org-glossary-snippet-fontication-hooks'."
  :type 'boolean)

(defface org-glossary-term
  '((t :inherit (org-agenda-date-today org-link) :weight normal))
  "Base face used for term references.")

(defface org-glossary-glossary-term
  '((t :inherit org-glossary-term))
  "Face used for term references.")

(defface org-glossary-acronym-term
  '((t :inherit org-glossary-term))
  "Face used for term references.")

(defface org-glossary-index-term
  '((t :inherit (org-scheduled-previously org-glossary-term)))
  "Face used for term references.")

(defface org-glossary-substitution-term
  '((t :inherit (org-archived org-glossary-term)))
  "Face used for term references.")

(defface org-glossary-substituted-value
  '((t :inherit default))
  "Face used for substitution values.")

(defvar-local org-glossary--terms nil
  "The currently known terms.")

(defvar org-glossary--paths-to-update nil
  "List of paths whos terms should be updated.")

(defvar org-glossary--path-update-timer nil
  "The timer which will update paths, should it exist.")

(defvar org-glossary--path-dependencies nil
  "Alist of definition source paths and their dependent paths.")

(defcustom org-glossary-idle-update-period 0.5
  "Idle time in seconds used when updating term definitions.
Set to nil to disable automatic update propagation."
  :type '(choice number (const nil :tag "Do not update")))

;;; Obtaining term definitions

(defun org-glossary--get-terms (&optional path-spec no-extra-sources already-included)
  "Obtain all known terms in the current buffer.
Terms from `org-glossary--extra-term-sources' will be added
unless PATH-SPEC is non-nil and NO-EXTRA-SOURCES nil."
  (let* ((path-spec (org-glossary--complete-path-spec path-spec))
         (term-source
          (and (not (member path-spec already-included))
               (org-glossary--get-terms-oneshot path-spec))))
    (setq already-included (nconc already-included (list path-spec)))
    (org-glossary--maybe-add-extra-terms
     #'org-glossary--get-terms
     (apply #'append
            (plist-get term-source :terms)
            (mapcar (lambda (p) (org-glossary--get-terms p t already-included))
                    (cl-set-difference (plist-get term-source :included)
                                       already-included)))
     (not no-extra-sources)
     already-included)))

(defun org-glossary--maybe-add-extra-terms (term-getter term-set do-it-p &optional already-included)
  "Apply TERM-GETTER to extra term sources add them to TERM-SET.
The extra terms sources are the elements of `org-glossary--extra-term-sources'.
TERM-GETTER will be called with three arguments:
- the term source
- t
- ALREADY-INCLUDED

If DO-IT-P is nil, then nothing will be done and TERM-SET will be returned."
  (if do-it-p
      (let ((accumulation term-set))
        (dolist (term-source (cl-set-difference
                              (mapcar #'org-glossary--complete-path-spec
                                      org-glossary--extra-term-sources)
                              already-included))
          (if (or (bufferp term-source)
                  (file-exists-p (plist-get term-source :file)))
              (setq accumulation
                    (append accumulation
                            (funcall term-getter term-source t already-included)))
            (display-warning
             '(org-glossary missing-source)
             (format "Glossary source `%s' does not exist! Skipping..."
                     (plist-get term-source :file)))))
        accumulation)
    term-set))

(defun org-glossary--get-terms-oneshot (&optional path-spec)
  "Optain all terms defined in PATH-SPEC."
  (let* ((path-spec (org-glossary--complete-path-spec path-spec))
         (path-buffer
          (cond
           ((bufferp path-spec) path-spec)
           ((equal (plist-get path-spec :file)
                   (buffer-file-name))
            (current-buffer))))
         (parse-tree
          (if path-buffer
              (with-current-buffer path-buffer
                (org-with-wide-buffer
                 (org-element-parse-buffer)))
            (with-temp-buffer
              (setq buffer-file-name (plist-get path-spec :file))
              (org-glossary--include-once path-spec)
              (set-buffer-modified-p nil)
              (org-with-wide-buffer
               (org-element-parse-buffer)))))
         (terms (org-glossary--extract-terms parse-tree)))
    (list :path path-spec
          :scan-time (current-time)
          :terms terms
          :terms-hash (sxhash terms)
          :included
          (mapcar
           (lambda (location)
             (org-glossary--parse-include-value
              location (and (plist-get path-spec :file)
                            (file-name-directory (plist-get path-spec :file)))))
           (org-element-map parse-tree 'keyword
             (lambda (kwd)
               (when (string= "INCLUDE" (org-element-property :key kwd))
                 (org-element-property :value kwd)))))
          :extra-term-sources
          (org-glossary--get-extra-term-sources parse-tree))))

(defun org-glossary--complete-path-spec (&optional path-spec)
  "Given a tentative PATH-SPEC, try to get a proper one.
The PATH-SPEC is formed with respect to the current buffer."
  (or (and (stringp path-spec)
           (org-glossary--parse-include-value path-spec))
      path-spec
      (and (buffer-file-name)
           (org-glossary--parse-include-value
            (format "%S" (buffer-file-name))))
      (current-buffer)))

(defun org-glossary--include-once (parameters)
  "Include content based on PARAMETERS."
  (unless (eq (plist-get parameters :env) 'literal)
    (require 'ox)
    (let ((lines (plist-get parameters :lines))
          (file (plist-get parameters :file))
          (location (plist-get parameters :location))
          (org-inhibit-startup t))
      (org-mode)
      (insert
       (org-export--prepare-file-contents
        file
        (if location
            (org-export--inclusion-absolute-lines
             file location
             (plist-get parameters :only-contents)
             lines)
          lines)
        0
        (plist-get parameters :minlevel)
        nil nil
        (buffer-file-name))))))

(defun org-glossary--parse-include-value (value &optional dir)
  "Extract the useful parameters from #+include: VALUE.
The file name is resolved against DIR."
  (when value
    (let* (location
           (file
            (and (string-match "^\\(\".+?\"\\|\\S-+\\)\\(?:\\s-+\\|$\\)" value)
                 (prog1
                     (save-match-data
                       (let ((matched (match-string 1 value))
                             stripped)
                         (when (string-match "\\(::\\(.*?\\)\\)\"?\\'"
                                             matched)
                           (setq location (match-string 2 matched))
                           (setq matched
                                 (replace-match "" nil nil matched 1)))
                         (setq stripped (org-strip-quotes matched))
                         (if (org-url-p stripped)
                             stripped
                           (expand-file-name stripped dir))))
                   (setq value (replace-match "" nil nil value)))))
           (only-contents
            (and (string-match ":only-contents *\\([^: \r\t\n]\\S-*\\)?"
                               value)
                 (prog1 (org-not-nil (match-string 1 value))
                   (setq value (replace-match "" nil nil value)))))
           (lines
            (and (string-match
                  ":lines +\"\\([0-9]*-[0-9]*\\)\""
                  value)
                 (prog1 (match-string 1 value)
                   (setq value (replace-match "" nil nil value)))))
           (env (cond
                 ((string-match "\\<example\\>" value) 'literal)
                 ((string-match "\\<export\\(?: +\\(.*\\)\\)?" value)
                  'literal)
                 ((string-match "\\<src\\(?: +\\(.*\\)\\)?" value)
                  'literal)))
           ;; Minimal level of included file defaults to the
           ;; child level of the current headline, if any, or
           ;; one.  It only applies is the file is meant to be
           ;; included as an Org one.
           (minlevel
            (and (not env)
                 (if (string-match ":minlevel +\\([0-9]+\\)" value)
                     (prog1 (string-to-number (match-string 1 value))
                       (setq value (replace-match "" nil nil value)))
                   (get-text-property (point)
                                      :org-include-induced-level)))))
      (list :file (if (org-url-p file) file
                    (expand-file-name file dir))
            :location location
            :only-contents only-contents
            :line lines
            :env env
            :minlevel minlevel))))

;;; Term cache

(defvar org-glossary--terms-cache nil
  "Cached definition sources.
An alist with entries of the form:
  (PATH-SPEC . TERM-CACHE-PLIST)

where PATH-SPEC is an absolute #+include path, and TERM-CACHE-PLIST
a plist of the form:
  (:path FILE-PATH-OR-URL
   :scan-time TIME-LIST
   :terms TERM-LIST
   :terms-hash (sxhash TERM-LIST)
   :included LIST-OF-PATH-SPECS
   :extra-term-sources LIST-OF-PATH-STRINGS)")

(defvar org-glossary--quicklookup-cache) ; For the byte-compiler.
(defvar org-glossary--help-echo-cache) ; For the byte-compiler.

(defun org-glossary--get-terms-cached (&optional path-spec no-extra-sources already-included)
  "Obtain all known terms in the current buffer, using the cache.
`org-glossary--extra-term-sources' will be used unless PATH-SPEC
is non-nil and NO-EXTRA-SOURCES nil.

If a source that could have contributed to the quicklookup cache is updated,
then the quicklookup cache (`org-glossary--quicklookup-cache') will be cleared."
  (let* ((path-spec (org-glossary--complete-path-spec path-spec))
         (term-source-cached (assoc path-spec org-glossary--terms-cache))
         (cached-path (plist-get (cdr term-source-cached) :path))
         (cached-file (plist-get cached-path :file))
         (cache-valid
          (and term-source-cached
               (not (bufferp cached-path))
               (or (org-url-p cached-file)
                   (and (file-exists-p cached-file)
                        (if (equal cached-file (buffer-file-name))
                            (not (buffer-modified-p)) t)
                        (not ; scan time >= mtime (scan time !< mtime)
                         (time-less-p (plist-get (cdr term-source-cached) :scan-time)
                                      (file-attribute-modification-time
                                       (file-attributes cached-file))))))))
         (term-source
          (or (and (member path-spec already-included)
                   '(:terms nil))
              (and term-source-cached
                   (if cache-valid t
                     (delq term-source-cached org-glossary--terms-cache)
                     nil)
                   (cdr term-source-cached))
              (cdar (push (cons path-spec
                                (org-glossary--get-terms-oneshot path-spec))
                          org-glossary--terms-cache)))))
    ;; If updating a source that could already be part of the quicklookup cache,
    ;; then clear the quicklookup cache to prevent outdated entries from persisting.
    ;; We should also clear the help-echo cache while we're at it.
    (when (and (not (hash-table-empty-p org-glossary--quicklookup-cache))
               term-source-cached (not cache-valid))
      (setq org-glossary--quicklookup-cache (make-hash-table :test #'equal)
            org-glossary--help-echo-cache (make-hash-table :test #'equal)))
    (setq already-included (nconc already-included (list path-spec)))
    (org-glossary--maybe-add-extra-terms
     #'org-glossary--get-terms-cached
     (apply #'append
            (plist-get term-source :terms)
            (mapcar (lambda (p) (org-glossary--get-terms-cached p t already-included))
                    (cl-set-difference (plist-get term-source :included)
                                       already-included)))
     (not no-extra-sources)
     already-included)))

(defun org-glossary-clear-cache ()
  "Clear the global term cache."
  (interactive)
  (setq org-glossary--terms-cache nil))

;;; Term identification

(defun org-glossary--extract-terms (&optional parse-tree)
  "Find all terms defined in the current buffer.
Note that this removes definition values from PARSE-TREE by
side-effect when it is provided."
  (let* ((parse-tree (or parse-tree
                         (org-with-wide-buffer
                          (org-element-parse-buffer))))
         (buffer-file-name (org-element-property :path parse-tree)))
    (apply #'nconc
           (org-element-map
               parse-tree
               'headline
             (lambda (heading)
               (and (member (org-element-property :raw-value heading)
                            org-glossary--heading-names)
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
             (and org-glossary-toplevel-only 'headline)))))

(defun org-glossary--entry-from-item (item)
  "Destructively build a glossary entry from a ITEM."
  (let* ((term-str (string-trim
                    (substring-no-properties
                     (org-element-interpret-data
                      (or (org-element-property :tag item)
                          (org-element-contents item))))))
         (case-fold-search nil)
         (sentancecase-to-lowercase
          (lambda (word)
            (if (or (string-match-p "^[[:upper:]][^[:space:]][^[:upper:]]+$" word)
                    ;; NOTE This following rule makes sense, but unfortunately is
                    ;; english-only. It would be good to support a similar case in
                    ;; more languages at some point.
                    (string-match-p "^\\(?:A\\|An\\)[[:space:]][^[:upper:]]+$" word))
                (concat (string (downcase (aref word 0))) (substring word 1))
              word)))
         (keys-terms (split-string term-str "[ \t]*=[ \t]*"))
         (term-and-plural (split-string (car (last keys-terms)) "[ \t]*,[ \t]*"))
         (term (funcall sentancecase-to-lowercase (car term-and-plural)))
         (plural (funcall sentancecase-to-lowercase
                          (or (cadr term-and-plural)
                              (funcall org-glossary-plural-function term))))
         (key-and-plural (split-string (car keys-terms) "[ \t]*,[ \t]*"))
         (key (funcall sentancecase-to-lowercase (car key-and-plural)))
         (key-plural (funcall sentancecase-to-lowercase
                              (or (cadr key-and-plural)
                                  (funcall org-glossary-plural-function key))))
         (type-category (org-glossary--entry-type-category
                         (org-element-lineage item '(headline))))
         (item-contents (and (org-element-property :tag item)
                             (org-element-contents item)))
         (value (mapcar #'org-element-extract-element item-contents)))
    (list :key key
          :key-plural (and (not (string-empty-p key-plural))
                           (not (string= key key-plural))
                           key-plural)
          :key-nonce (org-glossary--key-nonce key)
          :term term
          :term-plural plural
          :alias-for nil
          :type (car type-category)
          :category (cdr type-category)
          :value value
          :definition-file (or (buffer-file-name) (current-buffer))
          :definition-pos (+ (org-element-property :begin item) 2)
          :extracted nil
          :uses nil)))

(defvar org-glossary--category-heading-tag) ; For the byte-compiler.

(defun org-glossary--entry-type-category (datum)
  "Determine whether DATUM is a glossary or acronym entry."
  (cond
   ((null datum) nil)
   ((eq (org-element-type datum) 'org-data) nil)
   ((eq (org-element-type datum) 'headline)
    (let* ((title (org-element-property :raw-value datum))
           (type (alist-get title org-glossary-headings
                            nil nil #'string=))
           (recurse (unless type (org-glossary--entry-type-category
                                  (org-element-lineage datum '(headline)))))
           case-fold-search)
      (cond
       (type (cons type nil))
       ((member org-glossary--category-heading-tag
                (org-element-property :tags datum))
        (cons (car recurse) title))
       (t recurse))))
   (t (org-glossary--entry-type-category (org-element-lineage datum '(headline))))))

(defvar org-glossary--category-heading-tag "category"
  "The tag signifying that the heading correspands to a category of terms.")

(defun org-glossary--identify-alias-terms (terms)
  "Search for aliases in TERMS, and update term entries accordingly."
  (let ((key-term-map (make-hash-table :test #'equal :size (length terms))))
    (dolist (term-entry terms)
      (puthash (plist-get term-entry :key) term-entry key-term-map))
    (dolist (term-entry terms)
      (when-let ((value (plist-get term-entry :value))
                 (value-str (string-trim (org-element-interpret-data value)))
                 (associated-term (gethash value-str key-term-map)))
        (plist-put term-entry :alias-for associated-term)
        (plist-put term-entry :value (plist-get associated-term :value)))))
  terms)

(defvar org-glossary--key-nonces (make-hash-table :test #'equal)
  "The currently set key nonces.")

(defun org-glossary--key-nonce (key)
  "Return the nonce for KEY, assigning one if not already set."
  (or (gethash key org-glossary--key-nonces)
      (puthash key (1+ (hash-table-count org-glossary--key-nonces))
               org-glossary--key-nonces)))

;;; Term usage

(defun org-glossary-apply-terms (terms &optional no-modify no-number keep-unused)
  "Replace occurances of the TERMS with links.
This returns a copy of TERMS with references recorded in :uses.

When NO-MODIFY is non-nil, neither buffer content nor TERMS will be modified.
When NO-NUMBER is non-nil, all links created or modified shall not include
a reference number.
When KEEP-UNUSED is non-nil, unused terms will be included in the result."
  (interactive (list org-glossary--terms nil t))
  (let ((terms-mrx (org-glossary--mrx-construct-from-terms terms))
        (search-spaces-regexp "[ \t\n][ \t]*")
        (start-time (float-time))
        (last-redisplay (float-time))
        terms-used element-context element-at-point)
    (let ((register-term
           (lambda (term-entry)
             (push (plist-get term-entry :key) terms-used)
             (when (plist-get term-entry :alias-for)
               (push (plist-get (plist-get term-entry :alias-for) :key)
                     terms-used)))))
      (setq terms (org-glossary--strip-uses terms))
      (org-glossary--identify-alias-terms terms)
      (save-excursion
        (goto-char (point-min))
        (while (org-glossary--mrx-search-forward terms-mrx)
          (when (> (- (float-time) last-redisplay) 0.4)
            (let (message-log-max)
              (message "Scanning for term usage: (%s%%)"
                       (/ (* 100 (point)) (point-max))))
            (sit-for 0.01)
            (setq last-redisplay (float-time)))
          (save-match-data
            (setq element-at-point (org-element-at-point)
                  element-context (org-element-context element-at-point)))
          (cond
           ((or (org-glossary--within-definition-p element-context)
                (eq 'headline (org-element-type element-at-point))
                (and (eq 'keyword (org-element-type element-at-point))
                     (not (member (org-element-property :key element-at-point)
                                  org-element-parsed-keywords))))
            nil) ; Skip, not a valid reference.
           ((eq 'link (org-element-type element-context))
            (funcall register-term
                     (org-glossary--update-link
                      terms element-context no-modify no-number)))
           ((and org-glossary-automatic
                 (memq 'link (org-element-restriction element-context)))
            (funcall register-term
                     (org-glossary--update-plain
                      terms no-modify no-number))))))
      (when (> (- (float-time) start-time) 0.1)
        (message "Scanned for term usage in buffer (took %.2f seconds)."
                 (- (float-time) start-time)))
      (if keep-unused
          terms
        (setq terms-used (cl-delete-duplicates (delq nil terms-used) :test #'string=))
        (delq nil
              (mapcar
               (lambda (trm)
                 (when (member (plist-get trm :key) terms-used)
                   trm))
               terms))))))

(defun org-glossary--strip-uses (terms &optional no-modify)
  "Either modify or return a copy of TERMS with all :uses set to nil.
Behaviour is set according to NO-MODIFY."
  (if no-modify
      (mapcar
       (lambda (trm)
         (if (plist-get trm :uses)
             (org-combine-plists trm '(:uses nil))
           trm))
       terms)
    (dolist (term-entry terms)
      (plist-put term-entry :uses nil))
    terms))

(defvar org-glossary--mrx-last-tag nil
  "The tag of the last multi-rx matched by `org-glossary--multi-rx'.")

(defun org-glossary--mrx-search-forward (tagged-patterns &optional limit case-insensitive)
  "Find the closest matching pattern in TAGGED-PATTERNS before LIMIT.
Each entry in the list TAGGED-PATTERNS should be of the form:
  (TAG . STRINGS)

In the case of two equally close matches from TAGGED-PATTERNS,
the longest match will be used.

`case-fold-search' is locally bound to CASE-INSENSITIVE.

This is necessitated by problems when trying to apply
`regexp-opt' to many items, which can trigger:
  Lisp error: (invalid-regexp \"Regular expression too big\")"
  (let ((match-start most-positive-fixnum)
        (match-stop -1)
        (case-fold-search case-insensitive)
        the-match tag)
    (dolist (t-pat tagged-patterns)
      (save-excursion
        (when (and (re-search-forward (cdr t-pat) limit t)
                   (or (< (match-beginning 0) match-start)
                       (and (= (match-beginning 0) match-start)
                            (> (match-end 0) match-stop))))
          (setq the-match (match-data)
                match-start (match-beginning 0)
                match-stop (match-end 0)
                tag (car t-pat)))))
    (set-match-data the-match)
    (and the-match
         (setq org-glossary--mrx-last-tag tag)
         (goto-char match-stop))))

(defvar org-glossary--mrx-max-bin-size 800
  "The maximum number of strings that should be combined into a single regexp.
Larger is better, however typically 'invalid-regexp \"Regular
expression too big\"' is seen with around 1000+ terms.")

(defun org-glossary--mrx-construct (&rest tagged-strings)
  (let (bins accumulated (n 0))
    (dolist (tag-strs tagged-strings)
      (dolist (str (delete-dups (sort (copy-sequence (cdr tag-strs)) #'string<)))
        (if (< n org-glossary--mrx-max-bin-size)
            (progn (push str accumulated)
                   (setq n (1+ n)))
          (push (cons (car tag-strs) (regexp-opt accumulated 'words)) bins)
          (setq accumulated nil
                n 0)))
      (when accumulated
        (push (cons (car tag-strs) (regexp-opt accumulated 'words)) bins)
        (setq accumulated nil
              n 0)))
    bins))

(defun org-glossary--mrx-construct-from-terms (terms)
  "Construct a multi-rx from TERMS."
  (let ((term-collect
         (lambda (terms key)
           (apply #'nconc
                  (mapcar
                   (lambda (trm)
                     (when-let ((term-str (plist-get trm key))
                                (term-letter1 (aref term-str 0)))
                       (if (eq term-letter1 (upcase term-letter1))
                           (list term-str)
                         (list term-str (concat (string (upcase term-letter1))
                                                (substring term-str 1))))))
                   terms)))))
    (org-glossary--mrx-construct
     (cons 'singular (funcall term-collect terms :key))
     (cons 'plural (funcall term-collect terms :key-plural)))))

(defun org-glossary--within-definition-p (datum)
  "Whether DATUM exists within a term definition subtree."
  (when datum
    (if (and (eq 'headline (org-element-type datum))
             (org-glossary--definition-heading-p datum))
        t
      (org-glossary--within-definition-p
       (save-match-data (org-element-lineage datum '(headline)))))))

(defun org-glossary--definition-heading-p (heading)
  "Whether HEADING is recognised as a definition heading."
  (and (member (org-element-property :raw-value heading)
               org-glossary--heading-names)
       (or (= 1 (org-element-property :level heading))
           (not org-glossary-toplevel-only))))

(defun org-glossary--update-link (terms link &optional no-modify no-number)
  "Register LINK's reference to a term in TERMS, and update numbering.
When NO-MODIFY is non-nil, the reference will be lodged in
TERMS but the buffer content left unmodified.
When NO-NUMBER is non-nil, no reference number shall be inserted."
  (when (member (org-element-property :type link)
                '("gls" "glspl" "Gls" "Glspl"))
    (let* ((trm (replace-regexp-in-string
                 "^.+?:" ""
                 (org-element-property :path link)))
           (term-entry (org-glossary--find-term-entry terms trm :key))
           (index (org-glossary--record-term-usage term-entry link))
           (contents-begin (org-element-property :contents-begin link))
           (contents-end (org-element-property :contents-end link)))
      (org-element-put-property
       link :path (if no-number trm
                    (concat (number-to-string index) ":" trm)))
      (unless no-modify
        (replace-region-contents
         (org-element-property :begin link)
         (- (org-element-property :end link)
            (org-element-property :post-blank link))
         (lambda ()
           (org-element-link-interpreter
            link
            (and contents-begin contents-end
                 (buffer-substring contents-begin contents-end))))))
      term-entry)))

(defun org-glossary--update-plain (terms &optional no-modify no-number)
  "Register a reference to a term in TERMS, and convert to a link.
It is assumed that the term reference has just been matched with
a regexp of the form given by `org-glossary--construct-regexp'
and the match data is intact.

When NO-MODIFY is non-nil, the reference will be lodged in
TERMS but the buffer content left unmodified.
When NO-NUMBER is non-nil, no reference number shall be inserted."
  (let ((term-str
         (replace-regexp-in-string
          "[ \n\t]+" " "
          (substring-no-properties (match-string 0))))
        (plural-p (eq org-glossary--mrx-last-tag 'plural))
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
          term-entry
          (unless no-number
            (1+ (length (plist-get
                         (or (plist-get term-entry :alias-for) term-entry)
                         :uses))))
          plural-p capitalized-p)
         t t))
      (org-glossary--record-term-usage term-entry (org-element-context))
      term-entry)))

(defun org-glossary--find-term-entry (terms term-key key)
  "Find any term in TERMS where KEY is TERM-KEY."
  (cl-some (lambda (trm)
             (when (equal term-key (plist-get trm key))
               trm))
           terms))

(defun org-glossary--record-term-usage (term-entry record)
  "Record TERM-ENTRY's usage with RECORD, and give the use index."
  (let* ((canonical-term (or (plist-get term-entry :alias-for) term-entry))
         (uses (plist-get canonical-term :uses))
         (index (1+ (or (caar uses) 0))))
    ;; (plist-put canonical-term :uses (nconc (list (cons index record)) uses))
    (push (cons index record) (plist-get canonical-term :uses))
    index))

(defun org-glossary--clear-term-usage (term-entry)
  "Clear the :uses slot of TERM-ENTRY."
  (plist-put term-entry :uses nil))

(defun org-glossary--term-replacement (term-entry &optional index plural-p capitalized-p)
  "Construct a string refering to the TERM-ENTRY"
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

;;; Automatic term updating

(defun org-glossary--register-buffer-dependencies (&optional path-spec)
  "Watch all definition dependencies of PATH-SPEC for updates."
  (let ((path-spec (or path-spec (org-glossary--complete-path-spec))))
    (let* ((term-cache (cdr (assoc path-spec org-glossary--terms-cache)))
           (included (plist-get term-cache :included))
           (extras (mapcar #'org-glossary--parse-include-value
                           (plist-get term-cache :extra-term-sources))))
      (org-glossary--deregister-buffer-dependencies path-spec)
      (dolist (dep-pspec (delete path-spec (nconc extras included)))
        (if-let ((dep-files (assoc dep-pspec org-glossary--path-dependencies)))
            (unless (member path-spec dep-files)
              (push path-spec (cdr dep-files)))
          (push (list dep-pspec path-spec) org-glossary--path-dependencies))
        (org-glossary--register-buffer-dependencies dep-pspec)))))

(defun org-glossary--deregister-buffer-dependencies (&optional path-spec)
  "Stop watching definition dependencies for PATH-SPEC."
  (let ((path-spec (or path-spec (org-glossary--complete-path-spec))))
    (dolist (buf-dep org-glossary--path-dependencies)
      (setcdr buf-dep (delete path-spec (cdr buf-dep)))
      (unless (cdr buf-dep)
        (setq org-glossary--path-dependencies
              (delete buf-dep org-glossary--path-dependencies))
        (org-glossary--deregister-buffer-dependencies (car buf-dep))))))

(defun org-glossary--propagate-buffer-updates (path-spec)
  "Update all buffers whos definitions depend on PATH-SPEC.
The actual update is performed by `org-glossary--update-buffers'."
  (when-let ((dependants (cdr (assoc path-spec org-glossary--path-dependencies))))
    (unless org-glossary--path-update-timer
      (setq org-glossary--path-update-timer
            (run-with-idle-timer org-glossary-idle-update-period nil
                                 #'org-glossary--update-buffers)))
    (dolist (dep dependants)
      (add-to-list 'org-glossary--paths-to-update dep)
      (org-glossary--propagate-buffer-updates dep))))

(defun org-glossary--detect-updates-and-propagate (&optional path-spec)
  "Propagate any term definition updates originating from PATH-SPEC."
  (when org-glossary-idle-update-period
    (let* ((path-spec (or path-spec (org-glossary--complete-path-spec)))
           (cache-entry (cdr (assoc path-spec org-glossary--terms-cache)))
           (old-term-hash (plist-get cache-entry :terms-hash))
           (old-term-extras
            (sort (plist-get cache-entry :extra-term-sources) #'string<))
           new-term-hash new-term-extras)
      (org-glossary--get-terms-cached path-spec t nil)
      (setq cache-entry (cdr (assoc path-spec org-glossary--terms-cache))
            new-term-hash (plist-get cache-entry :terms-hash)
            new-term-extras (sort (plist-get cache-entry :extra-term-sources)
                                  #'string<))
      (unless (and (eq old-term-hash new-term-hash)
                   (equal old-term-extras new-term-extras))
        (when org-glossary-mode (org-glossary-update-terms))
        (org-glossary--propagate-buffer-updates path-spec)))))

(defun org-glossary--update-buffers ()
  "Update all live buffers in `org-glossary--paths-to-update'."
  (dolist (path-spec org-glossary--paths-to-update)
    (when-let ((buf (or (and (buffer-live-p path-spec) path-spec)
                        (get-file-buffer (plist-get path-spec :file)))))
      (with-current-buffer buf
        (when org-glossary-mode
          (org-glossary-update-terms)))))
  (when org-glossary--path-update-timer
    (cancel-timer org-glossary--path-update-timer)
    (setq org-glossary--path-update-timer nil))
  (setq org-glossary--paths-to-update nil))

;;; Export, general functionality

(defvar-local org-glossary--current-export-spec nil)

(defun org-glossary--get-export-specs (backend)
  "Determine the relevant export specs for BACKEND.
The information is extracted from `org-glossary-export-specs'."
  (let* ((default-spec (alist-get t org-glossary-export-specs))
         (current-spec
          (cl-loop for back = (if (symbolp backend)
                                  (org-export-get-backend backend)
                                backend)
                   then (org-export-get-backend
                         (org-export-backend-parent
                          back))
                   unless back return default-spec
                   for spec = (alist-get (org-export-backend-name back)
                                         org-glossary-export-specs)
                   when spec return spec))
         (default-template
           (org-combine-plists (alist-get t default-spec)
                               (alist-get t current-spec)))
         (complete-template
          (lambda (type)
            (let ((template
                   (org-combine-plists
                    default-template
                    (alist-get type default-spec)
                    (alist-get type current-spec))))
              (cons type template)))))
    (cons (cons t default-template)
          (mapcar complete-template
                  (mapcar #'cdr org-glossary-headings)))))

(defun org-glossary-set-export-spec (backend type &rest property-value-pairs)
  "For the TYPE plist for BACKEND's export spec, set PROPERTY-VALUE-PAIRS.
Specifically, for each :PROPERTY VALUE pair of PROPERTY-VALUE-PAIRS, that
PROPERTY is set to VALUE within the TYPE list of the BACKEND list in
`org-glossary-export-specs'."
  (declare (indent 2))
  (while property-value-pairs
    (setf (alist-get type (alist-get backend org-glossary-export-specs))
          (plist-put (alist-get type (alist-get backend org-glossary-export-specs))
                     (pop property-value-pairs) (pop property-value-pairs)))))

(defun org-glossary--export-instance (backend info term-entry form &optional ref-index plural-p capitalized-p extra-parameters)
  "Export the FORM of TERM-ENTRY according to `org-glossary--current-export-spec'.
All other argments (BACKEND, INFO, FORM, REF-INDEX, PLURAL-P,
CAPITALIZED-P, EXTRA-PARAMETERS) are simply passed onto the
relevant template."
  (let ((template
         (plist-get (alist-get
                     (plist-get term-entry :type)
                     org-glossary--current-export-spec)
                    form)))
    (cond
     ((stringp template)
      (org-glossary--export-template
       template backend info term-entry
       ref-index plural-p capitalized-p extra-parameters))
     ((functionp template)
      (funcall template backend info term-entry form
               ref-index plural-p capitalized-p extra-parameters))
     ((not template) "")
     (t "ORG-GLOSSARY-EXPORT-INVALID-SPEC"))))

(defun org-glossary--export-template (template backend info term-entry &optional ref-index plural-p capitalized-p extra-parameters)
  "Fill out TEMPLATE using BACKEND, INFO, and TERM-ENTRY.
The fields availible to the template are further affected by the
optional arguments:
 - REF-INDEX provides %r
 - PLURAL-P and CAPITALIZED-P affect %t and %v
 - EXTRA-PARAMETERS defines additional fields"
  (let ((parameters (reverse extra-parameters))
        (canonical-term (or (plist-get term-entry :alias-for) term-entry))
        case-fold-search)
    (when org-glossary-canonicalise-aliases
      (setq term-entry canonical-term))
    (when (string-match-p "%k" template)
      (push (cons ?k (plist-get canonical-term :key)) parameters))
    (when (string-match-p "%K" template)
      (push (cons ?K (number-to-string (plist-get canonical-term :key-nonce)))
            parameters))
    (when (string-match-p "%t" template)
      (push (cons ?t (funcall (if capitalized-p #'org-glossary--sentance-case
                                #'identity)
                              (plist-get term-entry
                                         (if plural-p :term-plural :term))))
            parameters))
    (when (string-match-p "%l" template)
      (push (cons ?l (string (downcase (aref (plist-get term-entry :term) 0))))
            parameters))
    (when (string-match-p "%L" template)
      (push (cons ?L (string (upcase (aref (plist-get term-entry :term) 0))))
            parameters))
    (when (and (not (memq ?v (mapcar #'car extra-parameters)))
               (string-match-p "%v" template))
      (push (cons ?v
                  (let ((value-str
                         (org-glossary--export-term canonical-term info)))
                    (funcall (if capitalized-p #'org-glossary--sentance-case
                               #'identity)
                             (if plural-p
                                 (let ((components (split-string value-str)))
                                   (setf (car (last components))
                                         (funcall org-glossary-plural-function
                                                  (car (last components))))
                                   (mapconcat #'identity components " "))
                               value-str))))
            parameters))
    (when (and ref-index (string-match-p "%r" template))
      (push (cons ?r (number-to-string ref-index))
            parameters))
    (when (string-match-p "%n" template)
      (push (cons ?n (number-to-string
                      (length (plist-get term-entry :uses))))
            parameters))
    (when (string-match-p "%c" template)
      (push (cons ?c (plist-get canonical-term :category))
            parameters))
    (when (string-match-p "%u" template)
      (push (cons ?u (org-glossary--export-instance
                      backend info term-entry :use
                      ref-index plural-p capitalized-p
                      extra-parameters))
            parameters))
    (format-spec template (nreverse parameters))))

(defun org-glossary--export-term (term-entry info)
  "When TERM-ENTRY's :value is non-nil, return the exported value using INFO.
If the :value has not been exported before, `org-export-filter-apply-functions'
is applied to the :value first, and the result is cached.

Should :value consist of a single paragraph, its contents are
exported in place of the paragraph itself."
  (when-let ((value (plist-get term-entry :value)))
    (or (gethash value (plist-get info :exported-data))
        (puthash value
                 (let ((filtered-value
                        (org-export-filter-apply-functions
                         (plist-get info :filter-parse-tree)
                         value info)))
                   (org-export-data
                    (if (and (= (length filtered-value) 1)
                             (eq (org-element-type (car filtered-value))
                                 'paragraph))
                        (mapcar #'org-element-extract-element
                                (org-element-contents (car filtered-value)))
                      filtered-value)
                    info))
                 (plist-get info :exported-data)))))

(defun org-glossary--sentance-case (s)
  "Return a sentance-cased version of S."
  (concat (string (upcase (aref s 0))) (substring s 1)))

;;; Export used term definitions

(defun org-glossary--print-terms (backend terms &optional types level duplicate-mentions)
  "Produce an org-mode AST defining TERMS for BACKEND.
Do this for each of TYPES (by default, the :type specified in
`org-glossary-default-print-parameters'),producing a heading of level
LEVEL (by default: 1). If LEVEL is set to 0, no heading is produced.

Unless duplicate-mentions is non-nil, terms already defined will be excluded."
  (let ((terms-by-type
         (org-glossary--group-terms
          (org-glossary--sort-plist
           (cl-remove-if
            (lambda (trm)
              (or (and (not (plist-get trm :uses)) ; Occurs when `trm' is an alias.
                       org-glossary-canonicalise-aliases)
                  (and (not duplicate-mentions)
                       (plist-get trm :extracted))))
            terms)
           :key #'org-glossary--string>)
          (lambda (trm) (plist-get trm :type))
          (or types (plist-get org-glossary-default-print-parameters :type))))
        (level (or level 1))
        export-spec content)
    (mapconcat
     (lambda (type-terms)
       (setq export-spec (alist-get (car type-terms) org-glossary--current-export-spec)
             content (org-glossary--print-terms-by-category
                      backend (car type-terms) (cdr type-terms) level))
       (if (and (not (string-empty-p (plist-get export-spec :heading)))
                (> level 0))
           (concat
            (and (string-match-p "^\\* " (plist-get export-spec :heading))
                 (make-string (1- level) ?*))
            (plist-get export-spec :heading)
            "\n"
            content)
         content))
     terms-by-type
     "\n")))

(defun org-glossary--print-terms-by-category (backend type terms level)
  "Produce a string printing TERMS for TYPE in BACKEND split by category."
  (let ((terms-by-category
         (org-glossary--group-terms
          (org-glossary--sort-plist terms :key #'org-glossary--string>)
          (lambda (trm) (plist-get trm :category))))
        (export-spec (alist-get type org-glossary--current-export-spec))
        content cat-heading)
    (if (= (length terms-by-category) 1)
        (org-glossary--print-terms-by-letter
         backend type terms (+ level (if (caar terms-by-category) 1 0)))
      (mapconcat
       (lambda (cat-terms)
         (setq content (org-glossary--print-terms-by-letter
                        backend type (cdr cat-terms)
                        (+ level (if (cl-some #'car terms-by-category) 1 0)))
               cat-heading (org-glossary--export-instance
                            backend nil (cadr cat-terms) :category-heading))
         (if (not (string-empty-p (plist-get export-spec :category-heading)))
             (concat
              (if (string-match-p "^\\* " cat-heading)
                  (concat (make-string level ?*) cat-heading)
                cat-heading)
              "\n"
              content)
           content))
       terms-by-category
       "\n"))))

(defun org-glossary--print-terms-by-letter (backend type terms level)
  "Produce an org-mode AST for TYPE in BACKEND defining ASSEMBLED-TERMS."
  (let* ((terms-by-letter
          (org-glossary--group-terms
           (org-glossary--sort-plist terms :key #'org-glossary--string>)
           (lambda (trm) (upcase (aref (plist-get trm :key) 0)))))
         (num-terms-by-letter (mapcar (lambda (trms) (length (cdr trms)))
                                      terms-by-letter))
         (export-spec (alist-get type org-glossary--current-export-spec))
         (use-letters-p
          (and (>= (apply #'+ num-terms-by-letter)
                   (car org-glossary-print-letter-minimums))
               (>= (apply #'max num-terms-by-letter)
                   (cdr org-glossary-print-letter-minimums))
               (not (string-empty-p (plist-get export-spec :letter-heading))))))
    (concat
     (and (not use-letters-p)
          (not (string-empty-p (plist-get export-spec :definition-structure-preamble)))
          (concat (plist-get export-spec :definition-structure-preamble) "\n"))
     (mapconcat
      (lambda (letter-terms)
        (let ((letter-heading
               (if use-letters-p
                   (org-glossary--export-instance
                    backend nil (cadr letter-terms) :letter-heading)
                 "")))
          (concat
           (and (not (string-empty-p letter-heading))
                (concat
                 (if (string-match-p "^\\* " letter-heading)
                     (concat (make-string level ?*) letter-heading)
                   letter-heading)
                 "\n"
                 (and (not (string= "" (plist-get export-spec :definition-structure-preamble)))
                      (concat (plist-get export-spec :definition-structure-preamble) "\n"))))
           (mapconcat
            (lambda (term-entry)
              (org-glossary--print-terms-singular backend term-entry))
            (cdr letter-terms)
            "\n"))))
      terms-by-letter
      "\n"))))

(defun org-glossary--print-terms-singular (backend term-entry)
  (org-glossary--export-instance
   backend nil term-entry :definition-structure
   nil nil nil
   (let ((key (plist-get term-entry :key)))
     (if (plist-get term-entry :alias-for)
         `((?d . ,(format "[[glsdef:%s]]" key))
           (?v . ,(org-glossary--export-instance
                   backend nil (plist-get term-entry :alias-for)
                   :alias-value 0))
           (?b . ,""))
       `((?d . ,(format "[[glsdef:%s]]" key))
         (?v . ,(string-trim (org-element-interpret-data
                              (plist-get term-entry :value))))
         (?b . ,(mapconcat
                 (lambda (use)
                   (format "[[glsuse:%d:%s]]" (car use) key))
                 (cl-sort
                  (plist-get term-entry :uses)
                  #'< :key #'car)
                 (org-glossary--export-instance
                  backend nil term-entry :backref-seperator))))))))

(defun org-glossary--group-terms (terms predicate &optional include)
  "Group TERMS according to PREDICATE, and optionaly only INCLUDE certain groups."
  (let (groups grp)
    (dolist (trm terms)
      (setq grp (funcall predicate trm))
      (push trm
            (cdr (or (assoc grp groups)
                     (car (push (cons grp nil)
                                groups))))))
    (mapcar
     (lambda (group-terms)
       (cons (car group-terms)
             (nreverse (cdr group-terms))))
     (if include
         (cl-remove-if
          (lambda (group-terms)
            (and include
                 (not (member (car group-terms)
                              include))))
          groups)
       groups))))

(defun org-glossary--sort-plist (plist key predicate)
  "Sort PLIST by KEY according to PREDICATE."
  (sort plist
        (lambda (a b)
          (funcall predicate
                   (plist-get a key)
                   (plist-get b key)))))

(defun org-glossary--string> (a b)
  "Check if A > B using collation order, ignoring case."
  (and (not (string= a b))
       (not (string-collate-lessp a b nil t))))

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

(defun org-glossary--extract-uses-in-region (terms begin end &optional types mark-extracted)
  "Extract uses of TERMS that occur between BEGIN and END.
If TYPES is non-nil, extracted entries shall be restricted instances of TYPES.
If MARK-EXTRACTED is non-nil, extracted uses shall be marked as extracted."
  (let (region-terms region-term-uses)
    (mapc
     (lambda (term-entry)
       (when (or (not types)
                 (memq (plist-get term-entry :type) types))
         (setq region-term-uses nil)
         (dolist (use (plist-get term-entry :uses))
           (when (and (<= begin
                          (org-element-property :begin (cdr use))
                          (org-element-property :end (cdr use))
                          end)
                      (push use region-term-uses)
                      mark-extracted)
             (plist-put (cdr use) :extracted t)))
         (when region-term-uses
           (push (org-combine-plists
                  term-entry (list :uses region-term-uses))
                 region-terms))))
     terms)
    (nreverse region-terms)))

(defun org-glossary--expand-print (backend terms &optional parameters)
  "Generate string defining TERMS in BACKEND, according to PARAMETERS."
  (org-glossary--print-terms
   backend terms
   (plist-get parameters :types)
   (if (plist-get parameters :only-contents)
       0
     (1+ (plist-get parameters :level)))
   (plist-get parameters :all)))

(defun org-glossary--expand-print-keyword (backend terms keyword)
  "Call `org-glossary--expand-print' with paramaters and terms based on KEYWORD.
BACKEND is passed through unmodified, but TERMS may be modified depending on
the :consume parameter extracted from KEYWORD."
  (let ((heading (org-element-lineage keyword '(headline org-data)))
        (parameters (org-combine-plists
                     org-glossary-default-print-parameters
                     (org-glossary--parse-print-keyword-value
                      (org-element-property :value keyword)))))
    (while (and (not (eq (org-element-type heading) 'org-data))
                (> (org-element-property :level heading)
                   (plist-get parameters :level)))
      (setq heading (org-element-lineage heading '(headline org-data))))
    (org-glossary--expand-print
     backend
     (org-glossary--extract-uses-in-region
      terms
      (org-element-property :begin heading)
      (org-element-property :end heading)
      (plist-get parameters :type)
      (plist-get parameters :consume))
     parameters)))

(defun org-glossary--parse-print-keyword-value (value)
  "Parse the string VALUE to a parameter plist."
  (let ((res '()))
    (dolist (pair (org-babel-parse-header-arguments value))
      (push (car pair) res)
      (push
       (pcase (car pair)
         (:type (mapcar #'intern (split-string (cdr pair))))
         ((or :consume :only-contents :all)
          (and (stringp (cdr pair))
               (or (string= "t" (cdr pair))
                   (string= "yes" (cdr pair)))))
         (_ (cdr pair)))
       res))
    (nreverse res)))

;;; Link definitions

(org-link-set-parameters "gls"
                         :export #'org-glossary--link-export-gls
                         :face 'org-glossary-term
                         :follow #'org-glossary-goto-term-definition
                         :help-echo #'org-glossary--help-echo-from-textprop)
(org-link-set-parameters "glspl"
                         :export #'org-glossary--link-export-glspl
                         :face 'org-glossary-term
                         :follow #'org-glossary-goto-term-definition
                         :help-echo #'org-glossary--help-echo-from-textprop)
(org-link-set-parameters "Gls"
                         :export #'org-glossary--link-export-Gls
                         :face 'org-glossary-term
                         :follow #'org-glossary-goto-term-definition
                         :help-echo #'org-glossary--help-echo-from-textprop)
(org-link-set-parameters "Glspl"
                         :export #'org-glossary--link-export-Glspl
                         :face 'org-glossary-term
                         :follow #'org-glossary-goto-term-definition
                         :help-echo #'org-glossary--help-echo-from-textprop)

(defun org-glossary--link-export-gls (index-term description backend info)
  "Export a gls link to term index-term with BACKEND."
  (org-glossary--link-export backend info index-term description nil nil))

(defun org-glossary--link-export-glspl (index-term description backend info)
  "Export a glspl link to term index-term with BACKEND."
  (org-glossary--link-export backend info index-term description t nil))

(defun org-glossary--link-export-Gls (index-term description backend info)
  "Export a Gls link to term index-term with BACKEND."
  (org-glossary--link-export backend info index-term description nil t))

(defun org-glossary--link-export-Glspl (index-term description backend info)
  "Export a Glspl link to term index-term with BACKEND."
  (org-glossary--link-export backend info index-term description t t))

(defconst org-glossary--index-stub-description
  "<org-glossary-index-stub>"
  "Usage description value that should be treated as an invisible reference.")

(defun org-glossary--link-export (backend info index-term description &optional plural-p capitalized-p)
  "Export a link to TERM with BACKEND, respecting PLURAL-P and CAPITALIZED-P."
  (if-let ((index (if (seq-contains-p index-term ?:)
                      (string-to-number (car (split-string index-term ":")))
                    1))
           (trm (replace-regexp-in-string "^.+?:" "" index-term))
           (term-entry (org-glossary--quicklookup trm)))
      (org-glossary--export-instance
       backend info term-entry (if (= 1 index) :first-use :use)
       index plural-p capitalized-p
       (and (stringp description)
            `((?t . ,(if (string= description org-glossary--index-stub-description)
                         "" description)))))
    (funcall (if capitalized-p #'org-glossary--sentance-case #'identity)
             (funcall (if plural-p org-glossary-plural-function #'identity)
                      trm))))

(org-link-set-parameters "glsdef"
                         :export #'org-glossary--link-export-glsdef
                         :face 'org-glossary-term)
(org-link-set-parameters "glsuse"
                         :export #'org-glossary--link-export-glsuse
                         :face 'org-glossary-term)

(defun org-glossary--link-export-glsdef (key _ backend info)
  (if-let ((term-entry (org-glossary--quicklookup key)))
      (let ((capitalised-term
             (concat (upcase (substring (plist-get term-entry :term) 0 1))
                     (substring (plist-get term-entry :term) 1))))
        (org-glossary--export-instance
         backend info term-entry :definition nil nil nil
         `((?t . ,capitalised-term)
           (?k . ,(plist-get term-entry :key))
           (?K . ,(number-to-string (plist-get term-entry :key-nonce))))))
    key))

(defun org-glossary--link-export-glsuse (index-term _desc backend info)
  (if-let ((index (if (seq-contains-p index-term ?:)
                      (string-to-number (car (split-string index-term ":")))
                    1))
           (trm (replace-regexp-in-string "^.+?:" "" index-term))
           (term-entry (org-glossary--quicklookup trm)))
      (org-glossary--export-instance backend info term-entry :backref index)
    index-term))

;;; Pluralisation

(defcustom org-glossary-english-plural-exceptions nil
  "An alist of (lowercase) words and their plural forms.
For inspiration, see https://github.com/RosaeNLG/rosaenlg/blob/master/packages/english-plurals-list/resources/noun.exc."
  :type '(alist :key-type (string :tag "singular")
                :value-type (string :tag "plural")))

(defun org-glossary-english-plural (word)
  "Generate the plural form of WORD."
  (or (let ((plural
             (alist-get (if (string-match-p "^[[:upper]]+$" word)
                            (downcase word) word)
                        org-glossary-english-plural-exceptions
                        nil nil #'string=))
            case-fold-search)
        (when plural
          (cond
           ((string-match-p "^[[:lower:]]+$" word) plural)
           ((string-match-p "^[[:upper:]][[:lower:]]+$" word)
            (capitalize plural))
           ((string-match-p "^[[:upper:]]+$" word) (upcase plural)))))
      ;; Source: https://github.com/plurals/pluralize/blob/master/pluralize.js#L334
      (cond
       ((let (case-fold-search) ; Acronyms shouldn't be treated as words.
          (string-match-p "^[[:upper:]]+$" word))
        (concat word "s"))
       ((string-match "m[ae]n$" word)
        (replace-match "men" nil t word))
       ((string-match-p "eaux$" word) word)
       ((string-match "\\(child\\)\\(?:ren\\)?$" word)
        (replace-match "\\1ren" nil nil word))
       ((string-match "pe\\(?:rson\\|ople\\)$" word)
        (replace-match "people" nil t word))
       ((string-match "\\b\\(\\(?:tit\\)?m\\|l\\)\\(?:ice\\|ouse\\)$" word)
        (replace-match "\\1ice" nil nil word))
       ((string-match "\\(matr\\|cod\\|mur\\|sil\\|vert\\|ind\\|append\\)\\(?:ix\\|ex\\)$" word)
        (replace-match "\\1ices" nil nil word))
       ((string-match "\\(x\\|ch\\|ss\\|sh\\|zz\\)$" word)
        (replace-match "\\1es" nil nil word))
       ((string-match "\\([^ch][ieo][ln]\\)ey$" word)
        (replace-match "\\1es" nil nil word))
       ((string-match "\\([^aeiou]\\|qu\\)y$" word)
        (replace-match "\\1ies" nil nil word))
       ((string-match "\\(?:\\(kni\\|wi\\|li\\)fe\\|\\(ar\\|l\\|ea\\|eo\\|oa\\|hoo\\)f\\)$" word)
        (replace-match "\\1\\2ves" nil nil word))
       ((string-match "sis$" word)
        (replace-match "ses" nil nil word))
       ((string-match "\\(apheli\\|hyperbat\\|periheli\\|asyndet\\|noumen\\|phenomen\\|criteri\\|organ\\|prolegomen\\|hedr\\|automat\\)\\(?:a\\|on\\)$" word)
        (replace-match "\\1a" nil nil word))
       ((string-match "\\(agend\\|addend\\|millenni\\|dat\\|extrem\\|bacteri\\|desiderat\\|strat\\|candelabr\\|errat\\|ov\\|symposi\\|curricul\\|automat\\|quor\\)\\(?:a\\|um\\)$" word)
        (replace-match "\\1a" nil nil word))
       ((string-match "\\(her\\|at\\|gr\\)o$" word)
        (replace-match "\\1oes" nil nil word))
       ((string-match "\\(seraph\\|cherub\\)\\(?:im\\)?$" word)
        (replace-match "\\1im" nil nil word))
       ((string-match "\\(alumn\\|alg\\|vertebr\\)\\(?:a\\|ae\\)$" word)
        (replace-match "\\1ae" nil nil word))
       ((string-match "\\(alumn\\|syllab\\|vir\\|radi\\|nucle\\|fung\\|cact\\|stimul\\|termin\\|bacill\\|foc\\|uter\\|loc\\|strat\\)\\(?:us\\|i\\)$" word)
        (replace-match "\\1i" nil nil word))
       ((string-match-p "\\([^l]ias\\|[aeiou]las\\|[ejzr]as\\|[iu]am\\)$" word) word)
       ((string-match "\\(e[mn]u\\)s?$" word)
        (replace-match "\\1s" nil nil word))
       ((string-match "\\(i\\|l\\)um$" word) ; added
        (replace-match "\\1a" nil nil word))
       ((string-match "\\(alias\\|[^aou]us\\|t[lm]as\\|gas\\|ris\\)$" word)
        (replace-match "\\1es" nil nil word))
       ((string-match "\\(ax\\|test\\)is$" word)
        (replace-match "\\1es" nil nil word))
       ((string-match "enon$" word)
        (replace-match "ena" nil nil word))
       ((string-match-p "\\([^aeiou]ese\\)$" word) word)
       (t (concat word "s")))))

;;; Export

(defun org-glossary--prepare-buffer (&optional backend)
  "Modify the buffer to resolve all defined terms, prepearing it for export.
This should only be run as an export hook."
  (setq org-glossary--terms (org-glossary--get-terms-cached)
        org-glossary--current-export-spec
        (org-glossary--get-export-specs backend))
  (org-glossary--strip-headings nil nil nil t)
  (let ((index-terms-mrx (org-glossary--mrx-construct-from-terms
                          (cl-remove-if
                           (lambda (trm)
                             (not (eq (plist-get trm :type) 'index)))
                           org-glossary--terms))))
    (save-excursion
      (goto-char (point-min))
      (while (org-glossary--mrx-search-forward index-terms-mrx)
        (when (save-match-data
                (looking-back "^[ \t]*#\\+[cfkptv]?index:[ \t]*"
                              (line-beginning-position)))
          (replace-match
           (format "[[gls:\\1][%s]]"
                   org-glossary--index-stub-description))))))
  (let ((used-terms (org-glossary-apply-terms org-glossary--terms))
        keyword print-glossary-p)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*#\\+print_glossary:" nil t)
        (setq print-glossary-p t
              keyword (org-element-context))
        (delete-region (org-element-property :begin keyword)
                       (- (org-element-property :end keyword)
                          (org-element-property :post-blank keyword)
                          1))
        (insert (org-glossary--expand-print-keyword backend used-terms keyword)))
      (unless print-glossary-p
        (goto-char (point-max))
        (insert "\n" (org-glossary--print-terms backend used-terms))))))

(add-hook 'org-export-before-parsing-functions #'org-glossary--prepare-buffer)

;;; Fontification

(defvar-local org-glossary--term-mrx nil
  "A multi-rx matching all known forms of terms.")

(defvar-local org-glossary--font-lock-keywords
  '((org-glossary--fontify-find-next
     (0 (org-glossary--fontify-term))))
  "`font-lock-keywords' entry that fontifies term references.")

(defun org-glossary--set-font-lock-keywords (&optional per-term-p)
  "Set `org-glossary--font-lock-keywords' according to PER-TERM-P."
  (setq org-glossary--font-lock-keywords
        (if per-term-p
            '((org-glossary--fontify-find-next
               (0 (org-glossary--fontify-term))))
          '((org-glossary--fontify-find-next
             (0 '(face org-glossary-term
                  help-echo org-glossary--help-echo-from-textprop
                  mouse-face (:inverse-video t)
                  keymap (keymap
                          (follow-link . mouse-face)
                          (mouse-2 . org-glossary-goto-term-definition)
                          ("RET" . org-glossary-goto-term-definition)
                          (return . org-glossary-goto-term-definition)))
                t)))))
  per-term-p)

(define-minor-mode org-glossary-mode
  "Glossary term fontification, and enhanced interaction."
  :global nil
  :group 'org-glossary
  (cond
   ((and org-glossary-mode org-glossary-automatic)
    (org-glossary--set-font-lock-keywords org-glossary-fontify-types-differently)
    (font-lock-add-keywords nil org-glossary--font-lock-keywords 'append))
   (t (font-lock-remove-keywords nil org-glossary--font-lock-keywords)
      (org-with-wide-buffer (font-lock-flush))))
  (if org-glossary-mode
      (progn
        (org-glossary-update-terms)
        (org-glossary--register-buffer-dependencies)
        (add-hook 'after-save-hook
                  'org-glossary--detect-updates-and-propagate nil t)
        (add-hook 'kill-buffer-hook
                  'org-glossary--deregister-buffer-dependencies nil t))
    (org-glossary--deregister-buffer-dependencies)
    (remove-hook 'kill-buffer-hook
                 'org-glossary--deregister-buffer-dependencies t))
  org-glossary-mode)

(defun org-glossary--fontify-find-next (&optional limit)
  "Find any next occurance of a term reference, for fontification."
  (let ((search-spaces-regexp (and font-lock-multiline "[ \t\n][ \t]*"))
        match-p exit element-at-point element-context)
    (while (and (not exit) (if limit (< (point) limit) t))
      (setq exit (null (org-glossary--mrx-search-forward
                        org-glossary--term-mrx limit)))
      (save-match-data
        (setq element-at-point (org-element-at-point)
              element-context (org-element-context element-at-point))
        (when (and (not exit)
                   (not (eq 'headline (org-element-type element-at-point)))
                   (memq 'link (org-element-restriction element-context))
                   (if (eq 'keyword (org-element-type element-at-point))
                       (member (org-element-property :key element-at-point)
                               org-element-parsed-keywords)
                     t)
                   (not (org-glossary--within-definition-p element-context)))
          ;; HACK For some strange reason, if I don't move point forwards
          ;; here, this function will end up being called again and again
          ;; ad-infinitum.  Strangely, while (forward-char 1) works
          ;; (goto-char (match-end 0)) does not.  What on earth is happening?
          ;; Please send help.
          (forward-char 1)
          (setq exit t match-p t))))
    match-p))

(defvar-local org-glossary--fontified-snippet-cache
  (make-hash-table :test #'equal))

(defun org-glossary--fontify-org-snippet (org-text)
  "Fontify the string ORG-TEXT using `org-mode'."
  (or (gethash org-text org-glossary--fontified-snippet-cache)
      (puthash org-text
               (with-temp-buffer
                 (insert org-text)
                 (let (org-mode-hook org-glossary-snippet-fontication-hooks)
                   (org-mode))
                 (font-lock-ensure)
                 (buffer-string))
               org-glossary--fontified-snippet-cache)))

(defun org-glossary--fontify-term ()
  "Fontify the matched term."
  (let ((term-entry (org-glossary--quicklookup (match-string 0)))
        case-fold-search)
    (add-text-properties
     (match-beginning 0) (match-end 0)
     (nconc
      (pcase (plist-get term-entry :type)
        ('substitution
         (if org-glossary-display-substitute-value
             `(face org-glossary-substituted-value
               help-echo org-glossary--help-echo-from-textprop
               mouse-face org-glossary-substitution-term
               display
               ,(funcall
                 (if org-glossary-fontify-displayed-substitute
                     #'org-glossary--fontify-org-snippet #'identity)
                 (funcall
                  (if (string-match-p "^[[:upper:]][^[:upper:]]+$"
                                      (match-string 0))
                      #'org-glossary--sentance-case #'identity)
                  (string-trim
                   (substring-no-properties
                    (org-element-interpret-data
                     (plist-get term-entry :value)))))))
           '(face org-glossary-substitution-term)))
        (type `(face ,(or (alist-get type org-glossary-fontify-type-faces)
                          'org-glossary-term))))
      `(help-echo
        org-glossary--help-echo-from-textprop
        mouse-face (:inverse-video t)
        keymap (keymap
                (follow-link . mouse-face)
                (mouse-2 . org-glossary-goto-term-definition)
                ("RET" . org-glossary-goto-term-definition)
                (return . org-glossary-goto-term-definition)))))))

(defvar-local org-glossary--help-echo-cache (make-hash-table :test #'equal)
  "A hash table for quickly looking up fontified help-echo strings.")

(defun org-glossary--term-help-echo-str (term-entry)
  (with-temp-buffer
    (let ((org-inhibit-startup t)
          org-mode-hook)
      (insert
       (string-trim
        (org-element-interpret-data
         (plist-get term-entry :value))))
      (when (looking-back "^[ \t]*#\\+end" (line-beginning-position))
        (insert "\n"))
      (org-do-remove-indentation)
      (org-mode)
      (font-lock-ensure)
      (replace-regexp-in-string
       org-link-any-re "\\3" (buffer-string)))))

(defun org-glossary--term-help-echo-str-cached (term-entry)
  (or (gethash term-entry org-glossary--help-echo-cache)
      (puthash term-entry (org-glossary--term-help-echo-str term-entry)
               org-glossary--help-echo-cache)))

(defun org-glossary--term-help-echo (term-entry &optional no-squash)
  "Generate a help-echo string for TERM-ENTRY."
  (let* ((referenced-term
          (or (plist-get term-entry :alias-for)
              (org-glossary--quicklookup
               (string-trim (substring-no-properties
                             (org-element-interpret-data
                              (plist-get term-entry :value)))))))
         (display-text
          (org-glossary--term-help-echo-str-cached
           (or referenced-term term-entry))))
    (format "(%s) %s %s"
            (propertize
             (symbol-name (plist-get (or referenced-term term-entry) :type))
             'face 'org-table)
            (concat
             (propertize
              (plist-get term-entry :term)
              'face (if referenced-term 'font-lock-doc-face 'org-list-dt))
             (and referenced-term
                  (concat
                   " ⟶ "
                   (propertize
                    (plist-get referenced-term :term)
                    'face 'org-list-dt))))
            (if no-squash
                display-text
              (replace-regexp-in-string
               "\s?\n\s*" " " ; flatten newline indentation
               display-text)))))

(defun org-glossary--help-echo-from-textprop (_window object pos &optional no-squash)
  "Find the term reference at POS in OBJECT, and get the definition."
  (let ((term-entry
         (org-glossary--quicklookup
          (with-current-buffer object
            (replace-regexp-in-string
             "^[Gg]ls\\(?:pl\\)?:" ""
             (buffer-substring-no-properties
              (or (previous-single-property-change (1+ pos) 'face) (point-min))
              (or (next-single-property-change pos 'face) (point-max))))))))
    (and term-entry (org-glossary--term-help-echo term-entry no-squash))))

;;; Completion

;; The sole purpose of `org-options-keywords' is to supply candidates
;; in org-pcomplete.el, so we may as well add our new keywords to the list.

(add-to-list 'org-options-keywords "GLOSSARY_SOURCES:")
(add-to-list 'org-options-keywords "PRINT_GLOSSARY:")

;;; Interaction

(defvar-local org-glossary--quicklookup-cache (make-hash-table :test #'equal)
  "A hash table for quickly looking up a term-entry from a reference form.")

(defun org-glossary--quicklookup (term-str)
  "Find the term entry reffered to by TERM-STR."
  (or (gethash term-str org-glossary--quicklookup-cache)
      (and (not (string-empty-p term-str))
           (let ((term-entry
                  (or (org-glossary--find-term-entry
                       org-glossary--terms term-str :key)
                      (org-glossary--find-term-entry
                       org-glossary--terms term-str :key-plural)
                      (org-glossary--find-term-entry
                       org-glossary--terms
                       (concat (string (downcase (aref term-str 0)))
                               (substring term-str 1))
                       :key)
                      (org-glossary--find-term-entry
                       org-glossary--terms
                       (concat (string (downcase (aref term-str 0)))
                               (substring term-str 1))
                       :key-plural))))
             (puthash term-str term-entry
                      org-glossary--quicklookup-cache)))))

(defun org-glossary-update-terms (&optional show-info)
  "Update the currently known terms."
  (interactive "p")
  (unless (derived-mode-p 'org-mode)
    (user-error "You need to be using `org-mode' to use org-glossary."))
  (let ((initial-terms (mapcar (lambda (trm) (plist-get trm :term))
                               org-glossary--terms)))
    (setq org-glossary--extra-term-sources (org-glossary--get-extra-term-sources)
          org-glossary--terms (org-glossary--get-terms-cached)
          org-glossary--term-mrx
          (org-glossary--mrx-construct-from-terms org-glossary--terms)
          org-glossary--quicklookup-cache (make-hash-table :test #'equal)
          org-glossary--help-echo-cache (make-hash-table :test #'equal))
    (when show-info
      (org-glossary--term-status-message
       (mapcar (lambda (trm) (plist-get trm :term))
               org-glossary--terms)
       initial-terms)))
  (when org-glossary-mode
    (org-with-wide-buffer
     (font-lock-flush))))

(defun org-glossary--get-extra-term-sources (&optional parse-tree)
  "Identify all applicable sources of extra terms for the current buffer.
This combines locations listed in `org-glossary-global-terms' with
local sources specified with \"#+glossary_sources: LOCATIONS\".

LOCATIONS is interpreted as space-seperated path specification
components which are prefixed by `org-glossary-collection-root'.
Path spec components including spaces can be given by enclosing
the location in double quotes. Should a file exist at a resolved
location with the extension .org, that file will be used as the
location."
  (append
   org-glossary-global-terms
   (mapcar
    (lambda (source-short)
      (let ((fq-source (concat org-glossary-collection-root source-short)))
        (cond
         ((file-exists-p (concat fq-source ".org"))
          (concat fq-source ".org"))
         ((string-match-p "\\.org::[*#]." fq-source)
          (concat fq-source " :only-contents t"))
         (t fq-source))))
    (org-babel-balanced-split
     (or (mapconcat
          #'identity
          (org-element-map (or parse-tree (org-element-parse-buffer)) 'keyword
            (lambda (keyword)
              (and (equal "GLOSSARY_SOURCES" (org-element-property :key keyword))
                   (org-element-property :value keyword))))
          " ")
         "")
     ?\s))))

(defun org-glossary--term-status-message (current-terms &optional initial-terms)
  "Emit a status mesage, based on CURRENT-TERMS and INITIAL-TERMS."
  (let ((added-terms (cl-set-difference current-terms initial-terms :test #'string=))
        (removed-terms (cl-set-difference initial-terms current-terms :test #'string=))
        (n-sources (length (cl-delete-duplicates
                            (mapcar
                             (lambda (term-entry)
                               (plist-get term-entry :definition-file))
                             org-glossary--terms)))))
    (message "%s"
             (concat
              (propertize "org-glossary" 'face 'bold)
              ": "
              (propertize (number-to-string (length current-terms))
                          'face 'warning)
              " registered term" (and (> (length current-terms) 1) "s")
              " from "
              (propertize (number-to-string n-sources)
                          'face 'bold)
              " source" (and (> n-sources 1) "s")
              (and (or added-terms removed-terms) ", ")
              (and added-terms
                   (format "%s term%s added"
                           (propertize (number-to-string (length added-terms))
                                       'face 'success)
                           (if (> (length added-terms) 1) "s" "")))
              (and added-terms removed-terms ", ")
              (and removed-terms
                   (format "%s term%s removed"
                           (propertize (number-to-string (length removed-terms))
                                       'face 'error)
                           (if (> (length removed-terms) 1) "s" "")))))))

(defun org-glossary--select-term (terms)
  "Select a term entry from TERMS."
  (let* ((term-text (mapcar #'org-glossary--select-term-candidatify terms))
         (choice
          (completing-read
           "Term: "
           (lambda (string predicate action)
             (if (eq action 'metadata)
                 '(metadata
                   (annotation-function . org-glossary--select-term-annotation)
                   (group-function . org-glossary--select-term-group)
                   (category . glossary-entry))
               (complete-with-action action term-text string predicate))))))
    (org-glossary--find-term-entry
     terms (car (split-string choice "\u200b")) :term)))

(defun org-glossary--select-term-candidatify (term-entry)
  "Create a term string from TERM-ENTRY with itself attached as a text property."
  (propertize
   (concat
    (plist-get term-entry :term)
    "\u200b"
    (make-string (max 0 (- 18 (length (plist-get term-entry :term)))) ?\s))
   'face 'font-lock-keyword-face
   'org-glossary--term term-entry))

(defun org-glossary--select-term-annotation (term-text)
  "Construct the annotation for TERM-TEXT.
Where TERM-TEXT is constructed by `org-glossary--select-term-candidatify'."
  (concat " "
          (unless org-glossary-group-ui
            (truncate-string-to-width
             (org-glossary--select-term-group term-text nil)
             9 0 ?\s))
          (replace-regexp-in-string
           "\n\s*" " "
           (string-trim
            (substring-no-properties
             (org-element-interpret-data
              (plist-get
               (get-text-property 0 'org-glossary--term term-text)
               :value)))))))

(defun org-glossary--select-term-group (term-text transform)
  "Construct the group of TERM-TEXT.
Where TERM-TEXT is constructed by `org-glossary--select-term-candidatify'."
  (if transform term-text
    (symbol-name
     (plist-get
      (get-text-property 0 'org-glossary--term term-text)
      :type))))

(defun org-glossary-goto-term-definition (&optional term-ref)
  "Go to the definition of TERM-REF.
TERM-REF may be a string, position in the buffer to look for a
term, or a term entry list. If TERM-REF is not given, the current
point will be used."
  (interactive)
  (org-glossary-update-terms)
  (when-let ((term-entry
              (if (consp term-ref) term-ref
                (or (org-glossary--quicklookup
                     (or (and (stringp term-ref) term-ref)
                         (replace-regexp-in-string
                          "^[Gg]ls\\(?:pl\\)?:" ""
                          (buffer-substring-no-properties
                           (or (previous-single-property-change
                                (1+ (or (and (numberp term-ref) term-ref) (point))) 'face)
                               (point-min))
                           (or (next-single-property-change
                                (or (and (numberp term-ref) term-ref) (point)) 'face)
                               (point-max))))))
                    (org-glossary--select-term org-glossary--terms)))))
    (if-let ((aliased-term
              (org-glossary--quicklookup
               (string-trim (org-element-interpret-data
                             (plist-get term-entry :value))))))
        (setq term-entry aliased-term))
    (let ((defsource (plist-get term-entry :definition-file)))
      (switch-to-buffer
       (or (and (bufferp defsource) defsource)
           (get-file-buffer defsource)
           (find-file defsource))))
    (goto-char (plist-get term-entry :definition-pos))
    term-entry))

(defun org-glossary-list-duplicates ()
  "Examine the currently defined terms, showing duplications."
  (interactive)
  (org-glossary-update-terms)
  (if-let ((duplicated-terms
            (org-glossary--identify-duplicates org-glossary--terms)))
      (let ((orig-buf (current-buffer))
            (buf (get-buffer-create "*Org Glossary Duplicate Terms*")))
        (with-current-buffer buf
          (erase-buffer)
          (insert (propertize (format "%d duplicated terms found"
                                      (length duplicated-terms))
                              'face 'org-level-2)
                  "\n")
          (dolist (term-duplicates duplicated-terms)
            (let ((term (car term-duplicates))
                  (duplicates (cdr term-duplicates))
                  (i 0))
              (insert (propertize (format "Duplicates for %s" term)
                                  'face 'org-level-3)
                      "\n")
              (dolist (dup duplicates)
                (insert (propertize (format " %d. " (setq i (1+ i)))
                                    'face '(bold org-list-dt))
                        (concat
                         (if (equal term (plist-get dup :key))
                             (propertize term 'face 'org-list-dt)
                           (propertize (plist-get dup :key) 'face 'shadow))
                         (and (plist-get dup :key-plural) "/")
                         (and (plist-get dup :key-plural)
                              (if (equal term (plist-get dup :key-plural))
                                  (propertize term 'face 'org-list-dt)
                                (propertize (plist-get dup :key-plural)
                                            'face 'shadow))))
                        " from ")
                (insert-text-button
                 (format "%s@%d"
                         (file-name-nondirectory (plist-get dup :definition-file))
                         (plist-get dup :definition-pos))
                 'face 'link
                 'action (lambda (_button)
                           (if (bufferp (plist-get dup :definition-file))
                               (select-window (display-buffer (plist-get dup :definition-file)
                                                              '(nil (inhibit-same-window t))))
                             (other-window 1))
                           (with-current-buffer orig-buf
                             (org-glossary-goto-term-definition dup)))
                 'help-echo "mouse-2, RET: Go to this definition"
                 'follow-link t)
                (insert "\n   "
                        (propertize
                         (truncate-string-to-width
                          (replace-regexp-in-string
                           "\n *" " "
                           (string-trim (org-element-interpret-data
                                         (plist-get dup :value))))
                          (min 120 (- (window-width) 4))
                          nil nil t)
                         'face 'font-lock-doc-face)
                        "\n"))
              (insert "\n")))
          (goto-char (point-min)))
        (pop-to-buffer buf))
    (message "No duplicate terms detected.")))

(defun org-glossary--identify-duplicates (terms)
  "Identify duplicates in TERMS."
  (let ((terms-seen (make-hash-table :size (length terms)
                                     :test #'equal))
        duplicated-terms)
    (dolist (term-entry terms)
      (dolist (keystr (list (plist-get term-entry :key)
                            (plist-get term-entry :key-plural)))
        (when keystr
          (if (gethash keystr terms-seen)
              (if (assoc keystr duplicated-terms)
                  (push term-entry
                        (alist-get keystr duplicated-terms nil nil #'equal))
                (push (list keystr (gethash keystr terms-seen) term-entry)
                      duplicated-terms))
            (puthash keystr term-entry terms-seen)))))
    duplicated-terms))

(defun org-glossary-insert-term-reference ()
  "Pick a term, and insert a reference to it."
  (interactive)
  (when-let ((term-entry (org-glossary--select-term org-glossary--terms)))
    (insert (format (if org-glossary-automatic "%s" "[[gls:%s]]")
                    (plist-get term-entry :key)))))

(defun org-glossary-create-definition (term-str definition type &optional category)
  "Add a entry for TERM-STR with DEFINITION, under TYPE and optionally CATEGORY."
  ;; This is a ugly long function, but I think it has to be this way.
  (interactive
   (let* ((term-str
           (read-string
            "Term: "
            (substring-no-properties
             (or (and (region-active-p)
                      (buffer-substring
                       (region-beginning)
                       (region-end)))
                 (thing-at-point 'word)
                 ""))))
          (definition (read-string "Definition: "))
          case-fold-search
          (type-category
           (split-string
            (completing-read
             "Type/category: "
             (mapcar #'cdr org-glossary-headings)
             nil nil
             (cond
              ((and (string-match-p "^[[:upper:]]+$" term-str)
                    (string= term-str
                             (replace-regexp-in-string
                              "[^[:upper:]]" "" definition)))
               "acronym")
              ((string-empty-p definition) "index")
              ((> (length (split-string definition)) 12) "glossary")))
            "/"))
          (type
           (car (or (rassoc (intern (car type-category)) org-glossary-headings)
                    (assoc (car type-category) org-glossary-headings)
                    (car org-glossary-headings))))
          (category (and (> (length type-category) 1) (cadr type-category))))
     (list term-str definition type category)))
  (unless (derived-mode-p 'org-mode)
    (user-error "You need to be in `org-mode' to use org-glossary"))
  (when (symbolp type)
    (setq type (car (rassoc type org-glossary-headings))))
  (save-excursion
    (let* ((type-sec-pattern
            (format "^\\*%s %s\n" (if org-glossary-toplevel-only "" "+") type))
           (type-sec-begin
            (progn
              (unless (or (re-search-forward type-sec-pattern nil t)
                          (progn (goto-char (point-min))
                                 (re-search-forward type-sec-pattern nil t)))
                (goto-char (point-max))
                (insert "\n* " type "\n"))
              (org-back-to-heading)))
           (type-hlevel
            (- (match-end 0) (match-beginning 0) 1))
           (type-sec-end
            (progn
              (org-forward-heading-same-level 1)
              (if (= type-sec-begin (point))
                  (point-max) (1- (point)))))
           (type-sec-nocat-end
            (progn
              (goto-char type-sec-begin)
              (forward-char 1)
              (or (and (re-search-forward "^\\*+ " nil t)
                       (forward-line -1)
                       (line-end-position))
                  type-sec-end)))
           (category-sec-end
            (and category
                 (if (re-search-forward
                      (format "^\\*+ %s[ \t]+:%s:\n"
                              category org-glossary--category-heading-tag)
                      nil t)
                     (or (and (re-search-forward "^\\*+ " nil t)
                              (forward-line -1)
                              (line-end-position))
                         type-sec-end)
                   (goto-char type-sec-end)
                   (insert (make-string (1+ type-hlevel) ?*) " "
                           category " :" org-glossary--category-heading-tag ":\n")
                   (point)))))
      (goto-char (or (and category category-sec-end) type-sec-nocat-end))
      (re-search-backward "^[ \t]*[-+*] \\|^\\*")
      (forward-line 1)
      (if (and definition (not (string-empty-p definition)))
          (insert (format "- %s :: %s" term-str definition) "\n")
        (insert "- " term-str "\n")))))

;;; Eldoc

(defun org-glossary--eldoc-function (&rest _)
  "Return help-echo output for org-glossary term at point.

This is intended as a :before-until advice for
`org-eldoc-documentation-function'."
  (and org-glossary-mode
       (eq (get-text-property (point) 'help-echo)
           #'org-glossary--help-echo-from-textprop)
       (org-glossary--help-echo-from-textprop nil (current-buffer) (point) t)))

(advice-add 'org-eldoc-documentation-function
            :before-until 'org-glossary--eldoc-function)

(provide 'org-glossary)
;;; org-glossary.el ends here
