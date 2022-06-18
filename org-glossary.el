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
;; Package-Requires: ((emacs "27.1") (org "9.6"))
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
;; DONE make the export formatting/style customisable
;;
;; DONE load terms from #+include'd files
;;
;; DONE fontification of terms
;;
;; DONE jump to definition, and
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
;; DONE org-glossary-global-terms
;;
;; DONE M-x org-glossary-create-definition
;;
;; DONE support named definition sections
;;
;; DONE support for term aliases
;;
;; DONE support #+print_glossary: :terms glossary acronyms :level N :only-contents nil :consume t
;;
;; REVIEW maybe support generating the glossary/acronym etc.
;; in the file, like org-toc.?
;; This is complicated by the way we treat * Glossary sections etc.
;;
;; TODO check for glossary updates with an idle timer, if performance
;; characteristics allow (maybe with a heuristic for file size/complexity).
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
;;; Code:

(require 'org)

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(defgroup org-glossary nil
  "Defined terms and abbreviations in Org."
  :group 'org
  :prefix "org-glossary-")

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
  :type '(list (choice string plist)))

(defcustom org-glossary-default-print-parameters
  '(:type (glossary acronym index)
    :level 0
    :consume nil
    :all nil
    :only-contents nil)
  "The default print parameters.
These can be set by #+print_glossary in babel :key value style."
  :type 'plist)

(defcustom org-glossary-export-specs
  '((t (t :use "%t"
          :first-use "%u"
          :definition "%t"
          :backref "%r"
          :definition-structure-preamble ""
          :definition-structure "*%d*\\emsp{}%v\\ensp{}%b\n"
          :category-heading "* %c\n"
          :letter-heading "*%L*\n")
       (glossary :heading "Glossary")
       (acronym :heading "Acronyms"
                :first-use "%v (%u)")
       (index :heading "Index"
              :definition-structure "%d\\ensp{}%b\n")
       (substitution :heading ""
                     :use "%v"
                     :definition-structure ""
                     :letter-heading ""))
    (latex (t :use "\\hyperlink{gls-%k}{\\label{gls-%k-use-%r}%t}"
              :definition "\\hypertarget{gls-%k}{%t}"
              :backref "\\pageref{gls-%k-use-%r}"))
    (html (t :use "<a class=\"org-gls\" href=\"#gls.%k\" id=\"glsr.%k.%r\">%t</a>"
             :definition "<span class=\"org-glsdef\" id=\"gls.%k\">%t</span>"
             :backref "<a class=\"org-glsdef\" href=\"#glsr.%k.%r\">%r</a>"))
    (ascii (t :definition-structure "*%d* %v [%n uses]\n")
           (index :definition-structure "%d [%n uses]\n"))
    (org (t :use "<<gr;%k;%r>>[[g;%k][%t]]"
            :backref "[[gr;%k;%r][%r]]"
            :definition-structure "- <<g;%k>>%t :: %v\\ensp{}%b")
         (index :definition-structure "- <<g;%k>>%t\\ensp{}%b")))
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
There are also two special forms for the default template spec:
  :definition-structure
  :letter-heading

Within each template, the following format specs are applied:
  %t the term
  %v the term value
  %k the term key
  %r the term reference index (applicable to :use, :first-use, and :backref)
  %n the number of term references (i.e. max %r)
  %c the category of the term

In :use and :first-use, %t/%v are pluralised and capitalised as
appropriate. The :first-use template can also use %u to refer to
the value of :use.

The default backend defines three special forms, expanded at the
start of the export process:
- The :definition-structure form is used as the template for the
  whole definition entry, and uses the format specs %d, %v, %b
  for the definition term, value, and backreferences respectively.
- The :letter-heading form is inserted before a block of terms
  starting with the letter, given by the format spec %l and %L in
  lower and upper case respectively.
- The :category-heading form is inserted before a block of terms
  all assigned a particular category, given by the format spec %c.

Instead of a format string, one can also provide a function as a
template spec so long as it matches the function signature of
`org-glossary--export-template'.

The literal content of :definition-structure-preamble is inserted
before the first :definition-structure in each block of
definitions.

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

(defcustom org-glossary-display-substitute-value t
  "Whether to display substitutions as their value.
Requires `org-glossary-fontify-types-differently' to be non-nil."
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

;;; Obtaining term definitions

(defun org-glossary--get-terms (&optional path-spec include-global)
  "Obtain all known terms in the current buffer.
`org-glossary-global-terms' will be used unless PATH-SPEC
is non-nil and INCLUDE-GLOBAL nil."
  (let ((term-source (org-glossary--get-terms-oneshot path-spec)))
    (org-glossary--maybe-add-global-terms
     #'org-glossary--get-terms
     (apply #'append
            (plist-get term-source :terms)
            (mapcar #'org-glossary--get-terms
                    (plist-get term-source :included)))
     (or include-global (null path-spec)))))

(defun org-glossary--maybe-add-global-terms (term-getter term-set do-it-p)
  "Apply TERM-GETTER to `org-glossary-global-terms' and add to TERM-SET if non-nil DO-IT-P."
  (if do-it-p
      (apply #'append
             term-set
             (mapcar term-getter org-glossary-global-terms))
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
                (save-restriction
                  (widen)
                  (org-element-parse-buffer)))
            (with-temp-buffer
              (setq buffer-file-name (plist-get path-spec :file))
              (org-glossary--include-once path-spec)
              (set-buffer-modified-p nil)
              (save-restriction
                (widen)
                (org-element-parse-buffer))))))
    (list :path path-spec
          :scan-time (current-time)
          :terms (org-glossary--extract-terms parse-tree)
          :included
          (mapcar
           #'org-glossary--parse-include-value
           (org-element-map parse-tree 'keyword
             (lambda (kwd)
               (when (string= "INCLUDE" (org-element-property :key kwd))
                 (org-element-property :value kwd))))))))

(defun org-glossary--complete-path-spec (path-spec)
  "Given a tentative PATH-SPEC, try to get a proper one."
  (or (and (stringp path-spec)
           (org-glossary--parse-include-value path-spec))
      path-spec
      (org-glossary--parse-include-value
       (buffer-file-name))
      (current-buffer)))

(defun org-glossary--include-once (parameters)
  "Include content based on PARAMETERS."
  (unless (eq (plist-get parameters :env) 'literal)
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
   :included LIST-OF-PATH-SPECS)")

(defun org-glossary--get-terms-cached (&optional path-spec include-global)
  "Obtain all known terms in the current buffer, using the cache.
`org-glossary-global-terms' will be used unless PATH-SPEC
is non-nil and INCLUDE-GLOBAL nil."
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
          (or (and term-source-cached
                   (if cache-valid t
                     (delq term-source-cached org-glossary--terms-cache)
                     nil)
                   (cdr term-source-cached))
              (cdar (push (cons path-spec
                                (org-glossary--get-terms-oneshot path-spec))
                          org-glossary--terms-cache)))))
    (org-glossary--maybe-add-global-terms
     #'org-glossary--get-terms-cached
     (apply #'append
            (plist-get term-source :terms)
            (mapcar #'org-glossary--get-terms-cached
                    (plist-get term-source :included)))
     (or include-global (null path-spec)))))

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
                         (save-restriction
                           (widen)
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
         (type-category (org-glossary--entry-type-category
                         (org-element-lineage item '(headline))))
         (item-contents (and (org-element-property :tag item)
                             (org-element-contents item)))
         (value (mapcar
                 #'org-element-extract-element
                 (if (and (= (length item-contents) 1)
                          (eq (caar item-contents) 'paragraph))
                     (org-element-contents (car item-contents))
                   item-contents))))
    (list :key key
          :key-plural (unless (string-empty-p key-plural) key-plural)
          :term term
          :term-plural (unless (string-empty-p plural) plural)
          :alias-for nil
          :type (car type-category)
          :category (cdr type-category)
          :value value
          :definition-file (or (buffer-file-name) (current-buffer))
          :definition-pos (+ (org-element-property :begin item) 2)
          :extracted nil
          :uses nil)))

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
  (let ((key-term-map (make-hash-table :test #'equal :size (length terms)))
        value-str)
    (dolist (term-entry terms)
      (puthash (plist-get term-entry :key) term-entry key-term-map))
    (dolist (term-entry terms)
      (when-let ((value (plist-get term-entry :value))
                 (value-str (string-trim (org-element-interpret-data value)))
                 (associated-term (gethash value-str key-term-map)))
        (plist-put term-entry :alias-for associated-term)
        (plist-put term-entry :value (plist-get associated-term :value)))))
  terms)

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
        (case-fold-search nil)
        (start-time (float-time))
        (last-redisplay (float-time))
        terms-used element-context)
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
          (push (plist-get (org-glossary--update-link
                            terms element-context no-modify no-number)
                           :key)
                terms-used))
         ((and org-glossary-automatic
               (memq 'link (org-element-restriction element-context)))
          (push (plist-get (org-glossary--update-plain
                            terms no-modify no-number)
                           :key)
                terms-used)))))
    (message "Scanned for term usage in buffer (took %.2f seconds)."
             (- (float-time) start-time))
    (if keep-unused
        terms
      (setq terms-used (cl-delete-duplicates (delq nil terms-used) :test #'string=))
      (delq nil
            (mapcar
             (lambda (trm)
               (when (member (plist-get trm :key) terms-used)
                 trm))
             terms)))))

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

(defun org-glossary--mrx-search-forward (tagged-patterns &optional limit)
  "Find the closest matching pattern in TAGGED-PATTERNS before LIMIT.
Each entry in the list TAGGED-PATTERNS should be of the form:
  (TAG . STRINGS)

In the case of two equally close matches from TAGGED-PATTERNS,
the longest match will be used.

This is necessitated by problems when trying to apply
`regexp-opt' to many items, which can trigger:
  Lisp error: (invalid-regexp \"Regular expression too big\")"
  (let ((match-start most-positive-fixnum) (match-stop -1)
        the-match tag)
    (dolist (t-pat tagged-patterns)
      (save-excursion
        (when (and (re-search-forward (cdr t-pat) limit t)
                   (or (< (match-beginning 0) match-start)
                       (and (= (match-beginning 0) match-start)
                            (> (match-end 0) match-stop))))
          (setq the-match (match-data)
                match-start (match-beginning 0)
                match-end (match-end 0)
                tag (car t-pat)))))
    (set-match-data the-match)
    (and the-match
         (setq org-glossary--mrx-last-tag tag)
         (goto-char match-end))))

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
                     (if (eq 'acronym (plist-get trm :type))
                         (list (plist-get trm key))
                       (when-let ((term-str (plist-get trm key))
                                  (term-letter1 (aref term-str 0)))
                         (if (eq term-letter1 (upcase term-letter1))
                             (list term-str)
                           (list term-str (concat (string (upcase term-letter1))
                                                  (substring term-str 1)))))))
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
         (lambda () (org-element-link-interpreter
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

;;; Export, general functionality

(defvar-local org-glossary--current-export-spec nil)

(defun org-glossary--get-export-specs (backend)
  "Determine the relevant export specs for BACKEND from `org-glossary-export-specs'."
  (let* ((default-spec (alist-get t org-glossary-export-specs))
         (current-spec
          (or (cl-some
               (lambda (export-spec)
                 (when (org-export-derived-backend-p backend (car export-spec))
                   (cdr export-spec)))
               org-glossary-export-specs)
              default-spec))
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
  (let ((parameters extra-parameters)
        (canonical-term (or (plist-get term-entry :alias-for) term-entry)))
    (when (string-match-p "%k" template)
      (push (cons ?k (plist-get canonical-term :key)) parameters))
    (when (string-match-p "%t" template)
      (push (cons ?t (funcall (if capitalized-p #'capitalize #'identity)
                              (plist-get term-entry
                                         (if plural-p :term-plural :term))))
            parameters))
    (when (and (not (memq ?v (mapcar #'car extra-parameters)))
               (string-match-p "%v" template))
      (push (cons ?v
                  (let ((value-str
                         (org-export-data (plist-get canonical-term :value) info)))
                    (funcall (if capitalized-p #'capitalize #'identity)
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
                      (length (plist-get canonical-term :uses))))
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

;;; Export used term definitions

(defun org-glossary--print-terms (backend terms &optional types level duplicate-mentions)
  "Produce an org-mode AST defining TERMS for BACKEND.
Do this for each of TYPES (by default: `org-glossary-default-print-parameters''s :type),
producing a heading of level LEVEL (by default: 1). If LEVEL is set to 0,
no heading is produced.
Unless duplicate-mentions is non-nil, terms already defined will be excluded."
  (let ((terms-by-type
         (org-glossary--group-terms
          (org-glossary--sort-plist
           (cl-remove-if
            (lambda (trm)
              (or (not (plist-get trm :uses)) ; This occurs when `trm' is an alias.
                  (and (not duplicate-mentions)
                       (plist-get trm :extracted))))
            terms)
           :key #'string<)
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
            (make-string level ?*)
            " "
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
          (org-glossary--sort-plist terms :key #'string<)
          (lambda (trm) (plist-get trm :category))))
        (export-spec (alist-get type org-glossary--current-export-spec))
        content cat-heading)
    (if (= (length terms-by-category) 1)
        (org-glossary--print-terms-by-letter backend type terms)
      (mapconcat
       (lambda (cat-terms)
         (setq content (org-glossary--print-terms-by-letter
                        backend type (cdr cat-terms))
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

(defun org-glossary--print-terms-by-letter (backend type terms)
  "Produce an org-mode AST for TYPE in BACKEND defining ASSEMBLED-TERMS."
  (let* ((terms-by-letter
          (org-glossary--group-terms
           (org-glossary--sort-plist terms :key #'string<)
           (lambda (trm) (aref (plist-get trm :key) 0))))
         (num-terms-by-letter (mapcar (lambda (trms) (length (cdr trms)))
                                      terms-by-letter))
         (export-spec (alist-get type org-glossary--current-export-spec))
         (use-letters-p
          (and (> (apply #'+ num-terms-by-letter) 15)
               (> (apply #'max num-terms-by-letter) 3)
               (not (string-empty-p (plist-get export-spec :letter-heading))))))
    (concat
     (and (not use-letters-p)
          (not (string-empty-p (plist-get export-spec :definition-structure-preamble)))
          (concat (plist-get export-spec :definition-structure-preamble) "\n"))
     (mapconcat
      (lambda (letter-terms)
        (let ((letter (car letter-terms))
              (terms (cdr letter-terms)))
          (concat
           (when use-letters-p
             (concat
              "\n"
              (format-spec
               (plist-get export-spec :letter-heading)
               `((?l . ,(string letter))
                 (?L . ,(string (upcase letter)))))
              "\n"
              (and (not (string= "" (plist-get export-spec :definition-structure-preamble)))
                   (concat (plist-get export-spec :definition-structure-preamble) "\n"))))
           (mapconcat
            (lambda (term-entry)
              (org-glossary--print-terms-singular backend term-entry))
            terms
            "\n"))))
      terms-by-letter
      "\n"))))

(defun org-glossary--print-terms-singular (backend term-entry)
  (org-glossary--export-instance
   backend nil term-entry :definition-structure
   nil nil nil
   `((?d . ,(format "[[glsdef:%s]]" (plist-get term-entry :key)))
     (?v . ,(string-trim (org-element-interpret-data
                          (plist-get term-entry :value))))
     (?b . ,(mapconcat
             (lambda (use)
               (format "[[glsuse:%d:%s]]"
                       (car use) (plist-get term-entry :key)))
             (cl-sort
              (plist-get term-entry :uses)
              #'< :key #'car)
             ", ")))))

(defun org-glossary--group-terms (terms predicate &optional include)
  "Group TERMS according to PREDICATE, and only INCLUDE certain groups (if non-nil)."
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
If TYPES is non-nil, the extracted entries shall be restricted instances of TYPES.
If MARK-EXTRACTED is non-nil, extracted uses shall be marked as extracted."
  (let (region-terms region-term-uses)
    (mapc
     (lambda (term-entry)
       (when (or (not types)
                 (memq (plist-get term-entry :type) types))
         (setq region-term-uses nil)
         (dolist (use (plist-get term-entry :uses))
           (when (and (<= begin
                          (org-element-property :begin (cdadr remaining-uses))
                          (org-element-property :end (cdadr remaining-uses))
                          end)
                      (push use region-term-uses)
                      mark-extracted)
             (plist-put use :extracted t)))
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
                         :face 'org-glossary-term)
(org-link-set-parameters "glspl"
                         :export #'org-glossary--link-export-glspl
                         :face 'org-glossary-term)
(org-link-set-parameters "Gls"
                         :export #'org-glossary--link-export-Gls
                         :face 'org-glossary-term)
(org-link-set-parameters "Glspl"
                         :export #'org-glossary--link-export-Glspl
                         :face 'org-glossary-term)

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
            (string= description "org-glossary-index-stub")
            '((?t . "") (?u . "%t"))))
    (funcall (if capitalized-p #'capitalize #'identity)
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
      (org-glossary--export-instance backend info term-entry :definition)
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
  (setq org-glossary--terms (org-glossary--get-terms-cached nil t)
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
          (replace-match "[[gls:\\1][org-glossary-index-stub]]")))))
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

(add-hook 'org-export-before-parsing-hook #'org-glossary--prepare-buffer)

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
                       help-echo org-glossary--term-help-echo
                       keymap (keymap
                               (follow-link . org-glossary-goto-term-definition)
                               (mouse-2 . org-glossary-goto-term-definition))) t)))))
  per-term-p)

(define-minor-mode org-glossary-mode
  "Glossary term fontification, and enhanced interaction."
  :global nil
  :group 'org-glossary
  (cond
   ((and org-glossary-mode org-glossary-automatic)
    (org-glossary--set-font-lock-keywords org-glossary-fontify-types-differently)
    (font-lock-add-keywords nil org-glossary--font-lock-keywords 'append)
    (org-glossary-update-terms))
   (t (font-lock-remove-keywords nil org-glossary--font-lock-keywords)
      (save-restriction
        (widen)
        (font-lock-flush)))))

(defun org-glossary--fontify-find-next (&optional limit)
  "Find any next occurance of a term reference, for fontification."
  (let (match-p exit element-at-point element-context)
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

(defun org-glossary--fontify-term ()
  "Fontify the matched term."
  (let ((term-entry (org-glossary--quicklookup (match-string 0))))
    (add-text-properties
     (match-beginning 0) (match-end 0)
     (nconc
      (pcase (plist-get term-entry :type)
        ('substitution
         (if org-glossary-display-substitute-value
             `(face org-glossary-substituted-value
                    mouse-face org-glossary-substitution-term
                    display
                    ,(string-trim
                      (substring-no-properties
                       (org-element-interpret-data
                        (plist-get term-entry :value)))))
           '(face org-glossary-substitution-term)))
        (type `(face ,(or (alist-get type org-glossary-fontify-type-faces)
                          'org-glossary-term))))
      `(help-echo
        org-glossary--term-help-echo
        keymap (keymap
                (follow-link . org-glossary-goto-term-definition)
                (mouse-2 . org-glossary-goto-term-definition)))))))

(defun org-glossary--term-help-echo (_window object pos)
  "Find the term reference at POS in OBJECT, and get the definition."
  (when-let ((term-entry
              (org-glossary--quicklookup
               (with-current-buffer object
                 (buffer-substring-no-properties
                  (previous-single-property-change (1+ pos) 'face)
                  (next-single-property-change pos 'face))))))
    (format "(%s) %s %s"
            (propertize
             (symbol-name (plist-get term-entry :type))
             'face 'org-table)
            (propertize
             (plist-get term-entry :term)
             'face 'org-list-dt)
            (string-trim
             (org-element-interpret-data
              (plist-get term-entry :value))))))

;;; Interaction

(defvar-local org-glossary--quicklookup-cache (make-hash-table :test #'equal)
  "A hash table for quickly looking up a term-entry from a reference form.")

(defun org-glossary--quicklookup (term-str)
  "Find the term entry reffered to by TERM-STR."
  (or (gethash term-str org-glossary--quicklookup-cache)
      (puthash term-str
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
                    :key-plural))
               org-glossary--quicklookup-cache)))

(defun org-glossary-update-terms ()
  "Update the currently known terms."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (user-error "You need to be in `org-mode' to use org-glossary."))
  (let ((initial-terms (mapcar (lambda (trm) (plist-get trm :term))
                               org-glossary--terms))
        current-terms added-terms removed-terms)
    (setq org-glossary--terms (org-glossary--get-terms-cached nil t)
          org-glossary--term-mrx
          (org-glossary--mrx-construct-from-terms org-glossary--terms)
          org-glossary--quicklookup-cache (make-hash-table :test #'equal))
    (setq current-terms (mapcar (lambda (trm) (plist-get trm :term))
                                org-glossary--terms)
          added-terms (cl-set-difference current-terms initial-terms :test #'string=)
          removed-terms (cl-set-difference initial-terms current-terms :test #'string=))
    (when (or added-terms removed-terms)
      (message "%s"
               (concat
                "org-glossary: "
                (and added-terms
                     (format "%s term%s added"
                             (length added-terms)
                             (if (> (length added-terms) 1) "s" "")))
                (and added-terms removed-terms ", ")
                (and removed-terms
                     (format "%s term%s removed"
                             (length removed-terms)
                             (if (> (length removed-terms) 1) "s" "")))))))
  (when org-glossary-mode
    (save-restriction
      (widen)
      (font-lock-flush))))

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
    (make-string (max 0 (- 18 (length(plist-get term-entry :term) ))) ?\s))
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
          (string-trim
           (substring-no-properties
            (org-element-interpret-data
             (plist-get
              (get-text-property 0 'org-glossary--term term-text)
              :value))))))

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
TERM-REF may be a string or position in the buffer to look for a term.
If TERM-REF is not given, the current point will be used."
  (interactive)
  (org-glossary-update-terms)
  (when-let ((term-entry
              (or (org-glossary--quicklookup
                   (or (and (stringp term-ref) term-ref)
                       (buffer-substring-no-properties
                        (or (previous-single-property-change
                             (1+ (or (and (numberp term-ref) term-ref) (point))) 'face)
                            (point-min))
                        (or (next-single-property-change
                             (or (and (numberp term-ref) term-ref) (point)) 'face)
                            (point-max)))))
                  (org-glossary--select-term org-glossary--terms))))
    (if-let ((aliased-term
              (org-glossary--quicklookup
               (string-trim (org-element-interpret-data
                             (plist-get term-entry :value))))))
        (setq term-entry aliased-term))
    (let ((def-file (plist-get term-entry :definition-file)))
      (if (bufferp def-file)
          (switch-to-buffer def-file))
      (find-file def-file))
    (goto-char (plist-get term-entry :definition-pos))))

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
  (unless (eq major-mode 'org-mode)
    (user-error "You need to be in `org-mode' to use org-glossary."))
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

(provide 'org-glossary)
;;; org-glossary.el ends here
