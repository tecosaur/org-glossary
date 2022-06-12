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
;; TODO M-x org-glossary-create-definition
;;
;; TODO support named indicies/index sections
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

(defcustom org-glossary-section "Glossary"
  "Outline heading containing term and acronym definitions.

During export, all subtrees starting with this heading will be removed."
  :type 'string)

(defcustom org-glossary-acronym-section "Acronyms"
  "Outline heading containing term and acronym definitions.

During export, all subtrees starting with this heading will be removed."
  :type 'string)

(defcustom org-glossary-index-section "Index"
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

(defcustom org-glossary-group-ui t
  "Group term definitions by type.

In practice, if using Emacs 28, this allows you to turn off
grouping, and add the target type to the annotation instead."
  :type 'boolean)

(defcustom org-glossary-global-terms nil
  "A list of globally availible term sources."
  :type '(list (choice string plist)))

(defcustom org-glossary-export-specs
  '((t (t :use "%t"
          :first-use "%u"
          :definition "%t"
          :backref "%r"
          :definition-structure-preamble ""
          :definition-structure "*%d*\\emsp{}%v\\ensp{}%b\n"
          :letter-separator "*%L*\n")
       (glossary :heading "Glossary")
       (acronym :heading "Acronyms"
                :first-use "%v (%u)")
       (index :heading "Index"
              :definition-structure "%d\\ensp{}%b\n"))
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
  :letter-separator

Within each template, the following format specs are applied:
  %t the term
  %v the term value
  %k the term key
  %r the term reference index (applicable to :use, :first-use, and :backref)
  %n the number of term references (i.e. max %r)

In :use and :first-use, %t/%v are pluralised and capitalised as
appropriate. The :first-use template can also use %u to refer to
the value of :use.

The default backend defines two special forms, expanded at the
start of the export process.
- The :definition-structure form is used as the template for the
  whole definition entry, and uses the format specs %d, %v, %b
  for the definition term, value, and backreferences respectively.
- The :letter-separator form is inserted before a block of terms
  starting with the letter, given by the format spec %l and %L in
  lower and upper case respectively.

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
  '((t :inherit (org-scheduled-previously org-glossary-term) :weight normal))
  "Face used for term references.")

(defface org-glossary-substitution-term
  '((t :inherit (org-archived org-glossary-term)))
  "Face used for term references.")

(defvar-local org-glossary--terms nil
  "The currently known terms.")

;;; Obtaining term definitions

(defun org-glossary--get-terms (&optional path-spec include-global)
  "Obtain all known terms in the current buffer.
`org-glossary-global-terms' will be used unless PATH-SPEC
is non-nil and INCLUDE-GLOBAL nil."
  (let ((term-source (org-glossary--get-terms-oneshot path-spec)))
    (org-glossary--maybe-add-global-terms
     (apply #'append
            (plist-get term-source :terms)
            (mapcar #'org-glossary--get-terms
                    (plist-get term-source :included)))
     (or include-global (null path-spec)))))

(defun org-glossary--maybe-add-global-terms (term-set do-it-p)
  "Add terms from `org-glossary-global-terms' to TERM-SET if non-nil DO-IT-P."
  (if do-it-p
      (apply #'append
             term-set
             (mapcar #'org-glossary--get-terms-cached
                     org-glossary-global-terms))
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
          :definition-file (or (buffer-file-name) (current-buffer))
          :definition-pos (+ (org-element-property :begin item) 2)
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

;;; Term usage

(defun org-glossary-apply-terms (terms &optional no-modify no-number keep-unused)
  "Replace occurances of the TERMS with links.
This returns a copy of TERMS with references recorded in :uses.

When NO-MODIFY is non-nil, the buffer content will not be modified.
When NO-NUMBER is non-nil, all links created or modified shall not include
a reference number.
When KEEP-UNUSED is non-nil, unused terms will be included in the result."
  (interactive (list org-glossary--terms nil t))
  (let ((terms (org-glossary--strip-uses terms))
        (terms-rx (org-glossary--construct-regexp terms))
        (search-spaces-regexp "[ \t\n][ \t]*")
        (case-fold-search nil)
        terms-used element-context)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward terms-rx nil t)
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
    (if keep-unused
        terms
      (setq terms-used (cl-delete-duplicates (delq nil terms-used) :test #'string=))
      (delq nil
            (mapcar
             (lambda (trm)
               (when (member (plist-get trm :key) terms-used)
                 trm))
             terms)))))

(defun org-glossary--strip-uses (terms)
  "Record a copy of TERMS with :uses set to nil."
  (mapcar (lambda (term-entry)
            (org-combine-plists term-entry '(:uses nil)))
          terms))

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
               (list org-glossary-section
                     org-glossary-acronym-section
                     org-glossary-index-section
                     org-glossary-substitution-section))
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
          term-entry
          (unless no-number
            (1+ (length (plist-get term-entry :uses))))
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
  (let* ((uses (plist-get term-entry :uses))
         (index (1+ (length uses))))
    (plist-put term-entry :uses (nconc uses (list (cons index record))))
    index))

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
                  '(glossary acronym index substitutions)))))

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
     (t "ORG-GLOSSARY-EXPORT-INVALID-SPEC"))))

(defun org-glossary--export-template (template backend info term-entry &optional ref-index plural-p capitalized-p extra-parameters)
  "Fill out TEMPLATE using BACKEND, INFO, and TERM-ENTRY.
The fields availible to the template are further affected by the
optional arguments:
 - REF-INDEX provides %r
 - PLURAL-P and CAPITALIZED-P affect %t and %v
 - EXTRA-PARAMETERS defines additional fields"
  (let ((parameters extra-parameters))
    (when (string-match-p "%k" template)
      (push (cons ?k (plist-get term-entry :key)) parameters))
    (when (string-match-p "%t" template)
      (push (cons ?t (funcall (if capitalized-p #'capitalize #'identity)
                              (plist-get term-entry
                                         (if plural-p :term-plural :term))))
            parameters))
    (when (and (not (memq ?v (mapcar #'car extra-parameters)))
               (string-match-p "%v" template))
      (push (cons ?v
                  (let ((value-str
                         (org-export-data (plist-get term-entry :value) info)))
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
                      (length (plist-get term-entry :uses))))
            parameters))
    (when (string-match-p "%u" template)
      (push (cons ?u (org-glossary--export-instance
                      backend info term-entry :use
                      ref-index plural-p capitalized-p))
            parameters))
    (format-spec template (nreverse parameters))))

;;; Export used term definitions

(defun org-glossary--print-terms (backend terms &optional types level keep-unused)
  "Produce an org-mode AST defining TERMS for BACKEND.
Do this for each of TYPES (by default: glossary, acronym, and index),
producing a heading of level LEVEL (by default: 1). If LEVEL is set to 0,
no heading is produced.
Unless keep-unused is non-nil, only used terms will be included."
  (let ((assembled-terms (org-glossary--assemble-terms
                          (if keep-unused
                              terms
                            (cl-remove-if
                             (lambda (trm)
                               (= (length (plist-get trm :uses)) 0))
                             terms))
                          types t t))
        (level (or level 1))
        export-spec content)
    (mapconcat
     (lambda (type)
       (setq export-spec (alist-get type org-glossary--current-export-spec)
             content (org-glossary--print-terms-by-letter
                      backend type (alist-get type assembled-terms)))
       (and (not (string-empty-p content))
            (if (> level 0)
                (concat
                 (make-string level ?*)
                 " "
                 (plist-get export-spec :heading)
                 "\n"
                 content)
              content)))
     (or types '(glossary acronym index))
     "\n")))

(defun org-glossary--print-terms-by-letter (backend type assembled-terms)
  "Produce an org-mode AST for TYPE in BACKEND defining ASSEMBLED-TERMS."
  (let* ((terms-per-letter
          (mapcar (lambda (tms) (length (cdr tms)))
                  assembled-terms))
         (export-spec (alist-get type org-glossary--current-export-spec))
         (use-letters-p
          (and (> (apply #'+ terms-per-letter) 15)
               (> (apply #'max terms-per-letter) 3)
               (not (string-empty-p (plist-get export-spec :letter-separator))))))
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
               (plist-get export-spec :letter-separator)
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
      assembled-terms
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

(defun org-glossary--assemble-terms (terms &optional types sort-p split-letters)
  "Collect TERMS into the form ((type . TERM-FORMS)...).
When a list of TYPES is provided, only terms which are of one of the provided
types will be used.

When SPLIT-LETTERS is non-nil TERM-FORMS will be of the form,
  ((first-char . MAYBE-SORTED-TERMS)...)
otherwise it will just be MAYBE-SORTED-TERMS.

If SORT-P is non-nil, MAYBE-SORTED-TERMS will be sorted alphabetically."
  (mapcar
   (lambda (type)
     (cons type
           (let ((type-terms
                  (cl-remove-if-not
                   (lambda (trm) (eq type (plist-get trm :type)))
                   terms)))
             (when sort-p
               (setq type-terms
                     (org-glossary--sort-plist type-terms :key #'string<)))
             (if split-letters
                 (org-glossary--assemble-terms-by-char type-terms)
               type-terms))))
   (or types (cl-delete-duplicates
              (mapcar (lambda (trm) (plist-get trm :type)) terms)))))

(defun org-glossary--assemble-terms-by-char (terms)
  "Collect TERMS into the form ((first-char . TERMS-LIST)...)."
  (mapcar
   (lambda (first-char)
     (cons first-char
           (cl-remove-if-not
            (lambda (trm)
              (eq first-char (aref (plist-get trm :key) 0)))
            terms)))
   (cl-delete-duplicates
    (mapcar (lambda (trm) (aref (plist-get trm :key) 0))
            terms))))

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

(defun org-glossary--extract-uses-in-region (terms begin end &optional types remove)
  "Extract uses of TERMS that occur between BEGIN and END.
If TYPES is non-nil, the extracted entries shall be restricted instances of TYPES.
If REMOVE is non-nil, extracted uses shall be destructively removed from TERMS."
  (let (region-terms)
    (mapc
     (lambda (term-entry)
       (when (or (not types)
                 (memq (plist-get term-entry :type) types))
         (let ((remaining-uses (cons nil (plist-get term-entry :uses)))
               region-term-uses)
           (while (cdr remaining-uses)
             (if (and (<= begin
                          (org-element-property :begin (cdadr remaining-uses))
                          (org-element-property :end (cdadr remaining-uses))
                          end)
                      (push (cadr remaining-uses) region-term-uses)
                      remove)
                 (progn
                   (setcdr remaining-uses (cddr remaining-uses))
                   (unless (car remaining-uses) ; When removing the first element.
                     (plist-put term-entry :uses (cdr remaining-uses))))
               (setq remaining-uses (cdr remaining-uses))))
           (when region-term-uses
             (push (org-combine-plists
                    term-entry (list :uses region-term-uses))
                   region-terms)))))
     terms)
    (nreverse region-terms)))

(defun org-glossary--expand-print (backend terms &optional parameters)
  "Generate string defining TERMS in BACKEND, according to PARAMETERS."
  (org-glossary--print-terms
   backend terms
   (plist-get parameters :types)
   (if (plist-get parameters :only-contents)
       0
     (1+ (plist-get parameters :level)))))

(defun org-glossary--expand-print-keyword (backend terms keyword)
  "Call `org-glossary--expand-print' with paramaters and terms based on KEYWORD.
BACKEND is passed through unmodified, but TERMS may be modified depending on
the :consume parameter extracted from KEYWORD."
  (let ((heading (org-element-lineage keyword '(headline org-data)))
        (parameters (org-combine-plists
                     org-glossary--default-print-keyword-parameters
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
         (:consume (and (stringp (cdr pair))
                        (or (string= "t" (cdr pair))
                            (string= "yes" (cdr pair)))))
         (:only-contents (and (stringp (cdr pair))
                              (or (string= "t" (cdr pair))
                                  (string= "yes" (cdr pair)))))
         (_ (cdr pair)))
       res))
    (nreverse res)))

(defvar org-glossary--default-print-keyword-parameters
  '(:type (glossary acronym index)
    :level 0
    :consume nil
    :only-contents nil)
  "The default #+print_glossary parameters.")

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
       index plural-p capitalized-p)
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
  (or (let ((plural (alist-get (downcase word)
                               org-glossary-english-plural-exceptions
                               nil nil #'string=))
            case-fold-search)
        (when plural
          (cond
           ((string-match-p "^[[:lower:]]+$" word) plural)
           ((string-match-p "^[[:upper:]][[:lower:]]+$" word)
            (capitalize plural))
           ((string-match-p "^[[:upper:]]+$" word) (upcase plural)))))
      (cond ; Source: https://github.com/plurals/pluralize/blob/master/pluralize.js#L334
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
       ((string-match "\\(seraph\\|cherub\\)\\(?:im\\)$" word)
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
        (replace-match "\1es" nil nil word))
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

(defvar-local org-glossary--term-regexp nil
  "A regexp matching all known forms of terms.")

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
                               (follow-link . org-glossary-term-definition)
                               (mouse-2 . org-glossary-term-definition))) t)))))
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
      (setq exit (null (re-search-forward org-glossary--term-regexp limit t)))
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
  (add-text-properties
   (match-beginning 0) (match-end 0)
   `(face ,(pcase (plist-get (org-glossary--quicklookup (match-string 0)) :type)
             ('glossary 'org-glossary-glossary-term)
             ('acronym 'org-glossary-acronym-term)
             ('index 'org-glossary-index-term)
             ('substitution 'org-glossary-substitution-term))
          help-echo org-glossary--term-help-echo
          og--test ,(substring-no-properties (match-string 0))
          keymap (keymap
                  (follow-link . org-glossary-term-definition)
                  (mouse-2 . org-glossary-term-definition)))))

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
  (setq org-glossary--terms (org-glossary-apply-terms
                             (org-glossary--get-terms-cached)
                             t nil t)
        org-glossary--term-regexp (org-glossary--construct-regexp
                                   org-glossary--terms)
        org-glossary--quicklookup-cache (make-hash-table :test #'equal))
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
    (make-string (max 0 (- 18 (length(plist-get term-entry :term) ))) ?\s)
    "\u200b")
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

(defun org-glossary-term-definition (&optional term-ref)
  "Go to the definition of TERM-REF.
TERM-REF may be a string or position in the buffer to look for a term.
If TERM-REF is not given, the current point will be used."
  (interactive)
  (org-glossary-update-terms)
  (when-let ((term-entry
              (or (org-glossary--quicklookup
                   (or (and (stringp term-ref) term-ref)
                       (buffer-substring-no-properties
                        (previous-single-property-change
                         (1+ (or (and (numberp term-ref) term-ref) (point))) 'face)
                        (next-single-property-change
                         (or (and (numberp term-ref) term-ref) (point)) 'face))))
                  (org-glossary--select-term org-glossary--terms))))
    (let ((def-file (plist-get term-entry :definition-file)))
      (if (bufferp def-file)
          (switch-to-buffer def-file))
      (find-file def-file))
    (goto-char (plist-get term-entry :definition-pos))))

(provide 'org-glossary)
;;; org-glossary.el ends here
