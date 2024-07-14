;;; p-search.el --- Emacs Search Tool Aggregator -*- lexical-binding: t; -*-

;; Author: Zachary Romero
;; URL: https://github.com/zkry/p-search.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools
;;

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; P-SEARCH is an tool for combining and executing searches in Emacs.
;; The tool takes its inspiration from Bayesian search theory where it
;; is assumed that the thing being searched for has a prior
;; distribution of where it can be found, and that the act of looking
;; should update our posterior probability distribution.

;; Terminology: In p-search there are parts of the search, the prior
;; and likelihood.  The prior is specified via certain predicates that
;; reflect your beliefs where the file is located.  For example, you
;; could be 90% sure that a file is in a certain directory, and 10%
;; elsewhere.  Or you can be very sure that what you are looking for
;; will contain some form of a search term.  Or you may have belife
;; that the object you are looking for may have a more active Git log
;; than other files.  Or you think you remember seeing the file you
;; were looking for in one of your open buffers.  And so on.  The
;; important thing is that priors have 1) an objective criteria 2) a
;; subjective belief tied to the criteria.
;;
;; The second part of the equation is the likelihood.  When looking
;; for something, the very act of looking for something and not
;; finding doesn't mean that the its not there!  Think about when
;; looking for your keys.  You may check the same place several times
;; until you actually find them.  The act of observation reduces your
;; probability that the thing being looked for is there, but it
;; doesn't reduce it to zero.  When looking for something via
;; p-search, you mark the item with one of several gradations of
;; certainty that the element being looked for exists.  After
;; performing the observation, the probabilities where things exists
;; gets updated.

;;; Code:

(require 'heap nil t) ;; TODO: Remove noerror
(require 'cl-lib)
(require 'subr-x)
(require 'eieio)
(require 'range)
(require 'org)
(require 'transient)



;;; Custom
(defgroup p-search nil
  "Emacs Search Tool Aggregator."
  :prefix "p-search-"
  :group 'applications)


;;; Consts

(defconst p-search-score-yes 0.7)
(defconst p-search-score-neutral 0.3)
(defconst p-search-score-no 0.3)



;;; Vars

(defvar p-search-documentizer-functions (make-hash-table :test #'equal)
  "Hashmap of document type to document property alist ((prop-name . function)).
The documentizer is used to make common document types uniform and extendable.
Documents are given ID of the form (list type-sym element), where element can
be any Lisp object.")

(defvar p-search-candidate-generators '()
  "List of candidate-generator objects known to the p-search system.")

(defvar p-search-default-candidate-generators '()
  "List of candidate generators to be applied on startup of p-search session.")



;;; Session Vars

;; The vars in this section are used on a per-search-session basis.

(defvar-local p-search-candidates-cache nil
  "Cache of generated candidates.")

(defvar-local p-search-candidate-generators nil
  "Alist of candidate-generator objects to user-provided args alist.")

(defvar-local p-search-priors nil
  "List of active prior components for search.")

(defvar-local p-search-active-prior nil
  "Dynamic variable of prior currently being acted on.")

(defvar-local p-search-posterior-probs nil
  "Heap of calculated posterior probabilities.
Elements are of the type (DOC-ID PROB).")

(defvar-local p-search--section-level 0
  "Variable used to determine leveling of nested sections.")


;;; Faces
(defgroup p-search-faces nil
  "Faces used by p-saerch."
  :group 'p-search
  :group 'faces)

(defface p-search-section-highlight
  `((((class color) (background light))
     :extend t
     :background "grey95")
    (((class color) (background  dark))
     :extend t
     :background "grey20"))
  "Face for highlighting the current section."
  :group 'p-search-faces)

(defface p-search-section-heading
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "DarkGoldenrod4"
     :weight bold)
    (((class color) (background  dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "LightGoldenrod2"
     :weight bold))
  "Face for section headings."
  :group 'p-search-faces)

(defface p-search-header-line-key
  '((t :inherit font-lock-builtin-face))
  "Face for keys in the `header-line'."
  :group 'p-search-faces)

(defface p-search-value
  '((t :inherit transient-value))
  "Face for keys in the `header-line'."
  :group 'p-search-faces)

(defface p-search-hi-yellow
  '((((min-colors 88) (background dark))
     (:weight bold :box (:line-width 1 :color "yellow1" :style nil)))
    (((background dark)) (:weight bold :box (:line-width 1 :color "yellow" :style nil)))
    (((min-colors 88)) (:weight bold :box (:line-width 1 :color "yellow1" :style nil)))
    (t (:weight bold :box (:line-width 1 :color "yellow" :style nil))))
  "Face for highlighting in p-search mode with bold text and a box."
  :group 'p-search-faces)



;;; Types

(cl-defstruct (p-search-candidate-generator
               (:copier nil)
               (:constructor p-search-candidate-generator-create))
  "Structure representing a method of creating search candidates."
  (name nil
   :documentation "Name of the generator, to be shown on search page.")
  (input-spec nil
   :documentation "Specification of inputs required for the function to function.")
  (options-spec nil
   :documentation "Specification of optional inputs required for the function to function.")
  (function nil
   :documentation "Function to generate list of candidates.
Takes one argument, the combined input/option arguments as an alist.")
  (term-freq-function nil
   :documentation "Function to determine term frequencies of docs generated by generator."))

(cl-defstruct (p-search-prior-template
               (:copier nil)
               (:constructor p-search-prior-template-create))
  "Structure representing a class of priors.
Base prior templates will have a non-nil value of `search-space-function'."
  (group nil :documentation "Group symbol of prior (e.g. git, filesystem)")
  (required-properties nil
   :documentation "List of required properties for the prior to be applicable.")
  (name nil
   :documentation "Name of prior, to be identified by the user")
  (input-spec nil
   :documentation "Specification of inputs required for the function to function.")
  (options-spec nil
                :documentation "Specification of parameters which alter the operation of the prior.")
  (initialize-function nil
                       :documentation "Function to populate prior results.
Called with three arguments: prior, base-priors, and args.")

  (result-hint-function nil
   :documentation "Optional function that takes the result in a buffer and
returns ranges of significance."))

(cl-defstruct (p-search-prior
               (:copier nil)
               (:constructor p-search-prior-create))
  "An instantiated prior created from a template informs a search."
  (template nil :type p-search-prior-template)
  (importance nil
   :documentation "How much the prior should influence results.")
  (results nil
   :documentation "hash table containing the result.
Maps from file name to result indicator.")
  (proc-or-thread nil
   :documentation "This slot stores the process or thread that does main computation.")
  (arguments nil
   :documentation "Arguments provided to the prior.  These are the union of inputs and options.")
  (default-result nil ;; TODO - is this needed?
   :documentation "Override of the tempate's default result."))



;;; Helper Functions

(defun p-search--file-text (file-name)
  "Return the contents of OS-file FILE-NAME."
  (with-temp-buffer
    (insert-file-contents file-name)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun p-search--git-available-p ()
  "Return non-nil if git is available from default directory."
  (= (call-process "git" nil nil nil "status") 0))



;;; Documentizer

;; A document in p-seach is an alist of information retrieval (IR) properties.
;; An example of a document is as follows:
;;
;; ((id . (book . "123")) (title . "Othello") (content . "...."))
;;
;; The documentizer exists in order to provide a standard interface to
;; create documents of a given type, and to allow the interface to be
;; extendible, being the bridge between entities (be it on the
;; filesystem or in Emacs) and IR documents.

(defun p-search-def-property (type property-symbol function)
  "Define property PROPERTY-SYMBOL on TYPE by calling FUNCTION."
  (let* ((funcs (gethash type p-search-documentizer-functions))
         (newelt (cons property-symbol function)))
    (if (alist-get property-symbol funcs)
        (setf (alist-get property-symbol funcs) newelt)
      (push newelt funcs))
    (puthash type funcs p-search-documentizer-functions)))

(defun p-search-documentize (doc-id)
  "Given DOC-ID, return IR properties."
  (pcase-let ((`(,doc-type ,element) doc-id)
              (results `((id . ,doc-id))))
    (pcase-dolist (`(,prop-id . ,function) (gethash doc-type p-search-documentizer-functions))
      (let* ((property-fetcher (lambda () (funcall function element))))
        (push (cons prop-id property-fetcher) results)))
    (nreverse results)))

(defun p-search-get-document-property (document property-id)
  "Return property PROPERTY-ID of DOCUMENT."
  (alist-get property-id document))

(p-search-def-property 'buffer 'title #'buffer-name)
(p-search-def-property 'buffer 'file-name #'buffer-file-name)
(p-search-def-property 'buffer 'content #'buffer-string)

(p-search-def-property 'file 'title #'identity)
(p-search-def-property 'file 'content #'p-search--file-text)
(p-search-def-property 'file 'file-name #'identity)



;;; Prior API

(defun p-search-candidates ()
  "Return the search candidates as map from id to document."
  (or p-search-candidates-cache
      (let ((candidates-set (make-hash-table :test 'equal)))
        (pcase-dolist (`(,gen . ,args) p-search-candidate-generators)
          (let* ((documents (funcall (p-search-candidate-generator-function gen) args)))
            (dolist (doc documents)
              (let* ((id (alist-get 'id doc)))
                (when (not (gethash id candidates-set))
                  (puthash id doc candidates-set))))))
        (setq p-search-candidates-cache candidates-set)
        candidates-set)))

(defun p-search-document-property (document property)
  "Return PROPERTY of DOCUMENT."
  (let ((elt (assoc property document)))
    (when elt
      (pcase-let ((`(_ . ,val) elt))
        (if (functionp val)
            (let* ((res (funcall val)))
              (setcdr elt res)
              res)
          val)))))

(defun p-search-candidates-with-property (property)
  "Return hashmap of documents with non-nil PROPERTY."
  (let* ((documents (p-search-candidates))
         (res-hashmap (make-hash-table :test 'equal)))
    (maphash
     (lambda (id document)
       (let* ((val (p-search-document-property document property)))
         (when val
           (puthash id document res-hashmap))))
     documents)
    res-hashmap))

(defun p-search-unique-candidate-properties (property)
  "Return a list of unique values of PROPERTY across the search candidates."
  '())

(defun p-search-set-score (document value)
  "Set the score of DOCUMENT to VALUE."
  (unless p-search-active-prior
    (error "Ensure that initialization function is called with `p-search-active-prior' bound"))
  (let* ((candidates (p-search-candidates))
         (results-ht (p-search-prior-results p-search-active-prior))
         (id (alist-get 'id document)))
    (unless id
      (error "Unable to get document id of %s" document))
    (when (gethash id candidates)
      (puthash id value results-ht))))


;;; Predefined Priors and Candidate Generators

;; This section contains pre-defined priors for searching.

(defconst p-search-candidate-generator-buffers
  (p-search-candidate-generator-create
   :name "BUFFERS"
   :input-spec '()
   :options-spec '()
   :function
   (lambda (_args)
     (seq-map
      (lambda (buf)
        (p-search-documentize `(buffer ,buf)))
      (buffer-list)))))

(defconst p-search-candidate-generator-filesystem
  (p-search-candidate-generator-create
   :name "FILESYSTEM"
   :input-spec '((base-directory . (directory-name
                                    :key "d"
                                    :description "Directories"
                                    :default (lambda () default-directory)))
                 (filename-regexp . (regexp
                                     :key "f"
                                     :description "Filename Pattern"
                                     :default ".*")))
   :options-spec '((ignore-pattern . (regexp
                                      :key "-i"
                                      :description "Ignore Patterns"
                                      :multiple t))
                   (use-git-ignore . (toggle
                                      :key "-g"
                                      :description "Git Ignore"
                                      :default on)))
   :function
   (lambda (args)
     (let-alist args
       (let* ((default-directory .base-directory)
              (git-available-p (p-search--git-available-p)))
         (when (and .use-git-ignore (not git-available-p))
           (message "Cannot use git ignore for directory %s.  Falling back on all files." default-directory))
         (let* ((default-directory .base-directory)
                (file-candidates (if (and .use-git-ignore git-available-p)
                                     (string-split (shell-command-to-string "git ls-files") "\n" t "[\n ]")
                                   (string-split (shell-command-to-string "find . -type f") "\n" t "[\n ]")))
                (documents '()))
           (dolist (file file-candidates)
             (catch 'skip
               (when (string-prefix-p "./" file)
                 (setq file (substring file 2)))
               (unless (or (equal .filename-regexp ".*")
                           (string-match-p .filename-regexp file))
                 (throw 'skip nil))
               (when (and .ignore-pattern (string-match-p .ignore-pattern file))
                 (throw 'skip nil))
               (setq file (file-name-concat default-directory file))
               (push (p-search-documentize `(file ,file)) documents)))
           (nreverse documents)))))))

;;; Generic priors

(defconst p-search-prior-title
  (p-search-prior-template-create
   :group 'general
   :name "title-name"
   :required-properties '(title)
   :input-spec '((title . (string
                           :key "-t"
                           :description "Document's Title")))
   :initialize-function
   (lambda (args)
     (let* ((title (alist-get 'title args))
            (documents (p-search-candidates-with-property 'title)))
       (maphash
        (lambda (_ document)
          (let* ((doc-title (p-search-document-property document 'title)))
            (when (string-search title doc-title)
              (p-search-set-score document p-search-score-yes))))
        documents)))))

;;; Buffer priors

(defconst p-search-prior-major-mode
  (p-search-prior-template-create
   :group 'emacs
   :name "buffer"
   :required-properties '(buffer)
   :input-spec '((major-mode . (string
                                :key "-m"
                                :description "Buffer major mode")))
   :initialize-function
   (lambda (args)
     (let* ((major-mode-sym (intern (alist-get 'major-mode args)))
            (documents (p-search-candidates)))
       (maphash
        (lambda (_ document)
          (let* ((buffer (p-search-document-property document 'buffer)))
            (with-current-buffer buffer
              (when (eql major-mode-sym major-mode)
                (p-search-set-score document p-search-score-yes)))))
        documents)))))

;;; File system priors

(defconst p-search-prior-subdirectory
  (p-search-prior-template-create
   :group 'filesystem
   :name "subdirectory"
   :required-properties '(file-name)
   :input-spec '((include-directories . (directory-names
                                         :key "i"
                                         :description "Directories")))
   :initialize-function
   (lambda (args)
     (let* ((directories (alist-get 'include-directories args))
            (documents (p-search-candidates)))
       (maphash
        (lambda (_ document)
          (catch 'out
            (let* ((file-name (p-search-document-property document 'file-name))
                   (file-expanded (expand-file-name file-name)))
              (dolist (dir directories)
                (when (string-prefix-p dir file-expanded)
                  (p-search-set-score document p-search-score-yes)
                  (throw 'out nil))))))
        documents)))))

;;; Git Priors

;; TODO --



;;; Posterior Calculation and Heap integration

;; Each search candidate document is assigned a probability based on
;; the user-defined prior distribution, and the users search
;; observations.
;;
;; The prior function is composed in two parts: first a probability is
;; assigned to each candidate based on how well the the prior provides
;; evidence of relevance.  For example, a user looking for a file with
;; "/tests/" in its path might assign a 0.7 to the file "/tests/a.el"
;; and 0.3 to "/src/a.el".  While yes or now priors may assign 0.7 and
;; 0.3 respectively, a hypothetical contains-vowels-in-name prior may
;; assign the gradations 0.7 to "/aaa/foo.el", 0.55 to "cdf/foo.el", 0.4 to
;; "cdf/a.el", and 0.3 to "cdf/xz.clj".
;;
;; After the "objective" probability is assigned, the user assigns a
;; level of importance to the prior, and based on the importance, the
;; "objective" probability is transformed to better reflect the users
;; beliefs of relevance.  So for example, if the above test assigns
;; 0.7 to file "/tests/a.el", but the user doesn't think this test is
;; that important, its probability shifts closer to 0.5, while if the
;; user thought that the test was vital, then it would shift closer to
;; 1.0.  This transformation is done via the BetaI function.
;;
;; After the prior distribution is calculated, the final posterior is
;; calculated based on the users observations.  A user can mark files
;; as observed.  Marking a file as observed will reduce the
;; probability of the file containing the thing being looked for but
;; it will not reduce it to 0.  Various gradation of observations can
;; exist, with more work-intensive observations reducing the documents
;; probability more than shorter observations.





;;; p-search Major Mode

;; This section contains the machinery for the p-search major mode.
;; The p-search major mode is for interacting with a search session.  The user
;; should be able to see an overview of what's being searched for and the
;; various priors being applied.  The p-search major mode is also used for
;; interacting with the various search results.

(defun p-search--add-candidate-generator (generator args)
  "Append GENERATOR with ARGS to the current p-search session."
  ;; Ensure that there is an alist entry in ARGS for every item in
  ;; input-spec of generator.
  (pcase-dolist (`(,key . _) (p-search-candidate-generator-input-spec generator))
    (unless (alist-get key args)
      (error "Unable to create candidate generator %s, missing arg %s"
             (p-search-candidate-generator-name generator)
             key)))
  (setq p-search-candidates-cache nil)
  (setq p-search-candidate-generators
        (append p-search-candidate-generators
                (list (cons generator args)))))

(defun p-search-initialize-session-variables ()
  "Instantiate the session-specific local variables."
  ;; (setq p-search-observations (make-hash-table :test 'equal))
  (setq p-search-candidates-cache nil)
  (setq p-search-candidate-generators nil)
  (setq p-search-priors nil)
  (setq p-search-active-prior nil))

(defun p-search--setup-candidate-generators ()
  "Setup initial candidate generators for session."
  ;; TODO: figure out how I want the user to specify desired initial priors.
  (p-search--add-candidate-generator p-search-candidate-generator-buffers '()))

(defmacro p-search-add-section (section-name &rest body)
  "Insert a collapsable section at the point with heading SECTION-NAME.
BODY should then insert the contents of the collapsible section, making
sure to end with a newline.  The section then spans from the start of
the heading to the point where BODY leaves off."
  (declare (indent 1))
  (let ((start (make-symbol "start"))
        (end (make-symbol "end"))
        (props (make-symbol "props"))
        (key (make-symbol "key")))
    `(let ((,start (point))
           (p-search--section-level (1+ p-search--section-level))
           (,props (and (not (stringp ,section-name))
                        (alist-get 'props ,section-name nil)))
           (,key (and (not (stringp ,section-name))
                      (alist-get 'key ,section-name nil))))
       (if (stringp ,section-name)
           (insert ,section-name)
         (insert (alist-get 'heading ,section-name)))
       (when (not (= (char-after (1- (point))) ?\n))
         (insert "\n"))
       ,@body
       (let ((,end (point)))
         (p-search-add-section-overlay ,start ,end ,props
                                       (or ,key (and (stringp ,section-name)
                                                     ,section-name)))))))

(defun p-search--insert-candidate-generator (generator-args)
  "Insert candidate generator args cons GENERATOR-ARGS into current buffer."

  )

(defun p-search--insert-prior (prior)
  "Insert PRIOR into current buffer.")

(defun p-search--insert-results ()
  "Insert the search results into current buffer."
  (when p-search-posterior-probs
    (let* ((elts '()))
      ())))

(defun p-search--reprint ()
  "Redraw the current buffer from the session's state."
  (unless (derived-mode-p 'p-search-mode)
    (error "Unable to print p-search state of buffer not in p-search-mode"))
  (let* ((inhibit-read-only t)
         (at-line (line-number-at-pos)))
    ;; TODO - occlusion states
    (erase-buffer)
    (p-search-add-section "Candidate Generators"
      (dolist (generator-args p-search-candidate-generators)
        (p-search--insert-candidate-generator generator-args)))
    (p-search-add-section "Priors"
      (dolist (prior p-search-priors)
        (p-search--insert-prior prior)))
    ;; TODO - Toggle occluded sections
    (p-search--insert-results)))

(defun p-search-setup-buffer ()
  "Initial setup for p-search buffer."
  (let* ((buffer (generate-new-buffer "p-search")))
    (with-current-buffer buffer
      (p-search-mode))
    (let ((win (display-buffer buffer nil)))
      (select-window win))
    (with-current-buffer buffer
      (p-search--setup-candidate-generators)
      (p-search--reprint))
    buffer))

(define-derived-mode p-search-mode special-mode "p-search"
  "Major mode for p-search."
  :group 'p-search
  (hack-dir-local-variables-non-file-buffer))



;;; Commands

(defun p-search ()
  "Start a p-search session."
  (interactive)
  (p-search-setup-buffer))


;;; Spec Helpers

;; A input/option specification is an alist of the form field-id to
;; cons of type and options, or visually as fallows:
;;
;; (name . (type . options-p-list))
;;
;; The following is an example specification:
;;
;; '((ignore-pattern . (regexp
;;                      :key "-i"
;;                      :description "Ignore Patterns"
;;                      :multiple t))  ;; TODO - implement multiple
;;   (use-git-ignore . (toggle
;;                      :key "-g"
;;                      :description "Git Ignore"
;;                      :default on)))
;;
;; Input refers to required parameters while options refers to
;; optional parameters.

(defun p-search--spec-default-arguments (spec)
  "Return default input and options of SPEC as one alist."
  (let* ((res '()))
    (pcase-dolist (`(,name . (,_type . ,options)) spec)
      (let* ((default (plist-get options :default))
             (default-val (if (functionp default) (funcall default) default)))
        (setq res (cons
                   (cons name default-val)
                   res))))
    (nreverse res)))


;;; Default Options

(add-to-list 'p-search-candidate-generators p-search-candidate-generator-buffers)
(add-to-list 'p-search-candidate-generators p-search-candidate-generator-filesystem)

(provide 'p-search)

;;; p-search.el ends here
;;; terraform.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("p-search-" . "p-search2-"))
;; End:
