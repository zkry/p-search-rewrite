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


;;; Vars

(defvar p-search-documentizer-functions (make-hash-table :test #'equal)
  "Hashmap of document type to document property alist ((prop-name . function)).
The documentizer is used to make common document types uniform and extendable.
Documents are given ID of the form (list type-sym element), where element can
be any Lisp object.")

(defvar p-search-candidate-generators '()
  "List of candidate-generator objects known to the p-search system.")



;;; Session Vars

;; The vars in this section are used on a per-search-session basis.

(defvar-local p-search-candidate-generators nil
  "Alist of candidate-generator objects to user-provided args alist.")


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
Takes one argument, the combined input/option arguments as an alist."))

(cl-defstruct (p-search-prior-template
               (:copier nil)
               (:constructor p-search-prior-template-create))
  "Structure representing a class of priors.
Base prior templates will have a non-nil value of `search-space-function'."
  (name nil
   :documentation "Name of prior, to be identified by the user")
  (initialize-function nil
                       :documentation "Function to populate prior results.
Called with three arguments: prior, base-priors, and args.")
  (default-result nil
   :documentation "Result that should be returned if no file is specified.")
  (input-spec nil
   :documentation "Specification of inputs required for the function to function.")
  (options-spec nil
   :documentation "Specification of parameters which alter the operation of the prior.")
  (search-space-function nil
   :documentation "Function that when called returns a list of items to be the seach space.
This function existing determines if a prior is a \"base prior\".")
  (result-hint-function nil
   :documentation "Optional function that takes the result in a buffer and
returns ranges of significance.")
  (add-prior-function nil
   :documentation "Function for base priors that dispatches the add-prior transient."))

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
  (default-result nil
   :documentation "Override of the tempate's default result."))



;;; Predefined Priors

;; This section contains pre-defined priors for searching.
;; Machinery:

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

(defun p-search-search-space ()
  "Return the search-space of the current p-search session."
  (error "Not implemented"))

;; Defaults:

(defun p-search--file-text (file-name)
  "Return the contents of OS-file FILE-NAME."
  (with-temp-buffer
    (insert-file-contents file-name)
    (buffer-substring-no-properties (point-min) (point-max))))

(p-search-def-property 'buffer 'title #'buffer-name)
(p-search-def-property 'buffer 'file-name #'buffer-file-name)
(p-search-def-property 'buffer 'content #'buffer-string)

(p-search-def-property 'file 'title #'identity)
(p-search-def-property 'file 'content #'p-search--file-text)
(p-search-def-property 'file 'file-name #'identity)

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

(provide 'p-search)

;;; p-search.el ends here
;;; terraform.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("p-search-" . "p-search2-"))
;; End:
