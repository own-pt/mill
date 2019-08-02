;;; conllu-mode.el --- editing mode for edit wordnet data in human-readable text format files  -*- lexical-binding: t; -*-

;; dependecies

(require 'mill-flymake)
(require 'xref)

;; constants

;;; fontification

(defconst mill--kwds-defs
  '("w" "d")
  "Mill definition keywords.")

(defconst mill--kwds-synset-rel
  '("hm" "hp" "hs" "vg" "mm"
    "mp" "ms" "sim" "entail"
    "drf" "mt" "mr" "mu" "dt"
    "dr" "du" "attr" "cause"
    "hyper" "ihyper" "see"
    "hypo" "ihypo"))

(defconst mill--kwds-word-rel
  '("ant" "vg" "drf"
    "mt" "mr" "mu" "dt"
    "dr" "du" "pv" "pe"
    "see"))

(defconst mill--font-lock-kwds-defs
  `(,(rx-to-string `(: line-start (or ,@mill--kwds-defs) ":"))
    (0 font-lock-keyword-face)))

(defconst mill--font-lock-def-word-and-relations
  `("^w: +\\(\\sw+ *[0-9]*\\)"
    (1 font-lock-function-name-face)
    (,(rx-to-string `(: (group (or ,@mill--kwds-word-rel))
			  (1+ space)
			  (group word-start (one-or-more word) word-end
				 (0+ space)
				 (0+ digit))))
     (line-end-position)
     nil
     (1 font-lock-variable-name-face)
     (2 font-lock-constant-face))))

(defconst mill--font-lock-synset-relation
  `(,(rx-to-string `(: line-start (or ,@mill--kwds-synset-rel) ":"))
    (0 font-lock-preprocessor-face)
    ("\\(\\sw+ *[0-9]*\\)"
     (line-end-position)
     nil
     (0 font-lock-constant-face))))

;;  mill xref backend

(defun mill--xref-backend () 'xref-mill)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql xref-mill)))
  (if (or (eq (get-char-property (point) 'face) 'font-lock-function-name-face)
	  (eq (get-char-property (point) 'face) 'font-lock-constant-face))
      (let ((beg (previous-single-property-change (point) 'face nil (line-beginning-position)))
	    (end (next-single-property-change (point) 'face nil (line-end-position))))
	(buffer-substring beg end))))


(defun mill-xref-collect-matches (regexp file) ;; TODO: document
  ;; inspired by `xref-collect-matches'
  (let ((command (format "grep -n -e \"%s\" %s" regexp file)) ;; TODO: grep adds external dependece, remove?
	(def default-directory)
	(grep-re "^\\([0-9]+\\):\\(.*\\)")
	(status)
	(hits))
    (with-current-buffer (get-buffer-create " *xref-mill-grep*")
      (erase-buffer)
      (setq default-directory def)
      (setq status
            (call-process-shell-command command nil t))
      (goto-char (point-min))

      (when (and (/= (point-min) (point-max))
                 (not (looking-at grep-re)))
        (user-error "Search failed with status %d: %s" status (buffer-string)))

      (cl-loop while (re-search-forward  grep-re nil t)
	       collect (xref-make (match-string 2)
				  (xref-make-file-location file
							   (string-to-number (match-string 1))
							   0))
	       into hits
	       finally (return hits)))))


(cl-defmethod xref-backend-definitions ((_backend (eql xref-mill)) identifier)
  (let ((ident (string-trim identifier)))
    (string-match "\\(\\(.*\\):\\(\\sw+\\)\\|\\(\\sw+\\)\\) *\\([0-9]*\\)" ident)
    (let ((file (if (match-string 2 ident)
		    (format "%s%s" default-directory (match-string 2 ident))
		  (buffer-file-name)))
	  (word (or (match-string 3 ident) (match-string 4 ident)))
	  (position (match-string 5 ident)))
      (mill-xref-collect-matches
       (if (string-empty-p position)
	   (concatenate 'string "^w: \\+" word "\\( *$\\| \\+[^0-9]\\+.*\\)")
	 (concatenate 'string "^w: \\+" word " \\+" position))
       file))))

;;;###autoload

(define-derived-mode mill-mode fundamental-mode "mill"
  "TODO: docstring"

  ;; syntax-table
  ;;; word
  (modify-syntax-entry ?. "w")
  (modify-syntax-entry ?: "w")
  (modify-syntax-entry ?- "w")
  (modify-syntax-entry ?_ "w")
  (modify-syntax-entry ?' "w")

  ;; truncate-lines
  (setq-local truncate-lines t)

  ;; flymake
  (mill-setup-flymake-backend)
  (flymake-mode)
  (remove-hook 'after-change-functions 'flymake-after-change-function t)

  ;; fontification
  (setq font-lock-defaults
        `((,mill--font-lock-def-word-and-relations
	   ,mill--font-lock-kwds-defs
	   ,mill--font-lock-synset-relation)
	  nil))

  ;; xref
  (add-hook 'xref-backend-functions #'mill--xref-backend nil t))

(provide 'mill-mode)
