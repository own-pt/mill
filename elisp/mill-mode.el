;;; conllu-mode.el --- editing mode for edit wordnet data in human-readable text format files  -*- lexical-binding: t; -*-

;; dependecies

(require 'mill-flymake)

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

;;;###autoload

(define-derived-mode mill-mode fundamental-mode "mill"
  "TODO: docstring"

  ;; syntax-table
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
	  nil)))

(provide 'mill-mode)
