;;; conllu-mode.el --- editing mode for edit wordnet data in human-readable text format files  -*- lexical-binding: t; -*-

;; dependecies

(require 'mill-flymake)

;;;###autoload

(define-derived-mode mill-mode fundamental-mode "mill"
  "TODO: docstring"
  (setq-local truncate-lines t)
  (mill-setup-flymake-backend)
  (flymake-mode))


(provide 'mill-mode)
