;;; mill-mode.el --- editing mode for edit wordnet data in human-readable text format files  -*- lexical-binding: t; -*-

;; dependencies

(require 'seq)
(require 'rx)
(require 'map)
(require 'pcase)
(require 'xref)
(require 'mill-flymake)

;; customizable variable

(defcustom mill--configuration-directory nil
  "Path to directory where wordnet configuration files reside in, or `nil'."
  :group 'mill
  :type '(choice file (const nil)))

(defvar mill-inter-wordnet-relations '("sa" "su" "sb")
  "These relations are used by `mill-display-related-synset' to
  display the definition of a synset related to the one at
  point.")

;; constants

;; (defconst mill--frames-file-name
;;   "frames.tsv")

(defconst mill--lexnames-config-file-name
  "lexnames.tsv")

(defconst mill--wns-config-file-name
  "wns.tsv")


;;; fontification

(defconst mill--kwds-defs
  '("w" "d" "e")
  "Mill definition keywords.")

(defconst mill--kwds-synset-rel
  (append mill-inter-wordnet-relations
	  '("hm" "hp" "hs" "vg" "mm"
	    "mp" "ms" "sim" "entail"
	    "drf" "mt" "mr" "mu" "dt"
	    "dr" "du" "attr" "cause"
	    "hyper" "ihyper" "see"
	    "hypo" "ihypo" "sa" "su" "sb")))

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


(defalias 'mill-λ #'pcase-lambda)

;;  mill xref backend

(defun mill--xref-backend () 'xref-mill)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql xref-mill)))
  (if (or (eq (get-char-property (point) 'face) 'font-lock-function-name-face)
	  (eq (get-char-property (point) 'face) 'font-lock-constant-face))
      (let ((beg (previous-single-property-change (point) 'face nil
						  (line-beginning-position)))
	    (end (next-single-property-change (point) 'face nil
					      (line-end-position))))
	(buffer-substring beg end))))


(cl-defmethod xref-backend-definitions ((_backend (eql xref-mill)) identifier)
  (pcase (string-trim identifier)
    ((rx
      (let maybe-wn (optional "@" (one-or-more (not (any ?:))) ":"))
      (let maybe-lex-name (optional (or "noun" "adjs" "adj" "adv" "verb")
				    "."
				    (one-or-more (not (any ?:)))
				    ":"))
      (let lex-form       (one-or-more (not (any ? ))))
      (optional (1+ space))
      (let maybe-lex-id   (optional (one-or-more (char digit)))))
     (let ((lex-file (if (string-empty-p maybe-lex-name)
			 ;; if wn is specified so must be the lexname,
			 ;; but not the contrary
			 (buffer-file-name)
		       (mill--lexname->file-path
			(string-trim-right maybe-lex-name ":")
			(string-trim-right (substring maybe-wn 1) ":"))))
	   (lex-id (unless (string-empty-p maybe-lex-id) maybe-lex-id)))
       (mill--collect-xref-matches lex-file
			       lex-form
			       lex-id)))))


(defun mill--collect-xref-matches (file lexical-form &optional lexical-id)
  (with-temp-buffer
    (insert-file-contents file)
    (let ((line 1)
	  (regexp (if lexical-id
		      (rx-to-string `(seq line-start "w: " ,lexical-form " " ,lexical-id))
		    (rx-to-string `(seq line-start "w: " ,lexical-form
					(or (seq (zero-or-more " ") line-end)
					    (seq " " (not (any digit))))))))
	  (matches nil))
      (while (not (eobp))
	(when (looking-at regexp)
	  (push (xref-make (thing-at-point 'line)
			   (xref-make-file-location file line 0))
		matches))
	(forward-line 1)
	(cl-incf line))
      (nreverse matches))))


(defun mill-display-related-synset ()
  "Display definition of related synset synset in another window.

A related synset is a synset that relates to the synset at point
by any of the relations in `mill-inter-wordnet-relations'. If
several of these relations are found, the first is used."
  (interactive)
  (let* ((original-buffer (current-buffer))
	 (original-window (get-buffer-window original-buffer))
	 (original-point (point)))
    (forward-char)     ;make it work when at first character of synset
    (backward-sentence) 		;go to synset start
    ;; search for related synset
    (re-search-forward (format "^%s: *" (regexp-opt mill-inter-wordnet-relations t))
		       (mill--sentence-end-point) t 1)
    ;; if successful, this command opens related synset in other
    ;; window and selects it
    (let ((identifier (xref-backend-identifier-at-point (mill--xref-backend))))
      (unless identifier (user-error "No suitable identifier found"))
      (xref-find-definitions-other-window identifier))
    ;; this recenters it
    (backward-sentence)
    (recenter-top-bottom 0)
    ;; re-select original window, recenter it, and put point at proper
    ;; point
    (select-window original-window)
    (with-current-buffer original-buffer
      (backward-sentence)
      (recenter-top-bottom 0)
      (goto-char original-point))))


(defun mill--sentence-end-point ()
  (save-excursion
    (forward-sentence)
    (point)))


;; indentation

(defconst default-tab-width 3)

(defun mill--indent-line ()
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)
    (let ((not-indented t) cur-indent)
      (if (looking-at "^[ \t]*\\sw+:")
	  (setq cur-indent 0)
	(save-excursion
	  (forward-line -1)
	  (if (looking-at "^d:")
	      (setq cur-indent default-tab-width)
	    (setq cur-indent (current-indentation)))))
      (indent-line-to cur-indent))))

;; interactive commands

(defun mill--configuration-file (filename)
  (let* ((current-directory-path (expand-file-name filename))
	 (configuration-file-path (expand-file-name filename
						    mill--configuration-directory)))

    (cond
     ((file-exists-p current-directory-path)
      current-directory-path)
     ((file-exists-p configuration-file-path)
      configuration-file-path)
     (t
      (user-error
       "Can't find configuration file %s, try setting variable `mill--configuration-directory'"
       filename)))))


(defun mill--read-tsv (filepath)
  (cl-labels
      ((read-line ()
		  (let* ((line (thing-at-point 'line t))
			 (fields (split-string line "\t" nil "[ \f\n\r\v]+")))
		    (pcase fields
		      (`(,singleton)
		       (unless (or (equal (substring singleton 0 2) "--")
				   (string-empty-p singleton))
			 fields))
		      (_ fields)))))
    (with-temp-buffer
      (insert-file-contents filepath)
      (goto-char (point-max))
      (let ((result nil))
	(while (not (bobp))
	  (let ((fields (read-line)))
	    (when fields
	      (push fields result)))
	  (forward-line -1))
	result))))


(defun mill--lexname->file-path (lexname &optional wn)
  "Given LEXNAME and possibly a WN, return the relative file path of specified lexicographer file"
  (if wn
      (let* ((lines (mill--read-tsv (mill--configuration-file mill--wns-config-file-name)))
	     (wn->directory-map (mapcar (mill-λ (`(,wn-name ,rel-dir)) (cons wn-name rel-dir))
					lines))
	     (wn-dir (map-elt wn->directory-map wn nil #'equal)))
	(unless wn-dir
	  (user-error "%s WordNet not specified in configuration" wn))
	(concat (file-name-as-directory wn-dir)
		lexname))
    lexname))

;;; FIXME: add frame
;; (defalias 'mill--read-frames #'mill--read-tsv "Read frames configuration file.")

;; (defun mill-new-frame ()
;;   "Create new frame at point."
;;   (interactive)
;;   (let* ((frames (mill--read-frames (mill--configuration-file mill--frames-config-path)))
;; 	 (choices (seq-map-indexed (lambda (elt idx) (cons (+ 97 idx) elt)) frames)))
;;     (read-multiple-choice "Select frame: " choices)))


(defun mill--at-wordsense-line? ()
  (save-excursion
    (goto-char (line-beginning-position))
    (looking-at "w:")))

;;; FIXME: add these
;; (defun mill--new-wordsense-relation ())


;; (defun mill--new-synset-relation ())


;; (defun mill-new-relation ()
;;   "Create new relation at point."
;;   (interactive)
;;   (if (mill--at-wordsense-line?)
;;       (mill--new-wordsense-relation)
;;     (mill--new-synset-relation)))


;;;###autoload
(define-derived-mode mill-mode fundamental-mode "mill"
  "TODO: docstring"

;;; syntax-table
  ;; word
  (modify-syntax-entry ?. "w")
  (modify-syntax-entry ?: "w")
  (modify-syntax-entry ?- "w")
  (modify-syntax-entry ?_ "w")
  (modify-syntax-entry ?' "w")
  (modify-syntax-entry ?@ "w")
  

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

  ;; to be able to use M-a and M-e to jump
  (setq-local sentence-end ".$$")

  ;; xref
  (add-hook 'xref-backend-functions #'mill--xref-backend nil t)

  ;; indentation
  (setq-local indent-line-function 'mill--indent-line))

(provide 'mill-mode)
