;;; mill-mode.el — editing mode for edit wordnet data in human-readable text format files  -*- lexical-binding: t; -*-

;; dependencies

(require 'map)
(require 'pcase)
(require 'rx)
(require 'seq)
(require 'subr-x)
(require 'xref)
(require 'mill-custom)
(require 'mill-flymake)

;; variables

(defvar mill-inter-wordnet-relations '("sa" "su" "sb")
  "List inter-WordNet relations.

These relations are used by `mill-display-related-synset' to
display the definition of a synset related to the one at point.")

(defvar mill-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; symbol components; these are used to name senses and synsets
    (modify-syntax-entry ?. "_" st)
    (modify-syntax-entry ?: "_" st)
    (modify-syntax-entry ?- "_" st)
    (modify-syntax-entry ?_ "_" st)
    (modify-syntax-entry ?' "_" st)
    (modify-syntax-entry ?@ "_" st)
    (modify-syntax-entry (string-to-char "]") "_" st)
    (modify-syntax-entry (string-to-char "[") "_" st)
    ;; comments — actually comments are only valid at the beginning of
    ;; synsets, but oh well…
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for ‘mill-mode’.")

(defvar mill--references-buffer-name "*mill-%s-references*"
  "Name of buffer where references are displayed at.")

;; constants

(defconst mill-frames-file-name
  "frames.tsv")

(defconst mill-lexnames-config-file-name
  "lexnames.tsv")

(defconst mill-relations-config-file-name
  "relations.tsv")

(defconst mill-wns-config-file-name
  "wns.tsv")


;;; fontification
;; https://web.archive.org/web/20161029181403/http://www.lunaryorn.com/posts/search-based-fontification-with-keywords.html

(defconst mill--kwds-defs
  '("w" "d" "e")
  "Mill definition keywords.")

(defconst mill--kwds-synset-rel
  (append mill-inter-wordnet-relations
	  '("hm" "hp" "hs" "vg" "mm" "fs"
	    "mp" "ms" "sim" "so" "entail"
	    "drf" "mt" "mr" "mu" "dt"
	    "dr" "du" "attr" "cause"
	    "hyper" "ihyper" "see"
	    "hypo" "ihypo" "sa" "su" "sb")))

(defconst mill--kwds-word-rel
  '("ant" "vg" "drf" "fs"
    "mt" "mr" "mu" "dt"
    "dr" "du" "pv" "pe"
    "see" "bmo" "udg" "us"
    "loc" "bdp" "prp" "evt"
    "stt" "res" "agt" "it"
    "mat" "veh" "dst"))

(defconst mill--font-lock-kwds-defs
  `(,(rx-to-string
      ;; must use rx-to-string because quasiquote breaks rx macro, and
      ;; regexp-opt can't work with it either (if you use eval, it
      ;; escapes the output string, rendering the regexp useless) —
      ;; plus or becomes regexp-opt anyway… the only downside is that
      ;; the quasiquoting prevents bytecompilation
      `(and line-start (or ,@mill--kwds-defs) ":") t)
    (0 font-lock-keyword-face)))

(defconst mill--font-lock-def-word-and-relations
  `(,(rx line-start "w:" (one-or-more space)
	 (group (one-or-more (or (syntax word) (syntax symbol)))))
    (1 font-lock-function-name-face)
    (,(rx-to-string
       `(and (group (or ,@mill--kwds-word-rel))
	     (one-or-more space)
	     (group word-start
		    (one-or-more (or (syntax word)
				     (syntax symbol)))))
       t)
     (line-end-position)
     nil
     (1 font-lock-variable-name-face)
     (2 font-lock-constant-face))))

(defconst mill--font-lock-synset-relation
  `(,(rx-to-string `(and line-start (or ,@mill--kwds-synset-rel) ":") t)
    (0 font-lock-preprocessor-face)
    (,(rx (group (one-or-more (or (syntax word)
				  (syntax symbol)))))
     (line-end-position)
     nil
     (1 font-lock-constant-face))))


(defalias 'mill-λ #'pcase-lambda)

;;  mill xref backend

(defun mill--xref-backend () 'xref-mill)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql xref-mill)))
  (thing-at-point 'symbol t))


(defun mill--parse-identifier (identifier)
  (pcase (string-trim identifier)
    ((rx
      (optional (and "@" (let maybe-wn (one-or-more (not (any ":")))) ":"))
      (optional (and (let maybe-pos (or "noun" "adj" "adv" "verb"))
		     "."
		     (let maybe-lexname (one-or-more (not (any ":"))))
		     ":"))
      (let lex-form (one-or-more (not (any "[" "]"))))
      (optional (and "["
		     (let maybe-lexical-id (one-or-more (char digit)))
		     "]")))
     (let ((lex-file (if (not maybe-lexname)
			 ;; if wn is specified so must be the lexname,
			 ;; but not the contrary
			 (buffer-file-name)
		       (mill--lexname->file-path
			(concat maybe-pos "." maybe-lexname)
			maybe-wn))))
       (list maybe-wn lex-file lex-form maybe-lexical-id)))))


(cl-defmethod xref-backend-definitions ((_backend (eql xref-mill)) identifier)
  (seq-let (_ lex-file lex-form maybe-lexical-id) (mill--parse-identifier identifier)
    (mill--collect-xref-matches lex-file
			    lex-form
			    maybe-lexical-id)))


(defun mill--collect-xref-matches (file lexical-form &optional lexical-id)
  (with-temp-buffer
    (insert-file-contents file)
    (let ((line 1)
	  (regexp (if lexical-id
		      (rx-to-string `(seq line-start "w: " ,lexical-form "[" ,lexical-id "]"))
		    (rx-to-string `(seq line-start "w: " ,lexical-form (or whitespace eol)))))
	  (matches))
      (while (not (eobp))
	(when (looking-at regexp)
	  (push (xref-make (thing-at-point 'line)
			   (xref-make-file-location file line 0))
		matches))
	(forward-line 1)
	(cl-incf line))
      (nreverse matches))))

(defun mill--grep-sentinel (f)
  "Create sentinel to used by `make-process' call of grep executable.

Call FUNCTION if the underlying grep process finishes
normally (exit codes 0 and 1), else raise an error."
  (lambda (process event-str)
    (pcase event-str
      ((or "finished\n"			       ; found something
	   "exited abnormally with code 1\n")  ; found nothing
       (funcall f))
      (_				       ; errored
       (error "Process %s %s" process event-str)))))


(defun mill-references (identifier)
  "Collect references to IDENTIFIER."
  (interactive (list (thing-at-point 'symbol t)))
  (seq-let (maybe-wn lex-file lex-form maybe-lex-id) (mill--parse-identifier identifier)
    (let ((wn (or maybe-wn (cl-first (mill--wordnet-names))))
	  (local-identifier (if maybe-lex-id
				(format "%s[%s]" lex-form maybe-lex-id)
			      lex-form)))
      (mill--collect-references local-identifier lex-file wn))))

(defun mill--collect-references (identifier lexicographer-file wn-name)
  (let* ((lexfile-paths (mill--lexicographer-file-paths))
	 (quoted-identifier (regexp-quote identifier))
	 (other-lexfile-paths (seq-difference lexfile-paths (list lexicographer-file)))
	 (qualified-identifier (regexp-quote (format "%s:%s" (file-name-nondirectory lexicographer-file) identifier)))
	 (buffer (format mill--references-buffer-name identifier))
	 (finish-sentinel (mill--grep-sentinel
			   (mill-λ ()
			     (with-current-buffer buffer
			       (goto-char (point-min))
			       ;; remove example and definition lines:
			       (delete-matching-lines "^[^:]+:[ed]:")
			       (delete-matching-lines (format "^[^:]+:w: *%s" quoted-identifier))
			       (grep-mode))
			     (display-buffer buffer))))
	 (collect-sentinel (mill--grep-sentinel
			    (mill-λ ()
			      (make-process :name "mill--other-files-references"
					    :command (cl-list* "grep" "--color" "-nH" "--null"
							       "-e" (format "\\(@%s\\)\\?%s\\([[:space:]]\\|$\\)" wn-name qualified-identifier)
							       other-lexfile-paths)
					    :buffer buffer
					    :sentinel finish-sentinel)))))
    ;; clean up buffer
    (when (get-buffer buffer)
      (with-current-buffer buffer
	(read-only-mode -1)
	(erase-buffer)))
    ;; call grep
    (make-process :name "mill--same-file-references"
		  :buffer buffer
		  :command (list "grep" "--color" "-nH" "--null"
				 "-e" (format "[[:space:]:]%s\\([[:space:]]\\|$\\)" quoted-identifier) lexicographer-file)
		  :sentinel collect-sentinel)))

;;; autocompletion

(defun mill--matches-in-file (regexp &optional buffer)
  "Return a list of matches of REGEXP in BUFFER.

If BUFFER is not given, use the current buffer."
  (let ((matches))
    (save-match-data
      (with-current-buffer (or buffer (current-buffer))
        (save-excursion
          (save-restriction
            (widen)
            (goto-char 1)
            (while (search-forward-regexp regexp nil t 1)
              (push (match-string-no-properties 1) matches)))))
      matches)))


(defun mill--sense-completions-here (to-complete lexfile &optional prefix)
  "Get completions of sense TO-COMPLETE in LEXFILE.

If prefix is non-nil, concatenate it to each completion before
returning."
  (let* (
;; FIXME: this is controversial — using find-file is not usually
;; recommended because it runs hooks and syntax-highlighting and in
;; this case the user didn't even ask for it. but if we open it
;; literally we might end up with two copies of it if the user
;; eventually opens it. if we open and close the buffer we do a lot of
;; work that might have to be redone. there seems to be no simple
;; solution here…
	 (buffer (or (get-buffer lexfile) (find-file-noselect lexfile t)))
	 (matches
	  (mill--matches-in-file
	   (rx-to-string
	    `(and line-start "w:" (one-or-more " ")
		  (group ,to-complete
			 (zero-or-more (not (any " " "\n"))))))
	   buffer)))
    (if prefix
	(mapcar (mill-λ (match) (concat prefix match)) matches)
      matches)))


(defun mill--lexnames ()
  (let* ((lexnames-path (mill--configuration-file mill-lexnames-config-file-name))
	 (lexname-lines (mill--read-tsv lexnames-path))
	 (lexnames (mapcar #'cl-second lexname-lines)))
    lexnames))


(defun mill--wordnet-names ()
  (let ((wn-lines (mill--read-tsv (mill--configuration-file mill-wns-config-file-name))))
    (mapcar #'cl-first wn-lines)))


(defun mill--other-wordnet-names ()
  ;; Assumes first wn listed is the main one.
  (cl-rest (mill--wordnet-names)))


(defun mill--lexicographer-file-paths ()
  (let* ((lexnames (mill--lexnames))
	 (this-wn-paths (mapcar #'mill--lexname->file-path lexnames))
	 (other-wns (mill--other-wordnet-names))
	 (these-wn-paths (seq-mapcat (mill-λ (wn-name)
				       (mapcar (mill-λ (lexname) (mill--lexname->file-path lexname wn-name))
					       lexnames))
				     other-wns)))
    (if (null other-wns)
	this-wn-paths
      (append this-wn-paths these-wn-paths))))


(defun mill--lexfile-prefixes ()
  "Show all possible lexfile prefixes for lexicographer files.

Used by `mill--sense-completions' to complete lexfile prefixes."
  (let* ((lexnames (mill--lexnames))
	 (wn-names (mill--other-wordnet-names)))
    (seq-mapcat (mill-λ (lexname)
		  (cons (concat lexname ":")
			;; FIXME: can't assume that every wordnet has
			;; the same lexnames
			(mapcar (mill-λ (wn-name) (concat "@" wn-name ":" lexname ":"))
				wn-names)))
		lexnames)))


(defun mill--sense-completions (to-complete &optional _buffer)
  (pcase to-complete
    ((rx (and (optional "@" (let maybe-wn-name (one-or-more (not (any ":")))) ":")
	      (let pos (or "noun" "adj" "adv" "verb")) "."
	      (let lexname (one-or-more (not (any ":")))) ":"
	      (let to-complete (zero-or-more (not (any " " "\n"))))))
     ;; pick correct lexfile
     (let ((lexfile (mill--lexname->file-path (concat pos "." lexname) maybe-wn-name)))
       ;; call function recursively
       (mill--sense-completions-here to-complete
				 lexfile
				 (concat (if maybe-wn-name
					     (concat "@" maybe-wn-name ":")
					   "")
					 pos "." lexname ":"))))
    (_	     ; could either be completions in this buffer or elsewhere
     (append
      ;; add lexnames up front since `completion-table-dynamic' will
      ;; only show them if they match
      (mill--lexfile-prefixes)
      ;; add completions of this buffer
      (mill--sense-completions-here to-complete (current-buffer))))))


(defun mill--complete-sense-at-point ()
  (interactive)
  (cl-destructuring-bind (beg . end)
      (bounds-of-thing-at-point 'symbol)
    (list beg
	  end
	  (completion-table-with-cache #'mill--sense-completions))))


;;; Utils
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
    (let (cur-indent)
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
						    mill-configuration-directory)))

    (cond
     ((file-exists-p current-directory-path)
      current-directory-path)
     ((file-exists-p configuration-file-path)
      configuration-file-path)
     (t
      (user-error
       "Can't find configuration file %s, try setting variable `mill-configuration-directory'"
       filename)))))


(defun mill--read-tsv (filepath)
  (cl-labels
      ((read-line ()
		  (unless (looking-at "[[:space:]]*$") ; empty line
		    (let* ((line (thing-at-point 'line t))
			   (fields (split-string line "\t" nil "[ \f\n\r\v]+")))
		      (pcase fields
			(`(,singleton)
			 (unless (or (equal (substring singleton 0 2) "--")
				     (string-empty-p singleton))
			   fields))
			(_ fields))))))
    (with-temp-buffer
      (insert-file-contents filepath)
      (goto-char (point-max))
      (let ((result))
	(while (not (bobp))
	  (let ((fields (read-line)))
	    (when fields
	      (push fields result)))
	  (forward-line -1))
	result))))


(defun mill--lexname->file-path (lexname &optional wn)
  "Given LEXNAME and possibly a WN, return the relative file path of specified lexicographer file."
  (if wn
      (let* ((lines (mill--read-tsv (mill--configuration-file mill-wns-config-file-name)))
	     (wn->directory-map (mapcar (mill-λ (`(,wn-name ,rel-dir . ,_)) (cons wn-name rel-dir))
					lines))
	     (wn-dir (map-elt wn->directory-map wn nil #'equal)))
	(unless wn-dir
	  (user-error "%s WordNet not specified in configuration" wn))
	(concat (file-name-as-directory wn-dir)
		lexname))
    lexname))


(defun mill--setup-tabulated-list-mode (buffer-name format entries)
  ;; used for picking new frame or relation
  (let ((buffer (generate-new-buffer buffer-name)))
    (with-current-buffer buffer
      (setq-local tabulated-list-format format)
      (setq-local tabulated-list-entries entries)
      (tabulated-list-mode)
      (tabulated-list-init-header)
      (tabulated-list-print)
      (hl-line-mode))
    (display-buffer buffer
		    (cons 'display-buffer-below-selected
			  '((window-height . fit-window-to-buffer)
			    (preserve-size . (nil . t)))))
    (select-window (get-buffer-window buffer))))


(cl-defun mill--read-relations (filepath &key obj pos)
  (let* ((relations (mill--read-tsv filepath))
	 (rel-filter (mill-λ (`(,name ,_ ,code ,_ ,rel-pos ,rel-domain ,description . ,_))
		       (when (and (not (string= code "_"))
				  (or (not pos)
				      (member pos (split-string rel-pos "," t)))
				  (or (not obj)
				      (member obj (split-string rel-domain "," t))))
			 (list (list name code description))))))
    (mapcan rel-filter relations)))


(defun mill--long-pos->short (lpos)
  (pcase lpos
    ("noun" "n")
    ("verb" "v")
    ("adj" "a")
    ("adv" "r")))


(defun mill-list-relations (obj pos)
  "List relations contained in configuration file.

The relations are shown if they have OBJ as domain and POS as PoS.

Click or press RET to insert relation code at point."
  (interactive (list
		(if (mill--at-wordsense-line?) "word" "synset")
		;; very hacky
		(mill--long-pos->short (file-name-base))))
  (let* ((original-buffer (current-buffer))
	 (format [("Code" 10 t) ("Name" 18 t) ("Description" 0 t)])
	 (relations (mill--read-relations (mill--configuration-file mill-relations-config-file-name)
				      :obj obj :pos pos))
	 (relation-to-entry (mill-λ (`(,name ,code ,description))
			      (list code
				    (vector
				     (list code 'action
					   (mill-λ (but)
					     (princ (format "%s" (button-label but))
						    original-buffer)))
				     name description))))
	 (entries (mapcar relation-to-entry relations)))
    (mill--setup-tabulated-list-mode (format " *mill-relations-%s-%s*" obj pos)
				 format entries)))



(defalias 'mill--read-frames #'mill--read-tsv "Read frames configuration file.")

(defun mill-list-frames ()
  "List frames contained in configuration file.

Press RET or click to insert frame number at point."
  (interactive)
  (let* ((original-buffer (current-buffer))
	 (format [("Number" 7 t) ("Template" 0 t)])
	 (frames (mill--read-frames (mill--configuration-file mill-frames-file-name)))
	 (frame-to-entry (mill-λ (`(,n ,template))
			   (list n
				 (vector
				  (list n 'action
					(mill-λ (but)
					  (princ (format " %s" (button-label but))
						 original-buffer)))
				  template))))
	 (entries (mapcar frame-to-entry frames)))
    (mill--setup-tabulated-list-mode " *mill-frames*" format entries)))


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

(defvar mill-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-?") #'mill-references)
    map)
  "Keymap for `mill-mode'.")

;;;###autoload
(define-derived-mode mill-mode fundamental-mode "mill"
  "`mill-mode' is a major mode for WordNet lexicographer files maintained by the mill tool.

See URL `https://github.com/own-pt/mill/'."
  :syntax-table mill-mode-syntax-table
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
  ;; to be able to use M-; to comment region
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  ;; xref
  (add-hook 'xref-backend-functions #'mill--xref-backend nil t)
  ;; completion
  (add-hook 'completion-at-point-functions #'mill--complete-sense-at-point nil t)
  ;; indentation
  (setq-local indent-line-function 'mill--indent-line))

(provide 'mill-mode)
;;; mill-mode ends here
