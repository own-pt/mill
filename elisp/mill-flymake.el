;;; mill-flymake.el --- A mill Flymake backend  -*- lexical-binding: t; -*-

(defvar-local mill--flymake-proc nil)

(defun mill-flymake (report-fn &rest _args)
  ;; Not having a mill interpreter is a serious problem which should cause
  ;; the backend to disable itself, so an error is signaled.
  (unless (executable-find "mill")
    (error "Cannot find a suitable mill"))
  ;; If a live process launched in an earlier check was found, that
  ;; process is killed.  When that process's sentinel eventually runs,
  ;; it will notice its obsoletion, since it have since reset
  ;; `mill-flymake-proc' to a different value
  (when (process-live-p mill--flymake-proc)
    (kill-process mill--flymake-proc))
  
  ;; Save the current buffer, the narrowing restriction, remove any
  ;; narrowing restriction.
  (let ((source (current-buffer)))
    (save-restriction
      (widen)
      ;; Reset the `mill--flymake-proc' process to a new process
      ;; calling the mill tool.
      (setq
       mill--flymake-proc
       (make-process
        :name "mill-flymake" :noquery t :connection-type 'pipe
        ;; Make output go to a temporary buffer.
        :buffer (generate-new-buffer " *mill-flymake*")
        :command `("mill" "validate" ,(buffer-file-name))
        :sentinel
        (lambda (proc _event)
          ;; Check that the process has indeed exited, as it might
          ;; be simply suspended.
          (when (eq 'exit (process-status proc))
            (unwind-protect
                ;; Only proceed if `proc' is the same as
                ;; `mill--flymake-proc', which indicates that
                ;; `proc' is not an obsolete process.
                (if (with-current-buffer source (eq proc mill--flymake-proc))
                    (with-current-buffer (process-buffer proc)
                      (goto-char (point-min))
                      ;; Parse the output buffer for diagnostic's
                      ;; messages and locations, collect them in a list
                      ;; of objects, and call `report-fn'.
                      (let
			  ((file-name-match '(submatch (one-or-more nonl)))
			   (beg-match '(submatch (one-or-more digit)))
			   (end-match '(submatch (one-or-more digit)))
			   (msg-match '(submatch (one-or-more nonl) (zero-or-more "\n")
						 (zero-or-more "  " (one-or-more nonl) "\n"))))
			
			(cl-loop
			 while (search-forward-regexp
				(rx-to-string
				 `(and ,file-name-match ":" ,beg-match ":" ,end-match ,msg-match))
				nil t)
			 for msg = (match-string 4)
			 for end = (string-to-number (match-string 3))
			 for beg = (string-to-number (match-string 2))
			 for type = :error
			 collect (flymake-make-diagnostic source
                                                          beg
                                                          end
                                                          type
                                                          msg)
			 into diags
			 finally
			 (funcall report-fn diags))))
                  (flymake-log :warning "Canceling obsolete check %s"
                               proc))
              ;; Cleanup the temporary buffer used to hold the
              ;; check's output.	      
              (kill-buffer (process-buffer proc))))))))))

(defun mill-setup-flymake-backend ()
  (add-hook 'flymake-diagnostic-functions 'mill-flymake nil t))

(provide 'mill-flymake)
