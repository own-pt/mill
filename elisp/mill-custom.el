;;; mill-mode.el — customizable variables -*- lexical-binding: t; -*-

(defcustom mill-configuration-directory nil
  "Path to directory where wordnet configuration files reside in, or nil."
  :group 'mill
  :type '(choice file (const nil)))

(defcustom mill-validate-one-wordnet nil
  "If non-nil, only consider the specified WordNet in
validation. String value should be the one found in the
configuration file wns.tsv."
  :group 'mill
  :type '(choice string (const nil)))

(provide 'mill-custom)
;;; mill-custom.el ends here
