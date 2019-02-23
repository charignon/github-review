;;; test-helper.el --- Helper functions to test github-review -*- lexical-binding: t; -*-

(when (require 'undercover nil t)
  (undercover "*.el" (:send-report nil)))

(require 'github-review)

;;; test-helper.el ends here
