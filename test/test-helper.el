;;; test-helper.el --- Helper functions to test greview -*- lexical-binding: t; -*-

(when (require 'undercover nil t)
  (undercover "*.el" (:send-report nil)))

(require 'greview)

;;; test-helper.el ends here
