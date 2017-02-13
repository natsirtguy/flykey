;;; flykey-helper.el --- Helper for flykey-test.

;;; Commentary:
;; Helper for flykey-test.

;;; Code:

;; Thanks to http://rejeep.github.io/emacs/testing/cask/ert-runner/2013/09/26/unit-testing-in-emacs.html

(require 'f)

(defvar flykey-test-path
  (f-dirname (f-this-file)))

(defvar flykey-code-path
  (f-parent flykey-test-path))

(defvar flykey-sandbox-path
  (f-expand "sandbox" flykey-test-path))

(defmacro with-sandbox (&rest body)
  "Evaluate BODY in an empty temporary directory."
  `(let ((default-directory flykey-sandbox-path))
     (when (f-dir? flykey-sandbox-path)
       (f-delete flykey-sandbox-path :force))
     (f-mkdir flykey-sandbox-path)
     ,@body))

(require 'flykey (f-expand "flykey.el" flykey-code-path))

(provide 'flykey-helper)
;;; flykey-helper.el ends here
