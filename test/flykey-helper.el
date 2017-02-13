;;; flykey-helper.el --- Helper for flykey-test.

;;; Commentary:
;; Helper for flykey-test.

;;; Code:

(require 'f)

(defvar flykey-test-path
  (f-dirname (f-this-file)))

(defvar flykey-code-path
  (f-parent flykey-test-path))

(require 'flykey (f-expand "flykey.el" flykey-code-path))

(provide 'flykey-helper)
;;; flykey-helper.el ends here
