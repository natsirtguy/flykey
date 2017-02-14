;;; test-helper.el --- Helper for flykey-test.

;;; Commentary:
;; Helper for flykey-test.

;;; Code:

;; Thanks to http://rejeep.github.io/emacs/testing/cask/ert-runner/2013/09/26/unit-testing-in-emacs.html

(require 'f)
(require 'cl)

(defvar flykey-test-path
  (f-dirname (f-this-file)))

(defvar flykey-code-path
  (f-parent flykey-test-path))

(defvar flykey-sandbox-path
  (f-expand "sandbox" flykey-test-path))

(defvar flykey-sandbox-flyks-path
  (f-expand "flyks" flykey-sandbox-path))

(defmacro with-sandbox (&rest body)
  "Evaluate BODY in an empty temporary directory."
  `(let ((default-directory flykey-sandbox-path))
     (when (f-dir? flykey-sandbox-path)
       (f-delete flykey-sandbox-path :force))
     (f-mkdir flykey-sandbox-path)
     (f-mkdir flykey-sandbox-flyks-path)
     ;; Make flykey believe that it lives in the sandbox.
     (let ((flykey-dir flykey-sandbox-path))
       ,@body)))

(defmacro kill-leftover-buffers (&rest body)
  "Kill any new buffers created during evaluation of BODY."
  `(let ((oldbufs (buffer-list)))
     (unwind-protect
	 (progn
	   ,@body)
       (dolist (extrabufs (set-difference (buffer-list) oldbufs))
	 (kill-buffer extrabufs)))))

(defmacro with-flykey-running (&rest body)
  "Evaluate BODY with FlyKey running in a 'lisp-mode' buffer."
  `(with-sandbox
    (with-temp-buffer
      (kill-leftover-buffers
       (rename-buffer "flykey-running")
       (lisp-mode)
       ;; The window must be big enough to subdivide twice.
       (set-frame-size (selected-frame) 80 48)
       (flykey)
       ,@body))))


(require 'flykey (f-expand "flykey.el" flykey-code-path))

(provide 'test-helper)
;;; test-helper.el ends here
