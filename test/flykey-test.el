;;; flykey-test.el --- Tests for the FlyKey package.

;;; Commentary:

;; Tests for the FlyKey package.

;;; Code:

;; Read list of quoted commands and return list of commands.
(ert-deftest flykey-read-quoted-cmds-test ()
  (let ((comquotes (list "(print \"a\")" "(print \"b\")" "(print \"c\")"))
	(coms (list '(print "a") '(print "b") '(print "c"))))
    (should
     (equal (flykey-read-quoted-cmds comquotes) coms))))

;; Send the contents of a buffer to a shell script and return the results.
(ert-deftest flykey-send-buffer-script-test ()
  (with-temp-buffer
    (insert "echoing in the dark")
    (let ((results (flykey-send-buffer-script
		    (current-buffer)
		    ;; flykey-test-path is defined in test-helper.el
		    (concat flykey-test-path "/test.sh"))))
      (should
       (string=
	(car results)
	"These are the buffer contents: echoing in the dark")))))

;; Create the list of keymap commands using a .flyk buffer.
(ert-deftest flykey-make-map-cmds-test ()
  (let ((testf (concat flykey-test-path "/test.flyk"))
	(cmdlist
	 (list
	  '(local-set-key (kbd "i")
			  (lambda () (interactive) (insert "\\int")))
	  '(local-set-key (kbd "u")
			  (lambda () (interactive) (insert "awesome"))))))
    (with-temp-buffer
      (insert-file-contents testf)
      (should (equal cmdlist (flykey-make-map-cmds (current-buffer)))))))

;; Evaluate a list of commands in a buffer.
(ert-deftest flykey-eval-cmds-test ()
  (let ((cmdlist (list '(setq x 5) '(setq y 8) '(setq z (+ x y)))))
    (with-temp-buffer
      (flykey-eval-cmds cmdlist)
      (should (= z 13)))))

;; Actually add the keybindings from test.flyk
(ert-deftest flykey-add-bindings-test ()
  (let ((byhand
	 (with-temp-buffer
	   (local-set-key (kbd "i")
			  (lambda () (interactive) (insert "\\int")))
	   (local-set-key (kbd "u")
			  (lambda () (interactive) (insert "awesome")))
	   (current-local-map)))
	(testf (concat flykey-test-path "/test.flyk")))
    (with-temp-buffer
      (insert-file-contents testf)
      (let ((flybuf (current-buffer)))
	(with-temp-buffer
	  (flykey-add-bindings flybuf (current-buffer))
	  (should (equal (current-local-map) byhand)))))))

;; Open a buffer with a brand new .flyk file.
(ert-deftest flykey-open-flyk-test ()
  ;; Since this will create files, work in a sandbox directory.
  (with-sandbox
   (kill-leftover-buffers
    ;; FlyKey wants to add the files to the directory that contains it,
    ;; so we need to override that behavior. See test-helper.el.
    ;; Test for the case with no .flyk.
    (with-temp-buffer
      (python-mode)
      (let ((flybuf (flykey-open-flyk)))
	(let ((flybufcontents
	       (with-current-buffer flybuf (buffer-string))))
	  (should (string= flybufcontents "#python-mode\n"))
	  (should
	   (string= (buffer-file-name flybuf)
		    (concat flykey-sandbox-path "/flyks/python-mode.flyk"))))))
    ;; Test for the case with .flyk.
    (with-temp-buffer
      (lisp-mode)
      (with-current-buffer (get-buffer-create "lisp-mode.flyk")
	(insert "#lisp-mode\n t=that \n")
	(write-file (concat flykey-sandbox-path "/flyks/lisp-mode.flyk"))
	(kill-buffer))
      (let ((flybuf (flykey-open-flyk)))
	(let ((flybufcontents
	       (with-current-buffer flybuf (buffer-string))))
	  (should (string= flybufcontents "#lisp-mode\n t=that \n"))))))))

;; Determine whether my helper macro kill-leftover-buffers is working.
(ert-deftest kill-leftover-buffers-test ()
  (with-temp-buffer
    (let ((buflist (buffer-list)))
      (kill-leftover-buffers
       (get-buffer-create "someotherbuffer"))
      (should (equal buflist (buffer-list))))))

;; Check that flykey-reload-map does not run the buffer-list-update-hook.
(ert-deftest flykey-test-buffer-list ()
  (with-flykey-running
   (select-window (get-buffer-window flykey-flybuf))
   (end-of-buffer)
   (insert "a=\\ant\n")
   (defun flykey-check-update-reloading ()
     (print "Watch out Peter!"))
   (add-hook 'buffer-list-update-hook 'flykey-check-update-reloading)
   (print "Begin sending script.")
   ;; (let ((bufcontents (with-current-buffer flykey-flybuf (buffer-string))))
   ;;   (shell-command-to-string (concat flykey-sh-file " \"" bufcontents "\"")))
   (print "End sending script.")
   (remove-hook 'buffer-list-update-hook 'flykey-check-update-reloading)))

(provide 'flykey-test)
;;; flykey-test.el ends here
