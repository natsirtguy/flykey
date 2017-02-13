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
	results "These are the buffer contents: echoing in the dark\n")))))

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
(ert-deftest flkey-open-flyk-test ()
  ;; Since this will create files, work in a sandbox directory.
  (with-sandbox
   ;; FlyKey wants to add the files to the directory that contains it,
   ;; so we need to override that behavior.
   (let ((flykey-dir flykey-sandbox-path))
     (with-temp-buffer
       (flykey-open-flyk)))))

(provide 'flykey-test)
;;; flykey-test.el ends here
