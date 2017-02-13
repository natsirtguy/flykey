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
		    (concat flykey-test-path "/test.sh"))))
      (should
       (string=
	results "These are the buffer contents: echoing in the dark\n")))))

(provide 'flykey-test)
;;; flykey-test.el ends here
