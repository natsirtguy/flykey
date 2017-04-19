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

;; Create the list of keymap commands using a .flyk buffer.
(ert-deftest flykey-make-map-cmds-test ()
  (let ((testf (concat flykey-test-path "/test.flyk"))
	(cmdlist
	 (list
	  '(local-set-key (kbd "u")
			  (lambda () (interactive) (insert "awesome")))
	  '(local-set-key (kbd "i")
			  (lambda () (interactive) (insert "\\int"))))))
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
	   (local-set-key (kbd "u")
			  (lambda () (interactive) (insert "awesome")))
	   (local-set-key (kbd "i")
			  (lambda () (interactive) (insert "\\int")))
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

;; Use helper macro kill-leftover-buffers.
(ert-deftest kill-leftover-buffers-test ()
  (with-temp-buffer
    (let ((buflist (buffer-list)))
      (kill-leftover-buffers
       (get-buffer-create "someotherbuffer"))
      (should (equal buflist (buffer-list))))))

;; Reload map with new command
(ert-deftest flykey-buffer-list-test ()
  (with-flykey-running
   (select-window (get-buffer-window flykey-flybuf))
   (goto-char (point-max))
   (insert "a=\\ant\n")
   (flykey-reload-map)))

;; Kill various flykey buffers.
(ert-deftest flykey-kill-buffer-test ()
  (with-flykey-running
   (kill-buffer flykey-flybuf))
  (with-flykey-running
   (kill-buffer flykey-insertbuf))
  (with-flykey-running
   (kill-buffer)))

;; Generate the pairs of normal binding commands.
(ert-deftest flykey-create-normal-cmd-pairs-test ()
  (let ((bufstr "blah \nt=this\nw=who\ne=a=b"))
    (should (equal
	     (flykey-create-normal-cmd-pairs bufstr)
	     '(("e" . "a=b") ("w" . "who") ("t" . "this"))))))

;; Generate pairs of command binding commands.
(ert-deftest flykey-create-command-cmd-pairs-test ()
  (let ((bufstr "blah \nt>this\nw>who\ne>a-b"))
    (should (equal
	     (flykey-create-command-cmd-pairs bufstr)
	     '(("e" . "a-b") ("w" . "who") ("t" . "this"))))))

;; Double backslashes.
(ert-deftest flykey-backslashes-test ()
  (should
   (equal (flykey-escape-bindings '(("c" . "\\b")
				    ("d" . "\\b")
				    ("e" . "\\b")))
	  '(("e" . "\\\\b") ("d" . "\\\\b") ("c" . "\\\\b")))))

;; Escape quotes.
(ert-deftest flykey-quote-test ()
  (should
   (equal (flykey-escape-bindings '(("c" . "\"b")))
	  '(("c" . "\\\"b")))))

;; Create list of normal binding commands, differently.
(ert-deftest flykey-create-normal-binding-cmds-test ()
  (with-temp-buffer
    (insert "blah \nt=this\na=\\ant\n\n\n")
    (should
     (equal
      (flykey-create-normal-binding-cmds
       (flykey-escape-bindings
	(flykey-create-normal-cmd-pairs (buffer-string))))
      '("(local-set-key (kbd \"a\" ) (lambda () (interactive) (insert \"\\\\ant\")))"
	"(local-set-key (kbd \"t\" ) (lambda () (interactive) (insert \"this\")))")))))

;; Create list of Elisp command binding commands.
(ert-deftest flykey-create-command-binding-cmds-test ()
  (with-temp-buffer
    (insert "blah \nt>erase-buffer\nc>comment-region")
    (should
     (equal
      (flykey-create-command-binding-cmds
       (flykey-escape-bindings
	(flykey-create-command-cmd-pairs (buffer-string))))
      '("(local-set-key (kbd \"c\" ) (lambda () (interactive) (comment-region)))"
	"(local-set-key (kbd \"t\" ) (lambda () (interactive) (erase-buffer)))")))))

;; Use flykey-create-normal-binding-cmds on a command binding.
(ert-deftest flykey-use-normal-on-command-test ()
  (with-temp-buffer
    (insert "blah \nt>erase-buffer\nc>comment-region")
    (should
     (equal
      (flykey-create-normal-binding-cmds
       (flykey-escape-bindings
	(flykey-create-normal-cmd-pairs (buffer-string))))
      nil))))

;; Create list of normal binding commands.
(ert-deftest flykey-create-quoted-cmds-normal-test ()
  (with-temp-buffer
    (insert "blah \nt=this\na=\\ant\n\n\n")
    (should
     (equal
      (flykey-create-quoted-cmds (current-buffer))
      '("(local-set-key (kbd \"a\" ) (lambda () (interactive) (insert \"\\\\ant\")))"
	"(local-set-key (kbd \"t\" ) (lambda () (interactive) (insert \"this\")))")))))

;; Create list of command binding commands.
(ert-deftest flykey-create-quoted-cmds-commands-test ()
  (with-temp-buffer
    (insert "blah \nt>erase-buffer\na>comment-region\n\n\n")
    (should
     (equal
      (flykey-create-quoted-cmds (current-buffer))
      '("(local-set-key (kbd \"t\" ) (lambda () (interactive) (erase-buffer)))"
	"(local-set-key (kbd \"a\" ) (lambda () (interactive) (comment-region)))")))))

;; Create list of mixed binding commands.
(ert-deftest flykey-create-quoted-cmds-mixed-test ()
  (with-temp-buffer
    (insert "blah \nt>erase-buffer\na=\\ant\n\n\n")
    (should
     (equal
      (flykey-create-quoted-cmds (current-buffer))
      '("(local-set-key (kbd \"a\" ) (lambda () (interactive) (insert \"\\\\ant\")))"
	"(local-set-key (kbd \"t\" ) (lambda () (interactive) (erase-buffer)))")))))

;; Use FlyKey with a file in fundamental mode (no keymap).
(ert-deftest flykey-fundamental-mode-test ()
  (with-sandbox
   (kill-leftover-buffers
    (fundamental-mode)
    (set-frame-size (selected-frame) 80 48)
    (flykey))))

;; Bind Elisp commands.
(ert-deftest flykey-bind-to-command-test ()
  (with-flykey-running
   (set-buffer flykey-flybuf)
   (insert "a>erase-buffer\n")
   (set-buffer flykey-insertbuf)
   (insert "what is this")
   (funcall (local-key-binding "a")) 	; Simulate keystroke.
   (should (equal (buffer-string) ""))))

(provide 'flykey-test)
;;; flykey-test.el ends here
