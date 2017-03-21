;;; flykey.el --- A tool to insert text using on-the-fly keybindings.

;; Copyright (C) 2017 Tristan McKinney

;; Author: Tristan McKinney <natsirtguy@gmail.com>
;; Maintainer: Tristan McKinney <natsirtguy@gmail.com>
;; Version: 0.0.1
;; URL: http://github.com/rejeep/prodigy.el
;; Package-Requires: ((emacs "24"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; flykey.el is a tool to rapidly insert specialized text, like LaTeX
;; equations, into a document by changing keymappings on the fly.

;;; Code:

(require 'f)

(defconst flykey-dir
  (f-dirname (f-this-file))
  "The directory containing this file.")

(defconst flykey-sh-file
  (concat flykey-dir "/flykey.sh")
  "The shell file used to create keybindings.")

;; These three variables will correspond to the buffer-local names of the
;; original buffer from which FlyKey is run, the buffer with the keybindings,
;; and the buffer from which text is inserted.
(defvar flykey-oldbuf)
(defvar flykey-flybuf)
(defvar flykey-insertbuf)

(defun flykey-read-quoted-cmds (comquotes)
  "Read COMQUOTES, a list of quoted commands, and return a list of commands."
  (if (and comquotes (not (string= (car comquotes) "")))
      (cons (car (read-from-string (car comquotes)))
	    (flykey-read-quoted-cmds (cdr comquotes)))
    nil))

(defun flykey-eval-cmds (cmds)
  "Eval a list of commands, CMDS."
  (if cmds (progn (eval (car cmds)) (flykey-eval-cmds (cdr cmds)))
    nil))

(defun flykey-insert-buffer (toinsert buf)
  "Insert contents of buffer-or-name TOINSERT into the buffer-or-name BUF."
  (interactive)
  (with-current-buffer buf
    (insert (with-current-buffer toinsert (buffer-string)))))

(defun flykey-input-no-map ()
  "Take input from the minibuffer and insert it without using the keymap."
  (interactive)
  (insert (read-from-minibuffer "Input: ")))

(defun flykey-send-buffer-script (buf shfile)
  "Send the contents of BUF to script SHFILE, return list of strings."
  (let ((bufcontents (with-current-buffer buf (concat (buffer-string) "\n"))))
    (process-lines shfile bufcontents)))

(defun flykey-make-map-cmds (flybuf)
  "Make the list of keymap commands from a buffer FLYBUF in the .flyk format."
  (flykey-read-quoted-cmds
   (flykey-send-buffer-script flybuf flykey-sh-file)))

(defun flykey-add-bindings (flybuf buf)
  "Add the keybindings described in FLYBUF to the keymap for BUF."
  (with-current-buffer buf
    (flykey-eval-cmds (flykey-make-map-cmds flybuf))))

(defun flykey-open-flyk ()
  "Return a buffer with the flykey file for the current major mode."
  (let ((flyfile
	 (concat flykey-dir "/flyks/" (format "%s" major-mode) ".flyk")))
    (if (file-exists-p flyfile) (find-file-noselect flyfile)
      ;; Otherwise, make a new file whose first line is #major-mode.
      (let ((flybuf (find-file-noselect flyfile))
	    (oldmajor major-mode))
	(with-current-buffer flybuf
	  (insert (concat "#" (format "%s" oldmajor) "\n")))
	flybuf))))

(defun flykey-open-windows (flybuf insertbuf)
  "Split current window and display FLYBUF and INSERTBUF."
  (set-window-buffer (split-window-below) insertbuf)
  (with-selected-window (get-buffer-window insertbuf)
    (set-window-buffer (split-window-below) flybuf)))

(defun flykey-set-up-buffers (flybuf insertbuf)
  "Open the windows for FLYBUF and INSERTBUF and apply the keybindings."
  (let ((pmap (copy-keymap (current-local-map))))
    (with-current-buffer insertbuf
      (use-local-map pmap))
    (flykey-open-windows flybuf insertbuf)
    ;; Add the initial bindings.
    (flykey-add-bindings flybuf insertbuf)
    ;; Add some keybindings to insert insertbuf, erase it,
    ;; or take input without the keymap from the minibuffer.
    (with-current-buffer insertbuf
      (local-set-key (kbd "C-c i") 'flykey-insert-and-close)
      (local-set-key (kbd "H-i") 'flykey-insert-and-close)
      (local-set-key (kbd "C-c c") 'flykey-clear-insertbuf)
      (local-set-key (kbd "H-c") 'flykey-clear-insertbuf)
      (local-set-key (kbd "C-c w") 'flykey-input-no-map)
      (local-set-key (kbd "H-w") 'flykey-input-no-map))
    (with-current-buffer flybuf
      (local-set-key (kbd "C-c i") 'flykey-insert-and-close)
      (local-set-key (kbd "H-i") 'flykey-insert-and-close)
      (local-set-key (kbd "C-c c") 'flykey-clear-insertbuf)
      (local-set-key (kbd "H-c") 'flykey-clear-insertbuf))))

(defun flykey-set-local-vars (buf oldbuf flybuf insertbuf)
  "In BUF, set local variables OLDBUF, FLYBUF, and INSERTBUF."
  (with-current-buffer buf
    (make-local-variable 'flykey-oldbuf)
    (setq flykey-oldbuf oldbuf)
    (make-local-variable 'flykey-flybuf)
    (setq flykey-flybuf flybuf)
    (make-local-variable 'flykey-insertbuf)
    (setq flykey-insertbuf insertbuf)))

(defun flykey ()
  "Run flykey."
  (interactive)
  (let ((oldbuf (current-buffer))
	(flybuf (flykey-open-flyk))
	(insertbuf
	 (get-buffer-create
	  (concat "*flykey-" (buffer-name (current-buffer)) "*"))))
    (flykey-set-up-buffers flybuf insertbuf)
    (select-window (get-buffer-window flybuf))
    (goto-char (point-max))
    (select-window (get-buffer-window insertbuf))
    (flykey-set-local-vars oldbuf oldbuf flybuf insertbuf)
    (flykey-set-local-vars flybuf oldbuf flybuf insertbuf)
    (flykey-set-local-vars insertbuf oldbuf flybuf insertbuf)
    ;; Make a local hook to refresh keybindings when the window is changed.
    (with-current-buffer insertbuf
      (add-hook 'buffer-list-update-hook 'flykey-reload-map nil t))
    (with-current-buffer flybuf
      (add-hook 'buffer-list-update-hook 'flykey-reload-map nil t))))

(defun flykey-close-windows ()
  "Close the windows associated with flybuf and insertbuf."
  (select-window (get-buffer-window flykey-oldbuf))
  (delete-window (get-buffer-window flykey-flybuf))
  (delete-window (get-buffer-window flykey-insertbuf)))

(defun flykey-insert-and-close ()
  "Insert insertbuf and close windows."
  (interactive)
  (flykey-insert-buffer flykey-insertbuf flykey-oldbuf)
  (flykey-close-windows))

(defun flykey-clear-insertbuf ()
  "Erase the contents of insertbuf."
  (interactive)
  (with-current-buffer flykey-insertbuf
    (erase-buffer)))

(defun flykey-reload-map ()
  "Reload the keymap from flybuf.
Because 'process-lines' changes the buffer list, and we want to use this as part
of a hook which runs when the buffer list changes, we must remove it from the
'buffer-list-update-hook' before adding the bindings, then restore the state."
  (if (buffer-live-p  flykey-insertbuf)
      (with-current-buffer flykey-insertbuf
	(remove-hook 'buffer-list-update-hook 'flykey-reload-map t)))
  (if (buffer-live-p  flykey-flybuf)
      (with-current-buffer flykey-insertbuf
	(remove-hook 'buffer-list-update-hook 'flykey-reload-map t)))
  (if (and
       (buffer-live-p flykey-flybuf)
       (buffer-live-p flykey-insertbuf)
       (buffer-modified-p flykey-flybuf))
      (progn
	(flykey-redo-bindings)
	(with-current-buffer flykey-flybuf
	  (set-buffer-modified-p nil))))
  (if (buffer-live-p  flykey-insertbuf)
      (with-current-buffer flykey-flybuf
	(remove-hook 'buffer-list-update-hook 'flykey-reload-map t)))
  (if (buffer-live-p  flykey-flybuf)
      (with-current-buffer flykey-insertbuf
	(add-hook 'buffer-list-update-hook 'flykey-reload-map nil t))))

(defun flykey-redo-bindings ()
  "Remake the keymap."
  (let ((pmap
	 (with-current-buffer flykey-oldbuf
	   (copy-keymap (current-local-map)))))
    (with-current-buffer flykey-insertbuf
      (use-local-map pmap))
    ;; Add the initial bindings.
    (flykey-add-bindings flykey-flybuf flykey-insertbuf)
    ;; Add some keybindings to insert insertbuf, erase it,
    ;; or take input without the keymap from the minibuffer.
    (with-current-buffer flykey-insertbuf
      (local-set-key (kbd "C-c i") 'flykey-insert-and-close)
      (local-set-key (kbd "H-i") 'flykey-insert-and-close)
      (local-set-key (kbd "C-c c") 'flykey-clear-insertbuf)
      (local-set-key (kbd "H-c") 'flykey-clear-insertbuf)
      (local-set-key (kbd "C-c w") 'flykey-input-no-map)
      (local-set-key (kbd "H-w") 'flykey-input-no-map))
    (with-current-buffer flykey-flybuf
      (local-set-key (kbd "C-c i") 'flykey-insert-and-close)
      (local-set-key (kbd "H-i") 'flykey-insert-and-close)
      (local-set-key (kbd "C-c c") 'flykey-clear-insertbuf)
      (local-set-key (kbd "H-c") 'flykey-clear-insertbuf))))

(provide 'flykey)
;;; flykey.el ends here
