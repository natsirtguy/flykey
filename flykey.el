;;; flykey.el --- A tool to insert text using on-the-fly keybindings.

;; Copyright (C) 2017 Tristan McKinney

;; Author: Tristan McKinney <johan.rejeep@gmail.com>
;; Maintainer: Tristan McKinney <johan.rejeep@gmail.com>
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
;; flykey.el is  tool to rapidly insert specialized text, like LaTeX
;; equations, into a document by changing keymappings on the fly.

;;; Code:

;; Major parts: open buffers, set up flykeyfile, create keymap,
;; use keymap in editing buffer, insert editing buffer contents,
;; close editing buffer and save flykeyfile.

(require 'f)

(defconst flykey-dir
  (f-dirname (f-this-file))
  "The directory containing this file.")

(defconst flykey-sh-file
  (concat flykey-dir "/flykey.sh")
  "The shell file used to create keybindings.")

(defun flykey-read-quoted-cmds (comquotes)
  "Read COMQUOTES, a list of quoted commands, and return a list of commands."
  (if comquotes (cons (car (read-from-string (car comquotes)))
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

(defun flykey-send-buffer-script (buf shfile)
  "Send the contets of buffer-or-name BUF to script SHFILE and return the results as a string."
  (let ((bufcontents (with-current-buffer buf (buffer-string))))
      (shell-command-to-string
       (concat "echo \"\\\"" bufcontents "\\\"\" | xargs " shfile))))

(defun flykey-make-map (f1 p1)
  "Run awk on file F1 and produce keymap inherited from P1."
  (let (
	
	;; Get the keys that will be replaced.
	(keys (split-string
	       (shell-command-to-string
		(concat "awk -F'=' 'NR>1 {print $1}' " f1)) "\n"))
	;; Get the new mappings for the keys.
	(bindings (split-string
		   (shell-command-to-string
		    (concat "awk -F'=' 'NR>1 {print $2}' " f1)) "\n"))
	;; New keymap to make.
	(newmap (make-sparse-keymap))
	)
    ;; Set up the keymap recursively.
    (flykey-recurse-map keys bindings newmap)
        
    ;; Add keybinding for flykey-insert-at-point.
    (define-key newmap (kbd "C-c i") 'flykey-insert-at-point)
    
    ;; Make a new keymap and inherit from p1.
    (make-composed-keymap newmap p1)
    ) 					; end let
  )

(defun flykey-open-flyk ()
  "Return a buffer with the flykey file for the given major mode."
  (let ((flyfile (concat "~/.emacs.d/flykey/"
			 (format "%s" major-mode)
			 ".flyk")))
    (if (file-exists-p flyfile)
	;; If it flykey file already exists, open it and return the buffer.
	(find-file-noselect flyfile)
      ;; Otherwise, make a new file whose first line is #major-mode.
      (let ((flybuf (find-file-noselect flyfile))
	    (oldmajor major-mode))
	(with-current-buffer flybuf
	  (insert (concat "#" (format "%s" oldmajor))))
	flybuf
	)
      )					; end if
    )
  )

;; (defun flykey-remake-map ()
;;   "Save flyfile and remake the flykey map."
;;   (with-selected-window
;;       ;; Get the window for the flykey file and save it.
;;       (get-buffer-window (get-file-buffer flykey-fkfile))
;;     (save-selected-window))
;;   (use-local-map
;;    (flykey-make-map flykey-fkfile flykey-fkpmap))
;;   )

(defun flykey-run ()
  "Run flykey by opening the flykey file buffer and a temporary buffer.
The temporary buffer uses the on-the-fly keybindings from the flykey file."
  (interactive)
  (let (
	;; The original buffer.
	(oldbuf (current-buffer))
	;; The buffer visiting the flykey file.
	(flyfilebuf (flykey-open-flyk))
	;; The parent keymap.
	(pmap (copy-keymap (current-local-map)))
	;; The temporary buffer.
	(flybuf (generate-new-buffer "*flykey*"))
	)
    ;; Open two more windows below the current one.
    (set-window-buffer (split-window-below) flybuf)
    (with-selected-window (get-buffer-window flybuf)
      (set-window-buffer (split-window-below) flyfilebuf))

    ;; Switch to flybuf and store the parent keymap, original file,
    ;; and flykey file buffer as local variables.
    (select-window (get-buffer-window flybuf))
    (make-local-variable 'flykey-fkold)
    (defconst flykey-fkold oldbuf
      "The buffer from which flykey was called. Buffer-local.")
    (make-local-variable 'flykey-fkfile)
    (defconst flykey-fkfile (buffer-file-name flyfilebuf)
      "The flykey file name. Buffer-local.")
    (make-local-variable 'flykey-fkpmap)
    (defconst flykey-fkpmap pmap
      "The parent keymap that is being inherited. Buffer-local.")

    ;; Add a hook to remake the map every time we modify the flykey file.
    ;; (remove-hook 'buffer-list-update-hook 'flykey-remake-map); nil t)
        
    (use-local-map (flykey-make-map flykey-fkfile flykey-fkpmap))
    )				; end let
  )

(provide 'flykey)
;;; flykey.el ends here
