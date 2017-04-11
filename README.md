# FlyKey: On-the-Fly Keybindings for Emacs

FlyKey is a tool to improve efficiency in editing repetitive documents
in Emacs by allowing you to insert text with a custom set of
keybindings created on the fly.

# Usage

Start FlyKey with `M-x flykey` (I have this bound to `C-c k`). Two
windows will be displayed below your current window: the "insertbuf"
on top and the "flybuf" on the bottom. The first line in the flybuf
should specify the type of file that is being edited. Subsequent lines
are keybindings with the following syntax:

`keys=binding`

These keybindings will then work in the insertbuf in addition to the
keybindings which are inherited from the buffer you were editing when
you invoked FlyKey. At this point, you can write something in the
insertbuf using the modified keybindings, then insert it into the
original buffer at the point using `C-c i`. `C-c w` will open a
minibuffer which will allow you to insert text without the keybindings
you have specified. `C-c c` clears the insertbuf. You can also save
your flybuf modifications for future use when editing documents with
the same major mode. Editing the flybuf will update the keybindings.

# Installation
Put `flykey.el` somewhere in your `load-path`.

# Built Using
* [Cask](https://cask.readthedocs.io/en/latest/)
* [ert-runner.el](https://github.com/rejeep/ert-runner.el)

## To do:
* Add customization options, such as whether to hide the buffers after
text is inserted, create a flyk file for each major mode or each file,
etc.
* Add error handling.
* Add functionality for binding keys to commands (maybe use > instead of =).
* Add an Info file for FlyKey.
