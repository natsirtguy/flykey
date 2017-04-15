# FlyKey: On-the-Fly Keybindings for Emacs

FlyKey is a tool to improve efficiency in editing repetitive documents
in Emacs by allowing you to insert text with a custom set of
keybindings created on the fly.

# Usage

Start FlyKey: `M-x flykey`
Two windows will be displayed below your current window. The bottom
buffer displays editable bindings for the middle buffer in the form

`keys=binding`

Insert text from middle buffer: `C-c i`
Type word in middle buffer without bindings: `C-c w`
Clear middle buffer: `C-c c`

You can save the middle buffer for future use when editing documents
with the same major mode.

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
