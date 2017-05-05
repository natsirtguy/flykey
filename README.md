# FlyKey: On-the-Fly Keybindings for Emacs

FlyKey is a tool to improve efficiency in editing repetitive documents
in Emacs by allowing you to insert text with a custom set of
keybindings created on the fly.

# Usage

Start FlyKey: `M-x flykey`

Two windows will be displayed below your current window. The bottom
buffer displays editable bindings for the middle buffer in the form

`keys=binding`  
`keys>command`

The first line is an example of a normal keybinding: typing "keys"
will insert "binding" instead. The second line is an example of a
command binding: typing "keys" will execute an Elisp command.

Insert text from middle buffer: `C-c i`  
Type word in middle buffer without bindings: `C-c w`  
Clear middle buffer: `C-c c`  
Quit FlyKey: `C-c q`

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
* Add an Info file for FlyKey.
* Prevent FlyKey from stopping when a bad keybinding is used.
