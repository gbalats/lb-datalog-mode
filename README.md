lb-datalog-mode
===============

An Emacs mode for LogicBlox Datalog.

### Author
[George Balatsouras](mailto:gbalats@di.uoa.gr)

### License
MIT license (see `LICENSE`)


Installation
------------

### Install [Cask](https://github.com/cask/cask):

To install `Cask` simply run:

    curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python

or:

    make setup


### Install `lb-datalog-mode`:

Run the following to compile, create a package (tarball), and install
it to `emacs`:

    make
    make install

Finallly, add the following into your `~/.emacs`:

    (require 'lb-datalog-mode)


Keyboard Shortcuts
------------------

### lb-datalog-mode

Keyboard shortcut        | Description
-------------------------|-------------------------------
<kbd>M-a</kbd>           | Move forward by one atom (either functional or plain).
<kbd>M-e</kbd>           | Move backwards by one atom  (either functional or plain).
<kbd>C-M-a</kbd>         | Move forward by one clause.
<kbd>C-M-e</kbd>         | Move backwards by one clause.
<kbd>C-:</kbd>           | Rename local variable (at clause scope).
<kbd>C-c C-f</kbd>       | Insert a functional atom at point. This requires [yasnippet][yasnippet] to be installed.


[yasnippet]: https://github.com/capitaomorte/yasnippet/
