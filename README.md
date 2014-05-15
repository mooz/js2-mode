About [![Build Status](https://travis-ci.org/mooz/js2-mode.png?branch=master)](https://travis-ci.org/mooz/js2-mode)
======

Improved JavaScript editing mode for GNU Emacs ([description here](http://elpa.gnu.org/packages/js2-mode.html)).

For some of the latest changes, see [latest user-visible changes](https://github.com/mooz/js2-mode/wiki/Latest-user-visible-changes).

Installation
======

The stable versions are hosted at [GNU ELPA](http://elpa.gnu.org/)
(<kbd>M-x list-packages</kbd>).

You can also install the latest development version from
[Melpa](http://melpa.milkbox.net/#installing).

#### Setting js2-mode as the default major mode for javascript files

After installing, you may neet to configure Emacs in order to use `js2-mode` as the default major mode for javascript files. You can do that by adding the following line to your `.emacs` file:

    (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

Emacs 22 and 23
===============

This version requires Emacs 24. For a backward compatible version,
check out the branch
[emacs23](https://github.com/mooz/js2-mode/tree/emacs23).

Bugs
====

* See broken syntax highlighting and timer errors? Recently upgraded
Emacs from version 24.2 or earlier?

* Try
[reinstalling or byte-recompiling](https://github.com/mooz/js2-mode/issues/72)
the package.

Please report problems at <http://github.com/mooz/js2-mode/issues>.

See Also
======

Some third-party modes that use the generated syntax tree:

* [js2-refactor](https://github.com/magnars/js2-refactor.el)
* [skewer-mode](https://github.com/skeeto/skewer-mode)
