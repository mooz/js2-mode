Description
======

An improved JavaScript mode for GNU Emacs. Forked from <http://code.google.com/p/js2-mode/>.

For some of the latest changes, see [latest user-visible changes](https://github.com/mooz/js2-mode/wiki/Latest-user-visible-changes).

Installation
======

    $ git clone git://github.com/mooz/js2-mode.git
    $ cd js2-mode
    $ git checkout emacs23
    $ emacs --batch -f batch-byte-compile js2-mode.el

Then put js2-mode.elc into your site-lisp directory.

In your emacs config:

    (autoload 'js2-mode "js2-mode" nil t)
    (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

See <http://code.google.com/p/js2-mode/wiki/InstallationInstructions> for
additional details.

Emacs 24
========

This is the backward compatible version, for Emacs 22 and 23.

For Emacs 24 and newer, the [master](https://github.com/mooz/js2-mode/tree/master) branch is recommended.

Bugs
====

If you find problems, please report them at <http://github.com/mooz/js2-mode/issues>.

See Also
======

Some third-party modes that use the generated syntax tree:

* [js2-refactor](https://github.com/magnars/js2-refactor.el)
* [skewer-mode](https://github.com/skeeto/skewer-mode)
