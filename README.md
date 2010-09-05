js2-mode
========

An improved JavaScript mode for GNU Emacs. Forked from <http://code.google.com/p/js2-mode/>.

Install
=======

    $ git clone git://github.com/mooz/js2-mode.git
    $ cd js2-mode
    $ emacs --batch --eval '(byte-compile-file "js2-mode.el")'

Then, place js2-mode.elc into your site-lisp directory.

In you emacs config:

    (autoload 'js2-mode "js2-mode" nil t)
    (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

See <http://code.google.com/p/js2-mode/wiki/InstallationInstructions> for details.

Differences between original js2-mode.el
========================================

Support for abbreviated destructuring assignments
-------------------------------------------------

    let {a, b}       = {a: 10, b: 20}; // Abbreviated   (Not supported in original js2-mode.el)
    let {a: a, b: b} = {a: 10, b: 20}; // Same as above (Supported in original js2-mode.el)

    (function ({responseText}) { /* */ })(xhr); // As the argument of function

Support for expression closure in property value
------------------------------------------------

    let worker = {
        get age() 20,
        get sex() "male",
        fire: function () _fire()
    };

Supported more popular indentation style
----------------------------------------

When js2-consistent-level-indent-inner-bracket-p is non-nil
    
    [foo, bar, baz].forEach(function (v) {
        if (validate(v))
            process(v);
    });
    
    [a, b, c].some(function (v) {
        return validate(v);
    });

When js2-consistent-level-indent-inner-bracket-p is nil
(Same as original js2-mode's indentation)

    [foo, bar, baz].forEach(function (v) {
                                if (validate(v))
                                    process(v);
                            });
    
    [a, b, c].some(function (v) {
                       return validate(v);
                   });

Fixed the odd indentation of "else if" with no braces
-----------------------------------------------------

In original js2-mode.el,

    if (foo)
        return foo;
    else if (bar)
    return bar;      // here

In this js2-mode.el,

    if (foo)
        return foo;
    else if (bar)
        return bar;  // fixed

Bugs
====

If you find problems, please report them to <http://github.com/mooz/js2-mode/issues>.
