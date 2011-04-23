js2-mode
========

An improved JavaScript mode for GNU Emacs. Forked from <http://code.google.com/p/js2-mode/>.

Install
=======

    $ git clone git://github.com/mooz/js2-mode.git
    $ cd js2-mode
    $ emacs --batch -f batch-byte-compile js2-mode.el

Then, place js2-mode.elc into your site-lisp directory.

In you emacs config:

    (autoload 'js2-mode "js2-mode" nil t)
    (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

See <http://code.google.com/p/js2-mode/wiki/InstallationInstructions> for details.

Differences between original js2-mode.el
========================================

Popular indentation style
-------------------------

When `js2-consistent-level-indent-inner-bracket-p` is non-nil
    
    [foo, bar, baz].forEach(function (v) {
        if (validate(v))
            process(v);
    });
    
    [a, b, c].some(function (v) {
        return validate(v);
    });

When `js2-consistent-level-indent-inner-bracket-p` is nil
(Same as original js2-mode's indentation)

    [foo, bar, baz].forEach(function (v) {
                                if (validate(v))
                                    process(v);
                            });
    
    [a, b, c].some(function (v) {
                       return validate(v);
                   });

Pretty multi-line variable declaration
--------------------------------------

In original js2-mode.el,

    var foo = 10,
    bar = 20,
    baz = 30;

In this js2-mode.el, when the value `js2-pretty-multiline-decl-indentation-p` is non-nil,

    var foo = 10,
        bar = 20,
        baz = 30;

Abbreviated destructuring assignments
-------------------------------------

    let {a, b}       = {a: 10, b: 20}; // Abbreviated   (Not supported in original js2-mode.el)
    let {a: a, b: b} = {a: 10, b: 20}; // Same as above (Supported in original js2-mode.el)

    (function ({responseText}) { /* */ })(xhr); // As the argument of function

    for (let [k, { name, age }] in Iterator(obj)) // nested
        print(k, name, age);

Expression closure in property value
------------------------------------

    let worker = {
        get age() 20,
        get sex() "male",
        fire: function () _fire()
    };

Fix for odd indentation of "else if" with no braces
---------------------------------------------------

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

Fixes in Imenu support
----------------------

Supports element-get form:

    foo["bar"] = function() {};
    foo[647] = function() {};

Proper position for functions in nested object literals:

    foo = {
        bar: function() {}, // ok in original
        baz: {
             boop: function() {} // fixed here
        }
    }

Imenu support for function nesting
----------------------------------

Supports one level of nesting:

    function foo() {
        function bar() { // shown as foo.bar
            function baz() {} // hidden
        }
    }

Top-level function can be anonymous wrapper:

    (function() {
        var foo = function() {}; // shown as foo
    })();

Examples of output:

* [Underscore.js](https://github.com/documentcloud/underscore/blob/master/underscore.js)
-> <https://gist.github.com/824262>
* [Backbone.js](https://github.com/documentcloud/backbone/blob/master/backbone.js)
-> <https://gist.github.com/824260>

No support for library-specific extension methods like _.extend.

Highlights undeclared/external variables
----------------------------------------

Original mode highlights them only on the left side of assignments:

    var house;
    hose = new House(); // highlights "hose"

Here they are highlighted in all expressions:
    
    function feed(fishes, food) {
        for each (var fish in fshes) { // highlights "fshes"
            food.feed(fsh); // highlights "fsh"
        }
        hood.discard(); // highlights "hood"
    }

Destructuring assignments and array comprehensions (JS 1.7) are supported:

    let three, [one, two] = [1, 2];
    thee = one + two; // highlights "thee" 

    function revenue(goods) {
        // highlights "coast"
        return [price - coast for each ({price, cost} in goods)].reduce(add);
    }
    
Bugs
====

If you find problems, please report them to <http://github.com/mooz/js2-mode/issues>.
