# Chapter 2: Writing your first Emacs Lisp Command

I think the best way to learn a programming language is by using it to build
something, and it's even better if you build something practical and useful. The
goal of this book is to show you how to build cool things right away and
demonstrate and explain Emacs Lisp concepts gradually at the same time. This
way, you'll learn as you go. You'll build cool things and have a complete
understanding of how they work.

In this chapter, we're going to dive right in and build a command to render a
preview of a Markdown file directly in Emacs. You can use this to preview a file
as you edit it -- in fact, I'm using it right now as I'm writing this!

Once we've built our command, I'll show you how to have it run automatically
whenever you save a Markdown file.

Before starting this chapter, you'll want to install
[`pandoc`](https://pandoc.org/) which we will use for rendering Markdown files,
Emacs 25 or newer, and the
[`markdown-mode`](https://github.com/jrblevin/markdown-mode) package.

### Evaluating forms and `ielm`

Before we get started, I want to briefly mention how to follow along with the
code in this chapter. For the functions we're working on, you'll probably want to
add them to your `~/.emacs.d/init.el` file. You can evaluate function
definitions by moving the cursor to some point in the function definition and
then doing `C-M-x`. You can also evaluate the form immediately preceding your
cursor with the key binding `C-x C-e`. Give them a try!

A big part of Lisp development is using a REPL. `ielm` is the Emacs Lisp REPL.
You can start it with `M-x ielm` and use it to evaluate Emacs Lisp forms. This
is especially useful for evaluating the smaller bits of code like `(+ 1 2)` that
eventually make it in to larger functions.

# Writing a `preview-markdown` command

Without further ado, let's start writing our command.

### Defining a function

Let's start off with an empty function declaration:

```lisp
(defun cam/preview-markdown ())
```

If you're completely new to Emacs Lisp, especially if you come from a Clojure
background, you might think the `cam/` portion of the name of the function has
some sort of special significance. It doesn't! Emacs Lisp doesn't have the
notion of separate namespaces that other languages have, so every function and
variable name is global; like Objective-C, it's a best-practice to prefix all
function and variable names with something that makes it clear they're part of a
particular package. Usually you'll see names prefixed with `<package-name>-`,
for example `helm-mark-ring`, but `<package-name>/` and `<package-name>:` are
reasonably common conventions as well. The `/` character itself has no special
significance in symbol names. I prefer prefixing all of my own functions and
macros with `cam/`, so I'll use that in the code in this chapter.

### Making it a command

Now this function itself doesn't do anything, and you can't invoke it with
`M-x`. Emacs Lisp functions can either be invoked programmatically (e.g. by
other Emacs Lisp functions, or the `ielm` REPL, or via `M-:`) or *interactively*
(via `M-x` or a keybinding). A function that be invoked interactively is known
as a *command*.

To make a function a command, you add an `interactive` declaration, which always
goes right after the argument list (or docstring, if you have one):

```lisp
(defun cam/preview-markdown ()
  (interactive))
```

Now the function can be invoked interactively. Try it out!
`M-x cam/preview-markdown`. Cool! But it still doesn't do
anything.

### Logging Messages

Let's start by having our command log a message. `message` is the Emacs Lisp
equivalent of `printf`, so this is sort of a "Hello World" example. Exciting!
We'll add a call `message` in the function body:

```lisp
(defun cam/preview-markdown ()
  (interactive)
  (message "Rendering Markdown preview of %s" buffer-file-name))
```

The variable `buffer-file-name` is bound to the string name of the file associated with
the current buffer (i.e., the one from which you invoked the command). Create a
markdown file and switch to it in Emacs, then run `M-x cam/preview-markdown` again.

You'll see something like

```
Rendering Markdown preview of /home/cam/README.markdown
```

appear in the minibuffer, and it will be appended to the `*Messages*` buffer as
well. When writing any code, it's important to be able to debug it, and until we
talk about using the Emacs Lisp debugger being able to print stuff out is a good first step.

### Calling external commands

Our plan is to:

1.  Take the markdown content in the current buffer and render it as HTML to a buffer named `*Preview Markdown Output*`
2.  Parse the HTML in `*Preview Markdown Output*`
3.  Clear `*Preview Markdown Output*` and then render the pretty GUI widgets
    like you saw in the GIF at the start of this chapter.

So let's add a command to render the Markdown to HTML using the external `pandoc` command:

```lisp
(defun cam/preview-markdown ()
  (interactive)
  (message "Rendering Markdown preview of %s" buffer-file-name)
  (shell-command-on-region (point-min) (point-max) "pandoc -f gfm" "*Preview Markdown Output*"))
```

`shell-command-on-region` runs a command (`pandoc -f gfm`) on a *region*. The
simplest explanation of a region is that it's all the text between two positions
in a buffer -- if you think of the current buffer as one giant string, then a
region would be a substring.

Since we want to render the entire buffer as HTML rather than just part of it, we want
our region to be the entire buffer. The functions `point-min` and `point-max`
get the minimum possible position and maximum possible position in the current
buffer respectively; together those positions represent the region we're operating on.

The final argument to `shell-command-on-region`, `"*Preview Markdown Output*"`,
tells it the name of the buffer to write the output of the shell command to
(creating it if it doesn't already exist). If the buffer already exists,
`shell-command-on-region` will clear any existing contents of the buffer before
writing the command output.

Emacs Lisp buffers used for internally or for other special purposes are conventionally given names with
asterisks (affectionately known as *earmuffs*), but you could name the buffer
anything you want.

Go ahead and give it a try in your Markdown file buffer, and you'll see a buffer
called `*Preview Markdown Output*` pop up with the HTML output.

Before moving on, let's take a quick break to discuss some essentials that
you'll really want to know when writing Emacs Lisp.

### Looking up documentation

To look up documentation for a function, you can use `C-h f <name-of-function>`.
If you want to write Emacs Lisp, commit that keybinding to memory right away --
you'll be using it all the time. Try using it to learn more about one of the
functions we're using.

Similarly, `C-h v` opens documentation for a variable, and `C-h k` will tell you
the function that gets run for a given keybinding. Write these down on a Post-It
and stick it to your computer until they are burned into your brain.

### Evaluating Emacs Lisp from the minibuffer with `M-:`

Another keybinding you'll want to commit to memory is `M-:`, which allows you to
evaluate an Emacs Lisp form from the minibuffer. The code is evaluated in the
context of the current buffer. If you want to look at certain
values of variables or test how certain code works when evaluated in a certain
buffer, `M-:` is one of the easiest ways to do so.

Try it from you markdown buffer -- `M-:` and then type or paste
`buffer-file-name` and hit return. The output is visible in the minibuffer and
`*Messages*`. Try it with a different buffer -- you'll see a different result.

### Viewing the HTML

We can use the built-in Simple HTML Renderer (SHR) to render HTML directly
in Emacs. Change our function to the following and give this a try:

```lisp
(defun cam/preview-markdown ()
  (interactive)
  ;; [1] bind the value of buffer-file-name to local variable filename
  (let ((filename buffer-file-name))
    (message "Rendering Markdown preview of %s" filename)
    (shell-command-on-region (point-min) (point-max) "pandoc -f gfm" "*Preview Markdown Output*")
    ;; [2] switch to a different window and bring up the *Preview Markdown Output* buffer
    (switch-to-buffer-other-window "*Preview Markdown Output*")
    ;; [3] Parse the HTML source in *Preview Markdown Output*, and store it in document
    (let ((document (libxml-parse-html-region (point-min) (point-max))))
      ;; [4] Clear the contents of *Preview Markdown Output*
      (erase-buffer)
      ;; [5] Render the parsed HTML into *Preview Markdown Output*
      (shr-insert-document document)
      ;; [6] Move back to the beginning of the buffer
      (goto-char (point-min)))))
```

We're doing a few new things here, and I'll explain each in turn:

#### Binding local variables with `let`

```lisp
(let ((filename buffer-file-name))
  ...)
```

Not all buffers are associated with files. `*Preview Markdown Output*` isn't
actually associated with a file, so `buffer-file-name` will be `nil` if we
evaluate it with `*Preview Markdown Output*` as the current buffer. To work
around this, we can evaluate `buffer-file-name` *before* we switch buffers and
save it for later as the local variable `filename`. `(let ...)` is used to
introduce local bindings.

```lisp
;; x = 100
(let ((x 100))
  (+ x x))
;; -> 200
```

You might be wondering why you need two sets of parentheses to set `x` in the
example above. The syntax for `let` is

```lisp
(let bindings
  ...)
```

where `bindings` is a list of bindings of the form `(symbol value)`, e.g. `(x 100)`.
This means you can pass in multiple binding forms to bind several local
variables at once:

```lisp
;; x, y = 100, 200
(let ((x 100)
      (y 200))
  (+ x y))
;; -> 300
```

Bindings can also be of the form `symbol`, which is just shorthand for `(symbol nil)`:

```lisp
;; x = nil
(let (x)
  x)
;; -> nil
```

This is sort of like declaring a variable in an Algol-family language, e.g.

```javascript
// JavaScript
var x;
```

You'd probably do this with the intention of (maybe) giving that variable a
value at some later point, e.g.

```lisp
;; x = nil
(let (x)
  ;; x = 100
  (setq x 100)
  x)
;; -> 100
```

You can use `setq` to set the value of a variable. `setq` is discussed more at
length later in the chapter. Note that Emacs Lisp is in many ways a traditional
imperative language. Emacs Lisp *is* a functional language in the sense that
functions are first-class objects that can be passed around as parameters and
returned. But it's not a functional language in the same pure function/immutable
value sense that Clojure or Haskell are.

There's one more thing you should know about `let`: bindings are done
independently of one another, as if they were done in parallel.

```lisp
(let ((x 100)
      (y 200))
  (+ x y))
```

is equivalent to the JavaScript

```javascript
// JavaScript
[x, y] = [100, 200]
```

So something like this doesn't work:

```lisp
(let ((x 100)
      (y (+ x x)))
  y)
;; Error: x is not bound yet
```

Many times you do want to refer to the results of one binding form in a subsequent one. In this case, you can use `let*`,
which binds things sequentially rather than in parallel:

```lisp
(let* ((x 100)
       (y (+ x x)))
  y)
;; -> 200
```

This is the equivalent of the JavaScript

```javascript
// JavaScript
x = 100;
y = x + x;
```

#### Switching to a different window

```lisp
(switch-to-buffer-other-window "*Preview Markdown Output*")
```

It would be annoying to replace the window with your Markdown file with
`*Preview Markdown Output*`, so it's a nicer experience to show it in a window
you're not currently using. If `*Preview Markdown Output*` is already visible in
a different window, it will switch to that window; otherwise it will pick
another window (creating one if needed) and then switch to the `*Preview
Markdown Output*` buffer in that window.

There are a lot of ways to create and split windows in Emacs. You might decide
that you'd rather have `*Preview Markdown Output*` open a new window rather than
replace the contents of an existing one; you can use functions like
`split-window-sensibly` to split the current window instead. A full discussion
of other options is outside the scope of this article, but hopefully I can
include more examples in the future.

#### Parsing the HTML source

```lisp
(libxml-parse-html-region (point-min) (point-max))
```

Before we can render the HTML, we have to parse it. `libxml-parse-html-region`
parses HTML in the region. Again, we're using `point-min` and `point-max` to
tell it to parse the entire buffer; we're saving the output of that function as
`document`.

#### Clearing the contents of `*Preview Markdown Output*`

```lisp
(erase-buffer)
```

We're going to put the rendered HTML back into `*Preview Markdown Output*`, so
we need to clear out the HTML source first. `(erase-buffer)` clears the contents
of the current buffer.

#### Render the parsed HTML document with SHR

```lisp
(shr-insert-document document)
```

Renders the parsed HTML document in the current buffer.

#### Moving back to the beginning of the buffer

```lisp
(goto-char (point-min))
```

At this point, the cursor will be all the way at the end of the buffer. It's
nicer behavior if our preview shows us the beginning of the file rather than the
end. `goto-char` moves the cursor, and we're moving it to the beginning of the
buffer, again using `point-min` to determine the beginning.

### Viewing Source

A side note about how I figured out which functions to use to parse and render
HTML: SHR isn't terribly well documented. I wasn't sure how to get it to do what
I wanted until I poked around the source for EWW (the Emacs Web "Wowser").

I strongly recommend you install a package like
[`elisp-slime-nav`](https://github.com/purcell/elisp-slime-nav) that makes it
easy to jump to the source code for functions. When editing Emacs Lisp,
`elisp-slime-nav` binds `M-.` to jump to the source of a function or variable;
if your cursor is over a symbol, it will jump straight to its definition.

# Improving the `preview-markdown` command

### Rendering relative to the current directory

Now that the meat and potatoes of our command are finished it's time to polish
it up and fix some of the annoyances. One issue with the preview is that links
and images with paths relative to the markdown document don't work correctly in
our preview. We can fix this by prepending the parsed `document` with a another
list that contains information as the base URL.

```lisp
(defun cam/preview-markdown ()
  (interactive)
  (let ((filename buffer-file-name))
    (message "Rendering Markdown preview of %s" filename)
    (shell-command-on-region (point-min) (point-max) "pandoc -f gfm" "*Preview Markdown Output*")
    (save-selected-window
      (switch-to-buffer-other-window "*Preview Markdown Output*")
      (let ((document (libxml-parse-html-region (point) (point-max)))
            ;; [1] Create the base URL
            (url (concat "file://" filename)))
        (erase-buffer)
        ;; [2] wrap `document` in a <base> element
        (shr-insert-document `(base ((href . ,url)) ,document))
        (goto-char (point-min))))))
```

First, we need to create a `file://` URL that SHR can use as the base URL for
images or other elements with a relative path. If our original file is
`/home/cam/README.markdown`, our URL should be
`file:///home/cam/README.markdown`. We can use `concat` to concatenate multiple
arguments together into a single string:

```lisp
(concat "file://" "/home/cam/README.markdown")
;; -> "file:///home/cam/README.markdown"
```

Next, we need to wrap `document` in a list that includes the base URL.

`document` itself is simply an Emacs Lisp representation of an HTML/XML data structure.
It has the basic format

```lisp
(element-name attributes child-elements...)
```

So for example HTML like

```html
<p>This is <span class="important">IMPORTANT</span></p>
```

would be represented as

```lisp
(p nil "This is " (span ((class . "important")) "IMPORTANT"))
```
Try it yourself. `M-:` and then type or paste the following form

```lisp
(shr-insert-document '(p nil "This is " (span ((class . "important")) "IMPORTANT")))
```

to render it and insert it into the current buffer.

#### Quoting

*Quoting* a form tells Emacs Lisp not evaluate it, and to instead return it
as-is:

```lisp
(+ 1 2)
;; -> 3

'(+ 1 2)
;; -> (+ 1 2)

buffer-file-name
;; -> "/home/cam/README.md"

'buffer-file-name
;; -> buffer-file-name
```

This is especially useful for passing in lists, which would otherwise get
interpreted as function calls, or symbols, which would otherwise get interpreted
as function names or variables. The `'` syntax is shorthand for the `(quote)` form:

```lisp
(quote (+ 1 2))
;; -> (+ 1 2)
```

#### Understanding Lists, Cons Cells, and Association Lists

`attributes` are an *association list*, which is a classic Lisp way to store a
map/dictionary as a list of key/value pairs. A deep dive into association lists
is outside the scope of this article, but we can go over them briefly. But
first, we should take a step back and look at how lists work in Lisp:

A Lisp list is composed of *cons cells*, which is Lisp-speak for linked list
nodes. A cons cell has two pointers: one to an item, and one to the next cons
cell. If a cons cell holds the last item in a list, its pointer to the next cell is `nil`. A cons
cell is written with the *dotted pair* syntax `(x . y)`:

```lisp
'(1 . nil)
;; -> (1)
```

`cons` can also be used to create cons cells and is equivalent to the dotted
pair syntax.

```lisp
(cons 1 nil)
;; -> (1)
```

Very important! Note that `(1 . nil)` or `(cons 1 nil)` are the same thing as the list `(1)`. A
list with a single item is just a cons cell whose first pointer is to the item,
and whose second pointer is `nil`.

You can chain together dotted pair forms or calls to `cons` to create lists if you like doing
things the hard way.

```lisp
'(1 . (2 . (3 . nil)))
;; -> (1 2 3)

(cons 1 (cons 2 (cons 3 nil)))
;; -> (1 2 3)
```

One more thing to note: to add an item to the front of an existing list, you can use `cons`.

```lisp
(cons 1 '(2 3 4))
;; -> (1 2 3 4)
```

The newly created cons cell's item and next-cons-cell pointers are to `1` and
the list `(2 3 4)` respectively. This is not a destructive operation -- the
original list of `(2 3 4)` does not need to be modified in any way. Prepending
items to a list via `cons` is a common operation in classic Lisp languages
(not so much in Clojure).

#### Other uses of cons cells

In some cases, both pointers in a cons cell are used to point to items.
Association lists are one such example: an association list is a list of cons
cells, where each cell represents a `(key . value)` pair.

So a JSON dictionary like

```json
{"a": 100, "b": 200}
```

could be translated to an association list like

```lisp
((a . 100) (b . 200))
```

#### Wrapping our HTML `document`

With that big explanation out of the way, we need to take our `<html>` element
stored in `document` and wrap it in a `<base>` form, so it looks like this:

```html
<base href="file:///home/cam/README.markdown">
  <html>...</html>
</base>
```

In the Lisp representation, this means we need the list

```lisp
(base ((href . "file:///home/cam/README.markdown")) document)
```

There are a few ways to create such a list. None are particularly convenient,
because you need to quote symbols `base` and `href` so the reader doesn't try to
evaluate them, but you can't quote the entire form, because you need to splice in
the URL we created, as well as the value of `document`.

```lisp
(list 'base (list (cons 'href url)) document)
```

is one option:

```lisp
(let ((document "my doc")
      (url "my url"))
  (list 'base (list (cons 'href url)) document))
;; -> (base ((href . "my url")) "my doc")
```

#### Backquote-splicing

Luckily, we can use *backtick* or *backquote-splicing*, which is more fun. As
mentioned before, we want something like `(base ((href . url)) document)`, but
quoting the entire form e.g. `'(base ((href . url)) document)` won't work
because then `url` or `document` would be quoted as well and thus wouldn't get
evaluated. Backquote splicing lets you quote things but selectively disable
quoting:

```lisp
(let ((document "my doc")
      (url "my url"))
  `(base ((href . ,url)) ,document))
;;-> (base ((href . "my url")) "my doc")
```

The backtick starts backquote-splicing, which you can think of as like switching
on a "quote mode" that lasts for the entire form; commas nested temporarily
inside disable quote mode for the next form. (For those familiar with Clojure:
`,` is the traditional Lisp equivalent of Clojure's `~`.)

You can nest backticks forms as well -- in other words, you can turn "quote
mode" back on and off inside nested forms as you see fit:

```lisp
(let ((x 1) (y 2))
  `(,x ,(cons y `(,x 0))))
;; -> (1 (2 1 0))
```

#### Splicing items into the parent form with `,@`

There's another splicing operator: `,@`, which lets you splice the items in a
list directly into their parent form:

```lisp
;; splice the list directly
(let ((my-list '(1 2 3)))
  `(,my-list 4 5))
;; -> ((1 2 3) 4 5)

;; splice the items in the list into the parent list
(let ((my-list '(1 2 3)))
  `(,@my-list 4 5))
;; -> (1 2 3 4 5)
```

Backquote splicing is essential when writing macros, as you'll see a bit later in the chapter.

### Preserving the current window

Right now, our command switches to the window containing the rendered HTML every
time it runs, so you have to switch back to the window with the Markdown file to
continue editing it. We eventually want this command to run automatically whenever we save
a file, and this behavior will get pretty annoying.

We can use `save-selected-window` to restore the current window after evaluating a series of forms:

```lisp
(defun cam/preview-markdown ()
  (interactive)
  (let ((filename buffer-file-name))
    (message "Rendering Markdown preview of %s" filename)
    (shell-command-on-region (point-min) (point-max) "pandoc -f gfm" "*Preview Markdown Output*")
    (save-selected-window
      (switch-to-buffer-other-window "*Preview Markdown Output*")
      (let ((document (libxml-parse-html-region (point) (point-max)))
            (url (concat "file://" filename)))
        (erase-buffer)
        (shr-insert-document `(base ((href . ,url)) ,document))
        (goto-char (point-min))))))
```

To recap: `switch-to-buffer-other-window` switches to a different window and
brings up the `*Preview Markdown Output*` buffer, and `shr-insert-document`
renders the GUI widgets into the current buffer. After that, we're free to
switch back to the original window. `save-selected-window` will preserve the
selected window and current buffer before the forms it wraps are executed; the
forms are then executed normally, and `save-selected-widnow` restores the
selected window and current buffer.

### Making `*Preview Markdown Output*` read-only

Since `*Preview Markdown Output*` contains rendered HTML widgets derived from a
separate source file, it's probably less confusing if we make `*Preview Markdown Output*`
 a read-only buffer. Whether a buffer is read-only is determined by the
variable `buffer-read-only`, which automatically becomes buffer-local if set.
This means when you set this variable, you are setting it only for the current
buffer; the current buffer's value with overshadow the global value.

`shell-command-on-region` will "helpfully" clear the read-only flag when it
writes its output to `*Preview Markdown Output*`, so we don't have to worry
about clearing it out if we end up reusing the buffer after running the command
a second time.

```lisp
(defun cam/preview-markdown ()
  (interactive)
  (let ((filename buffer-file-name))
    (message "Rendering Markdown preview of %s" filename)
    (shell-command-on-region (point-min) (point-max) "pandoc -f gfm" "*Preview Markdown Output*")
    (save-selected-window
      (switch-to-buffer-other-window "*Preview Markdown Output*")
      (let ((document (libxml-parse-html-region (point) (point-max)))
            (url (concat "file://" filename)))
        (erase-buffer)
        (shr-insert-document `(base ((href . ,url)) ,document))
        (goto-char (point-min))
        ;; [1] Make buffer read-only
        (setq buffer-read-only t)))))
```

To change the value of a variable, you can use `setq`. It's the equivalent of `=` assignment in Algol-style languages:

```javascript
// javascript
x = 10;
```

```lisp
;; emacs-lisp
(setq x 10)
```

The `q` in `setq` stands for *quote*. `setq` in the example above is actually
equivalent to:

```lisp
(set 'x 10)
```

`'x` is quoted because we're setting the value named by the symbol `x`. We want

```
x = 10
```

rather than

```
[current value of x] = 10
```

which probably won't make sense.

```lisp
(set 'x 100)
;; -> 100
(set x 200)
*** Eval error ***  Wrong type argument: symbolp, 100
```

This error means that it expected a symbol (i.e., something that satisfied the
`symbolp` predicate function) but got `100`. `100 = 200` doesn't really make
sense.

#### Macros 101

So how does `setq` work if the variable name needs to be quoted? With macros,
you can take the arguments you're passed, *before they are evaluated*, and
generate whatever code you want. This new code is used in place of the macro form.

`setq` could be implemented as a macro that quotes the variable name symbol.
(`setq` is *actually* a built-in special form, but you can implement similar
behavior with a macro). Here's a simple `setq`-style macro:

```lisp
(defmacro my-setq (symbol value)
  `(set ',symbol ,value))

(my-setq x 200)
;; -> 200
```

The important thing to know about Lisp macros is that they're just functions
that take arguments and return a new Lisp form to be used in their place. Macros
and regular functions essentially differ only in when they're evaluated, and
what's done with the result, but everything else works the same. Macros can call
other functions, and more complex macros often have parts of their
implementation split out into separate functions, defined with `defun` like any
other function.

Evaluating a macro form and replacing it with the result is called macro
"expansion". When a form is evaluated, all macros are expanded first (top-level
macro forms are expanded before ones nested inside them), and only *then* are
variables and function calls evaluated.

`(set 'x 100)` is just a list with the elements `set`, `'x`, and `100`, so if we
have a macro generate that sort of list for us our dreams will come true. In the
example above, we use backquote splicing to construct our list.

##### Macroexpansion

You can use `macroexpand` to expand a macro form to see the Lisp form it expands to:

```lisp
(macroexpand '(my-setq my-variable 200))
;; -> (set 'my-variable 200)
```

Note that you need to quote the form passed so it doesn't get evaluated before
`macroexpand` sees it.

Macros can of course expand to other macros, and Emacs Lisp will continue
expanding things until there are no more macros to expand. To expand a macro
form once instead of completely, you can use `macroexpand-1` instead of
`macroexpand`. In the example above, the result of `my-setq` is already fully
expanded, so both functions give you the same result.

When writing macros, some tools you might you might find the built-in
`pp-macroexpand-last-sexp` command useful; it pretty-prints the results of a
macro expansion. I'm also a big fan of the
[`macrostep`](https://github.com/joddie/macrostep) package, which lets you view
the expansions of macros directly in your source code.

### Scrolling to approximate location in preview

It's a little annoying to work on a giant Markdown file (like the source for
this Chapter) and have the preview always scroll to the very top. Why not just
have it scroll approximately the same position we're currently looking at?

Here's the quick and dirty solution we'll implement:

1. When first running our command, while the Markdown buffer is still current,
   record how far we've scrolled thru the document (e.g. 40%).
2. After we've rendered the HTML as GUI widgets, scroll to the line that is
   approximately the same distance thru the document.

This will work even if the rendered output has a lot more or less lines than the
Markdown source. For example, if our Markdown source file is 1000 lines, and the
rendered output 800 lines, and the top of the window shows line 400, we'd record
a `scroll-percentage` of 0.4 (40% scrolled); in the rendered output, we'll
scroll to line 320 (800 * 0.4).

This isn't perfect, since there isn't a 1:1 translation between Markdown text
and rendered HTML lines, but in my experience it works well enough that I
haven't bothered creating a more sophisticated version.

For readability, I broke the command we've been working on out into a few
separate functions. Since Emacs Lisp doesn't have encapsulation features like
private functions, functions intended to be private are often given a name with
a dash after the "namespace" part of the name, such as `package--function` or
`package/-function`. Thus I named our "private" functions with the pattern
`cam/-function-name`.

```lisp
(defun cam/-scroll-percentage ()
  (/ (float (line-number-at-pos (window-start)))
     (float (line-number-at-pos (point-max)))))

(defun cam/-set-window-start-to-percentage (scroll-percentage)
  ;; Move to the beginning of the rendered output buffer
  (goto-char (point-min))
  ;; target line number is floor(total-number-of lines * scroll-percentage)
  (let ((target-line-number (truncate (* (line-number-at-pos (point-max)) scroll-percentage))))
    ;; move to target line number
    (forward-line (1- target-line-number)))
  ;; now scroll the window so the line in question is at the top
  (set-window-start nil (point)))

(defun cam/preview-markdown ()
  (interactive)
  (let ((url (concat "file://" buffer-file-name))
        ;; record how far thru the Markdown source file we've scrolled
        (scroll-percentage (cam/-scroll-percentage)))
    (message "Rendering Markdown preview of %s" buffer-file-name)
    (shell-command-on-region (point-min) (point-max) "pandoc -f gfm" "*Preview Markdown Output*")
    (save-selected-window
      (switch-to-buffer-other-window "*Preview Markdown Output*")
      (let ((document (libxml-parse-html-region (point) (point-max))))
        (erase-buffer)
        (shr-insert-document `(base ((href . ,url)) ,document))
        (cam/-set-window-start-to-percentage scroll-percentage)
        (setq buffer-read-only t)))))
```

This is pretty straightforward. To calculate the scroll percentage, we:

1.  Use `(window-start)` to get the position of the first character visible in
    the current window

2.  Use `line-number-at-pos` to convert the position to a line number.

3.  Calculate the total number of lines by getting the line number of the last
    character in the buffer by calling `(line-number-at-pos (point-max))`.

4.  Divide the window start line by the last line to get a percentage. As in C,
    integer division is truncated. By first casting the integers to
    floating-point numbers with `(float)` we can use floating-point division
    instead; the final result is a number like `0.4`.

    ```lisp
    (/ 40 100)
    ;; -> 0

    (/ (float 40) (float 100))
    ;; -> 0.4
    ```

Once the output is rendered, to scroll to the line the desired line, we:

1.  Move the cursor to the beginning of the buffer by calling `(goto-char (point-min))`

2.  Calculate the total number of lines using `(line-number-at-pos (point-max))`

3.  Multiply the total number of lines by `scroll-percentage`

4.  Call `truncate` to convert the resulting floating point number to an
    integer. The result of this is our `target-line-number`.

    ```lisp
    (truncate 20.5432)
    ;; -> 20
    ```

5.  Next, we move the cursor to our target line. Since we already moved to line
    1 in step 1, we need to move forward by `target-line-number - 1` lines.
    `forward-line` is used to move forward a number of lines. For example, if we
    want to move from line 1 to line 20, we can call `(forward-line 19)`. `1-`
    function returns its argument minus one.

    ```lisp
    (1- 20)
    ;; -> 19
    ```

6.  Now that the cursor is at the correct position, we can scroll the window so
    the first line is the one with the cursor. `(point)` gets the current
    position of the cursor (the *point*) and `set-window-start` scrolls the
    window so it shows that position in the first line ("start") of the window.
    Nice!

# Prompting for a file to preview

Here we've broken out `cam/preview-markdown` a bit further.

This version of the command works the same as before when run programmatically,
but when executed interactively (e.g. via `M-x`) it will prompt you for the file
to preview, complete with autocomplete; it defaults to the current file.

```lisp
(defun cam/-scroll-percentage ()
  (/ (float (line-number-at-pos (window-start)))
     (float (line-number-at-pos (point-max)))))

(defun cam/-set-window-start-to-percentage (scroll-percentage)
  (goto-char (point-min))
  (let ((target-line-number (truncate (* (line-number-at-pos (point-max)) scroll-percentage))))
    (forward-line (1- target-line-number)))
  (set-window-start nil (point)))

(defun cam/-render-markdown-preview-current-buffer ()
  (message "Rendering Markdown preview of %s" buffer-file-name)
  (shell-command-on-region (point-min) (point-max) "pandoc -f gfm" "*Preview Markdown Output*")
  (switch-to-buffer-other-window "*Preview Markdown Output*")
  (let ((document (libxml-parse-html-region (point) (point-max))))
    (erase-buffer)
    (shr-insert-document `(base ((href . ,url)) ,document))
    (setq buffer-read-only t)))

(defun cam/-preview-markdown-file (filename)
  (save-selected-window
    (find-file filename)
    (let ((url (concat "file://" filename))
          (scroll-percentage (cam/-scroll-percentage)))
      (cam/-render-markdown-preview-current-buffer)
      (cam/-set-window-start-to-percentage scroll-percentage))))

(defun cam/preview-markdown (&optional filename)
  "Render a markdown preview of FILENAME (by default, the current file) to HTML and display it with `shr-insert-document'."
  (interactive "fFile: ")
  (if filename
      (progn
        (cam/-preview-markdown-file filename)
        (switch-to-buffer (current-buffer)))
    (cam/-preview-markdown-file buffer-file-name)))
```

There are a couple of new concepts to introduce here:

1. `&optional` arguments
2. arguments to the `(interactive)` declaration
3. `progn`
4. Emacs Lisp docstrings

### Optional arguments

In function definitions, `&optional` is used to denote optional positional
arguments. If an optional argument is not passed, its value will be `nil`
in the body of the function:

```lisp
(defun my-filename (&optional filename)
  filename)

(my-filename "x.txt") ; -> "x.txt"
(my-filename)         ; -> nil
```

In

```lisp
(defun cam/preview-markdown (&optional filename)
  ...)
```

we are making the `filename` argument optional, because we'd like to be able to
call this function with a specific file to preview, but default to the current
file when it is called with no argument (such as when it is called as part of a
*hook*, as discussed below).

### Specifying a prompt in the `interactive` declaration

The `interactive` declaration can optionally take an argument that tells Emacs
how to prompt for values to use as the command's arguments:

`(interactive "fFile: ")`

The `f` [code
character](https://www.gnu.org/software/emacs/manual/html_node/elisp/Interactive-Codes.html#Interactive-Codes)
tells Emacs to prompt for an **existing filename**, defaulting to the name of the
file in the current buffer. The rest of the string is the text of the prompt to
show the user in the minibuffer (`"File: "`). Note that we have to include the
space after the prompt ourselves.

The string name of the file the user chooses will be passed in as the `filename`
argument. When the function is invoked programmatically, Emacs will not prompt
the user for a value of `filename`.

### `if` forms

In Emacs Lisp, `if` forms have the syntax

```lisp
(if condition
    then-form
  else-forms...)
```

For example:

```lisp
;; t means true and nil means null/false
(defun my-> (x y)
  (if (> x y)
      t
    nil))

(my-> 3 2)
;; -> t
```

The `if` form in the example is roughly equivalent to this Algol-style if form:

```javascript
if (x > y) {
  true;
} else {
  false;
}
```

### `progn` forms

`progn` can be used to execute multiple statements as a single form. (If you're
familiar with Clojure, this is the classic Lisp equivalent of `do`). Each form is
evaluated in order, and the result of a `progn` form is the result of the last
form contained by it:

```lisp
(progn
  (message "We are adding some numbers.")
  (+ 3 4))
;; -> 7
```

Because only the value of the last form is returned, forms other than the last
are usually executed for side effects.

### Understanding the updated `cam/preview-markdown`

Now that we've discussed the new concepts, Let's take a deeper look at our simplified `cam/preview-markdown` function:

```lisp
(defun cam/preview-markdown (&optional filename)
  "Render a markdown preview of FILENAME (by default, the current file) to HTML and display it with `shr-insert-document'."
  (interactive "fFile: ")
  (if filename
      (progn
        (cam/-preview-markdown-file filename)
        (switch-to-buffer (current-buffer)))
    (cam/-preview-markdown-file buffer-file-name)))
```


1.  If a `filename` argument was passed:
    1.  Call `cam/-preview-markdown-file` with that filename
    2.  Switch back to the current buffer (`save-selected-window`, called in
        `cam/-preview-markdown-file`, also restores the current buffer, but
        doesn't necessarily bring it to the top).
2.  If no `filename` argument was passed:
    1.  Call `cam/-preview-markdown-file` with the filename of the current
        buffer.


### Opening a file/switching to existing buffer for a file

In `cam/-preview-markdown-file` we've added a call to `find-file`.

```lisp
(find-file filename)
```

switches to a buffer containing `filename`. In cases where we're already looking
at that file, `find-file` doesn't change anything, so this code continues to
work normally when rendering the file in the current buffer. For other files, it
will switch to a buffer for that file, creating a buffer (opening the file) if
needed.

### Adding a docsting

```emacs-liso
(defun cam/preview-markdown (&optional filename)
  "Render a markdown preview of FILENAME (by default, the current file) to HTML and display it with `shr-insert-document'."
  (interactive "fFile: ")
  ...)
```

Emacs Lisp docstrings come after the argument list but *before* the
`interactive` declaration, if there is one. Emacs Lisp docstrings conventionally
mention arguments in all capital letters (e.g. `FILENAME`). When viewing the
documentation for a function, arguments mentioned this way are highlighted
automatically.

You can add hyperlinks to other Emacs Lisp functions or variables using the

```
`symbol-name'
```

syntax. These days you can also use *curved* single quotes instead, but figuring
out how to type them involves too much effort, so I stick with the
backtick-single-quote convention, which is the one you'll encounter most
commonly in the wild.

Try it yourself! `C-h f cam/preview-markdown` to view the documentation for
the function `cam/preview-markdown`.

[This
page](https://www.gnu.org/software/emacs/manual/html_node/elisp/Documentation-Tips.html)
of the Emacs Lisp documentation has a very good explanation of docstrings for further reading.

# Adding an `after-save-hook`

Now all that's left to do is telling Emacs to automatically run our command
whenever we save a Markdown file.

```lisp
(add-hook 'markdown-mode-hook
  (lambda ()
    (add-hook 'after-save-hook #'cam/preview-markdown nil t)))
```

Whenever we open a Markdown file, Emacs will set the major mode to
`markdown-mode`. Major modes and most minor modes have *hooks* that run whenever
the mode is entered. When we enter `markdown-mode`, Emacs will run any functions
in `markdown-mode-hook`.

To `markdown-mode-hook` we add a *lambda* (anonymous function) that itself adds
the command `#'cam/preview-markdown` to `after-save-hook`. Any functions in
`after-save-hook` will run after a file is saved.

A *hook* is just a variable that contains a list of functions that get ran at a
specific point in time. `markdown-mode-hook` is a list of functions to run when
entering `markdown-mode` and `after-save-hook` is a list of functions to run
after saving a file.

### Add-hook and buffer-local variables

`add-hook` adds a function to a hook (which, again, is just a list), creating
the variable if it doesn't already exist. `add-hook` has two `optional` args,
`append` (default `nil`) and `local` (default `nil`). `append` tells it to add
the function at the end of the list, meaning it gets ran last. In some cases, it
is preferable to run a certain function after others in the hook have ran. In
our case, it doesn't really matter if our function runs before or after others,
so we'll pass the default value of `nil`. `local` tells it to add it to the
*buffer-local* version of the hook rather than the global version. We'll explore
the difference more in the future, but for now suffice to know that variables
can have global values as well as values specific to a buffer. Buffer-local
values overshadow global values.

```lisp
(add-hook 'after-save-hook #'cam/preview-markdown)
```

without the optional args would add the function `#'cam/preview-markdown` to the
*global* `after-save-hook`, which means it would run after saving *any* file,
which is not what we want. By adding the `local` option, the function is only
added to the hook for the current buffer, meaning it only runs for the current buffer.

To sum it up: `markdown-mode-hook` gets ran once for every new Markdown file we
open, and we use that to add `cam/preview-markdown` to the `after-save-hook` for
the newly created buffer. Files that aren't opened in `markdown-mode` aren't
affected at all.

#### Lisp-1s and Lisp-2s

Emacs Lisp is a Lisp-2, which means that variables and functions live in separate
"namespaces". For example, `length` could refer to both a variable named `length`
*and* a function named `length`.

```lisp
;; Emacs Lisp
(length '(1 2 3))
;; -> 3

;; the length variable doesn't overshadow the length function
(let ((length 4))
  (length '(1 2 3)))
;; -> 3
```

Contrast this to Clojure, a Lisp-1, where variables and functions share a
"namespace":

```clojure
;; Clojure
;; count is the Clojure equivalent of length
(count '(1 2 3))
;; -> 3

;; let-bound count overshadows *any* usage of the symbol
(let [count 4]
  (count '(1 2 3)))
;; -> [100 line stacktrace: Integer cannot be invoked as a function]
```

Using functions passed as arguments is much simpler in Lisp-1s, however:

```clojure
;; Clojure
(defn call-f [f]
  (f 100))

(call-f (fn [n] (inc n)) 100)
;; -> 101
```

With Lisp-2s, you have to use `funcall` to call a function bound to a variable:

```lisp
;; Emacs Lisp
;; the symbol f refers only to variable, so to use it as a function contained in
;; f, you have to use funcall
(defun call-f (f)
  (funcall f 100))

(call-f (lambda (n) (1+ n)) 100)
;; -> 101
```

There are pros and cons to both Lisp-1s and Lisp-2s. Lisp-1s lend themselves
more elegantly to passing around functions since you don't need to use special
forms like `funcall`. However you have to be much more careful not to
unintentionally overshadow functions in Lisp-1s, which usually means
intentionally misspelling function parameter names. You'll often see nonsense
like this in Lisp-1s:

```clojure
;; clojure
;; so as to not overshadow the "type" function, we have to give our type
;; parameter a different name, such as "typ"
(defn type=
  "True if the type of `x` is equal to `typ`."
  [x typ]
  (= (type x) typ))
```

### Quoting function names

Attempting to evaluate `cam/preview-markdown` will result in an error:

```lisp
cam/preview-markdown
*** Eval error ***  Symbol’s value as variable is void: cam/preview-markdown
```

Instead, we can quote the symbol name so the reader doesn't attempt to evaluate
it. Emacs offers an alternative syntax for quoting symbols that refer to
functions:

```lisp
#'cam/preview-markdown
```

Just as `'x` is shorthand for `(quote x)`, `#'x` is shorthand for `(function x)`.

In many cases, using `quote` for function names will still work correctly:

```lisp
(add-hook 'after-save-hook 'cam/preview-markdown)
```

But `function` is preferred over `quote` for symbols that name functions for a
couple of reasons: it's clearer and more explicit, and the compiler is better
able to optimize `function` forms.
