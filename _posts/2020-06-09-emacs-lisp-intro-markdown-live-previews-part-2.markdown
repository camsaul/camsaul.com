---
layout: post
title:  "Intro to Emacs Lisp: Adding Live Previews when Editing Markdown Files (Part 2 of 3)"
date:   2020-06-09T16:12:00-07:00
categories: emacs-lisp
---

This post is part two of a series of 3 posts. View the other parts:

*  [Part 1](./emacs-lisp-intro-markdown-live-previews-part-1.html)
*  [Part 2](./emacs-lisp-intro-markdown-live-previews-part-2.html)
*  [Part 3](./emacs-lisp-intro-markdown-live-previews-part-3.html)

In this post we'll make a series of improvements to the `cam/preview-markdown` command, including:

*  Rendering in a way that images and other files relative to the directory
   containing the Markdown source work correctly
*  Preserving the current window
*  Making `*Preview Markdown Output*` read-only
*  Scrolling the preview so it's at approximately the same location as the Markdown buffer

We'll also look at the following Emacs Lisp concepts:

*  Lists, cons cells, and association lists (briefly)
*  Backquote-splicing
*  Macros

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

```emacs-lisp
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
        ;; wrap `document` in a <base> element
        (shr-insert-document `(base ((href . ,url)) ,document))
        (goto-char (point-min))))))
```

First, we need to create a `file://` URL that SHR can use as the base URL for
images or other elements with a relative path. If our original file is
`/home/cam/README.markdown`, our URL should be
`file:///home/cam/README.markdown`. We can use `concat` to concatenate multiple
arguments together into a single string:

```emacs-lisp
(concat "file://" "/home/cam/README.markdown")
;; -> "file:///home/cam/README.markdown"
```

Next, we need to wrap `document` in a list that includes the base URL.

`document` itself is simply an Emacs Lisp representation of an HTML/XML data structure.
It has the basic format

```emacs-lisp
(element-name attributes child-elements...)
```

So for example HTML like

```html
<p>This is <span class="important">IMPORTANT</span></p>
```

would be represented as

```emacs-lisp
(p nil "This is " (span ((class . "important")) "IMPORTANT"))
```
Try it yourself. `M-:` and then type or paste the following form

```emacs-lisp
(shr-insert-document '(p nil "This is " (span ((class . "important")) "IMPORTANT")))
```

to render it and insert it into the current buffer.

#### Quoting

*Quoting* a form tells Emacs Lisp not evaluate it, and to instead return it
as-is:

```emacs-lisp
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

```emacs-lisp
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

```emacs-lisp
'(1 . nil)
;; -> (1)
```

`cons` can also be used to create cons cells and is equivalent to the dotted
pair syntax.

```emacs-lisp
(cons 1 nil)
;; -> (1)
```

Very important! Note that `(1 . nil)` or `(cons 1 nil)` are the same thing as the list `(1)`. A
list with a single item is just a cons cell whose first pointer is to the item,
and whose second pointer is `nil`.

You can chain together dotted pair forms or calls to `cons` to create lists if you like doing
things the hard way.

```emacs-lisp
'(1 . (2 . (3 . nil)))
;; -> (1 2 3)

(cons 1 (cons 2 (cons 3 nil)))
;; -> (1 2 3)
```

One more thing to note: to add an item to the front of an existing list, you can use `cons`.

```emacs-lisp
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

```emacs-lisp
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

```emacs-lisp
(base ((href . "file:///home/cam/README.markdown")) document)
```

There are a few ways to create such a list. None are particularly convenient,
because you need to quote symbols `base` and `href` so the reader doesn't try to
evaluate them, but you can't quote the entire form, because you need to splice in
the URL we created, as well as the value of `document`.

```emacs-lisp
(list 'base (list (cons 'href url)) document)
```

is one option:

```emacs-lisp
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
evaluated. Luckily, there is a way quote things but selectively disable quoting:

```emacs-lisp
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

```emacs-lisp
(let ((x 1) (y 2))
  `(,x ,(cons y `(,x 0))))
;; -> (1 (2 1 0))
```

#### Splicing items into the parent form with `,@`

There's another splicing operator: `,@`, which lets you splice the items in a
list directly into their parent form:

```emacs-lisp
;; splice the list directly
(let ((my-list '(1 2 3)))
  `(,my-list 4 5))
;; -> ((1 2 3) 4 5)

;; splice the items in the list into the parent list
(let ((my-list '(1 2 3)))
  `(,@my-list 4 5))
;; -> (1 2 3 4 5)
```

Backquote splicing is essential when writing macros, so we'll revisit it in a
future post.

### Preserving the current window

Right now, our command switches to the window containing the rendered HTML every
time it runs, so you have to switch back to the window with the Markdown file to
continue editing it. We eventually want this command to run automatically whenever we save
a file, and this behavior will get pretty annoying.

We can use `save-selected-window` to restore the current window after evaluating a series of forms:

```emacs-lisp
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
buffer; the current buffer's value with overshadow the global default value.

`shell-command-on-region` will "helpfully" clear the read-only flag when it
writes its output to `*Preview Markdown Output*`, so we don't have to worry
about clearing it out if we end up reusing the buffer after running the command
a second time.

```emacs-lisp
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

```emacs-lisp
;; emacs-lisp
(setq x 10)
```

The `q` in `setq` stands for *quote*. `setq` in the example above is actually
equivalent to:

```emacs-lisp
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

```emacs-lisp
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
generate whatever code you want. So `setq` could be implemented with a macro
that quotes the variable name symbol. (`setq` is actually a built-in special
form, but you can implement similar behavior with a macro). Here's a simple
`setq`-style macro:

```emacs-lisp
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
have a macro generate that sort of list for us our dreams will come. We can use
backquote splicing to easily construct our list.

##### Macroexpansion

You can use `macroexpand` to expand a macro form to see the Lisp form it expands to:

```emacs-lisp
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

The command `pp-macroexpand-last-sexp` is useful for pretty-printing the results
of a macro expansion; the [`macrostep`](https://github.com/joddie/macrostep)
package lets you view the expansions of macros directly in your source code.

### Scrolling to approximate location in preview

It's a little annoying to work on a giant Markdown file like this blog post and
have the preview always scroll to the very top. Why not just have it scroll
approximately the same position we're currently looking at?

We can accomplish this by recording how far thru the Markdown document we are as
a percentage of line numbers and then scroll the output to line number that is
the closest percentage. For example, if our Markdown source file is 1000 lines,
and the rendered output 800 lines, and the top of the window shows line 400,
we'd record a `scroll-percentage` of 0.4 (40% scrolled), and we'll scroll to
line 320 (800 * 0.4) in the output buffer.

This isn't perfect, since there isn't a 1:1 translation between Markdown text
and rendered HTML lines, but in my experience it works well enough that I
haven't bothered creating a more sophisticated version.

For readability, I broke the function out into a few separate functions. Since
Emacs Lisp doesn't have encapsulation features like "private" functions,
functions intended to be private are often given a name with a dash after the
"namespace" part of the name, such as `package--function` or
`package/-function`.

```emacs-lisp
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

This is pretty straightforward. First, we record the scroll percentage, using
`(window-start)` to get the position of the first character visible in the
current window, and use `line-number-at-pos` to convert the position to a line
number. Then we get the final line number by calling `(line-number-at-pos (point-max))`. As
 in C, integer division is truncated.  By first casting the integers to floating-point numbers with
`(float)` we can use floating-point division instead; the final result is a number like
`0.4`.

Once the output is rendered, we first move the cursor to the beginning
of the buffer, then calculate the total number of lines using
`(line-number-at-pos (point-max))`. We multiple this by `scroll-percentage` and
call `truncate` to convert the floating point number to an integer by dropping
the fractional values to get our target line number. Finally, since we're
already at line 1, we move forward by `target-line-number - 1` lines; the `1-`
function returns a its argument minus one.

Now that the cursor is at the correct position, we can scroll the window so the
first line is the one with the cursor. `(point)` gets the current position of
the cursor (the *point*) and `set-window-start` scrolls the window so that
position is at the beginning of the window. Nice!

# Next Steps

In [Part 3](./emacs-lisp-intro-markdown-live-previews-part-3.html), we'll have
the command prompt us for a file to preview when running interactively, and have
Emacs run it automatically whenever we save a Markdown file. We'll discuss
optional arguments, `interactive` code characters, `progn` forms, hooks,
buffer-local variables, and Lisp-2s.
