---
layout: post
title:  "Intro to Emacs Lisp: Adding Live Previews when Editing Markdown Files (Part 1 of 3)"
date:   2020-06-09T13:12:00-07:00
categories: emacs-lisp
---

This post is part one of a series of 3 posts. View the other parts:

*  [Part 1](https://camsaul.com/emacs-lisp/2020/06/09/emacs-lisp-intro-markdown-live-previews-part-1.html)
*  [Part 2](https://camsaul.com/emacs-lisp/2020/06/09/emacs-lisp-intro-markdown-live-previews-part-2.html)
*  [Part 3](https://camsaul.com/emacs-lisp/2020/06/10/emacs-lisp-intro-markdown-live-previews-part-3.html)

This is the first post in what I hope is an ongoing series of posts that show
you how to get started with Emacs Lisp by writing practical Emacs commands and
customizing Emacs in other ways. For me, one of the best things about Emacs is
that you can customize it to work however you want, but learning how to do so is
easier said than done, and while the official Emacs Lisp documentation is
extremely detailed, it can be hard to approach.

I'm hoping I can demonstrate that Emacs Lisp is fun to write and not as hard as
you might think. I'm going to assume experience using Emacs. For example, you
should have a basic understanding of things like *windows* and *buffers*, and know
how to read and type keyboard shortcuts written in the Emacs style (`C-x C-s` the world).

I will also assume experience programming in other languages, but not
necessarily Lisp ones; however, you should still have a very basic
understanding of how Lisp languages work, for example what the result of `(+ 1 2)`
is. If you don't, the [GNU Intro to Emacs Lisp](https://www.gnu.org/software/emacs/manual/eintr.html)
is a good starting point.

In this series of posts I'm going to show you step-by-step how to set write a
command to render a preview of a Markdown file and open it directly in Emacs.
Then I'll show you how to have it run automatically whenever you save a Markdown
file.

For this tutorial you'll want to install [`pandoc`](https://pandoc.org/)
which we will use for rendering Markdown files, Emacs 25 or newer, and the
[`markdown-mode`](https://github.com/jrblevin/markdown-mode) package.

[![Preview](/assets/emacs-markdown-live-preview.gif)](/assets/emacs-markdown-live-preview.gif)

### Evaluating forms and `ielm`

Before we get started, I want to briefly mention how to follow along with the
code in this post. For the functions we're working on, you'll probably want to
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

```emacs-lisp
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
macros with `cam/`, so I'll use that in the code in this post.

### Making it a command

Now this function itself doesn't do anything, and you can't invoke it with
`M-x`. Emacs Lisp functions can either be invoked programmatically (e.g. by
other Emacs Lisp functions, or the `ielm` REPL, or via `M-:`) or *interactively*
(via `M-x` or a keybinding). A function that be invoked interactively is known
as a *command*.

To make a function a command, you add an `interactive` declaration, which always
goes right after the argument list (or docstring, if you have one):

```emacs-lisp
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

```emacs-lisp
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
    like you saw in the GIF at the start of this post.

So let's add a command to render the Markdown to HTML using the external `pandoc` command:

```emacs-lisp
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

```emacs-lisp
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

```emacs-lisp
(let ((filename buffer-file-name))
  ...)
```

Not all buffers are associated with files. `*Preview Markdown Output*` isn't
actually associated with a file, so `buffer-file-name` will be `nil` if we
evaluate it with `*Preview Markdown Output*` as the current buffer. To work
around this, we can evaluate `buffer-file-name` *before* we switch buffers and
save it for later as the local variable `filename`. `(let ...)` is used to
introduce local bindings.

```emacs-lisp
;; x = 100
(let ((x 100))
  (+ x x))
;; -> 200
```

You might be wondering why you need two sets of parentheses to set `x` in the
example above. The syntax for `let` is

```emacs-lisp
(let bindings
  ...)
```

where `bindings` is a list of bindings of the form `(symbol value)`, e.g. `(x 100)`.
This means you can pass in multiple binding forms to bind several local
variables at once:

```emacs-lisp
;; x, y = 100, 200
(let ((x 100)
      (y 200))
  (+ x y))
;; -> 300
```

Bindings can also be of the form `symbol`, which is just shorthand for `(symbol nil)`:

```emacs-lisp
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

```emacs-lisp
;; x = nil
(let (x)
  ;; x = 100
  (setq x 100)
  x)
;; -> 100
```

You can use `setq` to set the value of a variable. `setq` is discussed more at
length in Part 2. Note that Emacs Lisp is in many ways a traditional imperative
language. Emacs Lisp *is* a functional language in the sense that functions are
first-class objects that can be passed around as parameters and returned. But
it's not a functional language in the same pure function/immutable value sense
that Clojure or Haskell are.

There's one more thing you should know about `let`: bindings are done
independently of one another, as if they were done in parallel.

```emacs-lisp
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

```emacs-lisp
(let ((x 100)
      (y (+ x x)))
  y)
;; Error: x is not bound yet
```

Many times you do want to refer to the results of one binding form in a subsequent one. In this case, you can use `let*`,
which binds things sequentially rather than in parallel:

```emacs-lisp
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

```emacs-lisp
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

```emacs-lisp
(libxml-parse-html-region (point-min) (point-max))
```

Before we can render the HTML, we have to parse it. `libxml-parse-html-region`
parses HTML in the region. Again, we're using `point-min` and `point-max` to
tell it to parse the entire buffer; we're saving the output of that function as
`document`.

#### Clearing the contents of `*Preview Markdown Output*`

```emacs-lisp
(erase-buffer)
```

We're going to put the rendered HTML back into `*Preview Markdown Output*`, so
we need to clear out the HTML source first. `(erase-buffer)` clears the contents
of the current buffer.

#### Render the parsed HTML document with SHR

```emacs-lisp
(shr-insert-document document)
```

Renders the parsed HTML document in the current buffer.

#### Moving back to the beginning of the buffer

```emacs-lisp
(goto-char (point-min))
```

At this point, the cursor will be all the way at the end of the buffer. It's
nicer behavior if our preview shows us the beginning of the file rather than the
end. `goto-char` moves the cursor, and we're moving it to the beginning of the
buffer, again using `point-min` to determine the beginning.

# Next Steps


In [Part 2](https://camsaul.com/emacs-lisp/2020/06/09/emacs-lisp-intro-markdown-live-previews-part-2.html), we'll make
improvements to the command, and discuss Emacs Lisp lists, cons cells,
backquote-splicing, and macros.

In [Part 3](https://camsaul.com/emacs-lisp/2020/06/10/emacs-lisp-intro-markdown-live-previews-part-3.html), we'll have
the command prompt us for a file to preview when running interactively, and have
Emacs run it automatically whenever we save a Markdown file. We'll discuss
optional arguments, `interactive` code characters, `progn` and `if` forms, hooks,
buffer-local variables, and Lisp-2s.
