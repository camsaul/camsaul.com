---
layout: post
title:  "Intro to Emacs Lisp: Adding Live Previews when Editing Markdown Files (Part 3 of 3)"
date:   2020-06-09T19:12:00-07:00
categories: emacs-lisp
---

This post is part two of a series of 3 posts. View the other parts:

*  [Part 1](https://camsaul.com/emacs-lisp/2020/06/09/emacs-lisp-intro-markdown-live-previews-part-1.html)
*  [Part 2](https://camsaul.com/emacs-lisp/2020/06/09/emacs-lisp-intro-markdown-live-previews-part-2.html)
*  [Part 3](https://camsaul.com/emacs-lisp/2020/06/10/emacs-lisp-intro-markdown-live-previews-part-3.html)

In this post, we'll make the command more useful by:

*  Having it prompt us for a file to preview when running interactively.
*  Adding documentation
*  Having Emacs run it automatically whenever we save a Markdown file

We'll discuss the following Emacs Lisp concepts:

*  Optional arguments
*  `interactive` code characters
*  `progn`
*  Docstrings
*  Hooks
*  Buffer-local variables
*  What it means that Emacs Lisp is a Lisp-2

# Prompting for a file to preview

Here we've broken out `cam/preview-markdown` a bit further.

This version of the function works the same as before when run programmatically,
but when executed interactively (e.g. via `M-x`) it will prompt you for the file
to preview, complete with autocomplete; it defaults to the current file.

```emacs-lisp
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

```emacs-lisp
(defun my-filename (&optional filename)
  filename)

(my-filename "x.txt") ; -> "x.txt"
(my-filename)         ; -> nil
```

In

```emacs-lisp
(defun cam/preview-markdown (&optional filename)
  ...)
```

we are making the `filename` argument optional, because we'd like to be able to
call this function with an specific file to preview, but default to the current
file when called with no argument (such as when it is called programmatically).

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
argument. When the function is invoked programmatically by the
`after-save-hook` (discussed below), `filename` will not be prompted for, and will be `nil`.

### `progn` forms

Let's take a deeper look at our simplified `cam/preview-markdown` function:

```emacs-lisp
(defun cam/preview-markdown (&optional filename)
  "Render a markdown preview of FILENAME (by default, the current file) to HTML and display it with `shr-insert-document'."
  (interactive "fFile: ")
  (if filename
      (progn
        (cam/-preview-markdown-file filename)
        (switch-to-buffer (current-buffer)))
    (cam/-preview-markdown-file buffer-file-name)))
```

1.  If a `filename` argument was passed, call `cam/-preview-markdown-file` with
    that filename, then switch back to the current buffer
    (`save-selected-window` also restores the current buffer, but doesn't
    necessarily bring it to the top).
2.  If no `filename` argument was passed, call `cam/-preview-markdown-file` with
    the filename of the current buffer.

`progn` can be used to execute multiple statements as a single form. (If you're
familiar with Clojure, this is the classic Lisp equivalent of `do`). Each form is
evaluated in order, and the result of a `progn` form is the result of the last
form contained by it:

```emacs-lisp
(progn
  (+ 1 2)
  (+ 3 4))
;; -> 7
```

Because the values of forms other than the last are discarded, the other forms
passed to `progn` are usually executed for side effects.

### Opening a file/switching to existing buffer for a file

In `cam/-preview-markdown-file` we've added a call to `find-file`.

```emacs-lisp
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
`interactive` declaration, if there is one. (For Clojure people -- this is an
important difference.) Emacs Lisp docstrings conventionally mention arguments in
all capital letters. When viewing the documentation for a function, arguments
mentioned this way are highlighted automatically.

You can add hyperlinks to other Emacs lisp functions or variables using the

```
`symbol-name'
```

syntax. These days you can also use curved single quotes instead, but figuring
out how to type them involves effort, so I stick with the backtick-single-quote
convention, which is the one you'll encounter most commonly in the wild.

Try it yourself! `C-h f cam/preview-markdown` to view the documentation for
the function `cam/preview-markdown`.

[This
page](https://www.gnu.org/software/emacs/manual/html_node/elisp/Documentation-Tips.html)
of the Emacs Lisp documentation has a very good explanation of docstrings for further reading.

# Adding an `after-save-hook`

Now all that's left to do is telling Emacs to automatically run our command
whenever we save a Markdown file.

```emacs-lisp
(add-hook 'markdown-mode-hook
  (lambda ()
    (add-hook 'after-save-hook #'cam/preview-markdown nil t)))
```

Whenever we open a Markdown file, Emacs will set the major mode to
`markdown-mode`. Major modes and most minor modes have *hooks* that run whenever
the mode is entered. When we enter `markdown-mode`, Emacs will run any functions
in `markdown-mode-hook`.

To `markdown-mode-hook`, we add a *lambda* (anonymous function) that itself adds
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
the function at the end of the list. In some cases, it is preferable to run a
certain function after others in the hook have ran. In our case, it doesn't
really matter if our function runs before or after others, so we'll pass the
default value of `nil`. `local` tells it to add it to the *buffer-local* version
of the hook rather than the global version. We'll explore the difference more in
the future, but for now suffice to know that variables can have global values as
well as values specific to a buffer. Buffer-local values overshadow
global values.

```emacs-lisp
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

```emacs-lisp
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
(defn call-f [f]
  (f 100))

(call-f (fn [n] (inc n)) 100)
;; -> 101
```

With Lisp-2s, you have to use `funcall` to call a function bound to a variable:

```emacs-lisp
;; the symbol f refers only to variable, so to use it as a function contained in
;; f, you have to use funcall
(defun call-f (f)
  (funcall f 100))

(call-f (lambda (n) (1+ n)) 100)
;; -> 101
```

There are other differences to explore in the a future post, but let's get back to tweaking our command!

### Quoting function names

Attempting to evaluate `cam/preview-markdown` will result in an error:

```emacs-lisp
cam/preview-markdown
*** Eval error ***  Symbol’s value as variable is void: cam/preview-markdown
```

Instead, we can quote the symbol name so the reader doesn't attempt to evaluate
it. Emacs offers an alternative syntax for quoting symbols that refer to
functions:

```emacs-lisp
#'cam/preview-markdown
```

Just as `'x` is shorthand for `(quote x)`, `#'x` is shorthand for `(function x)`.

In many cases, using `quote` for function names will still work correctly:

```emacs-lisp
(add-hook 'after-save-hook 'cam/preview-markdown)
```

But `function` is preferred over `quote` for symbols that name functions for a
couple of reasons: it's clearer and more explicit, and the compiler is better
able to optimize `function` forms.

# Final Thoughts

Complete source for the final version can be found at this [GitHub
Gist](https://gist.github.com/camsaul/71d8d8c3e9c1cc4e0a3ee2d4b04d0fef). Please
feel free to leave comments or suggestions there!

If you enjoyed these posts and have money burning a whole in your pocket,
consider buying me a cup of coffee at [GitHub
Sponsors](https://github.com/sponsors/camsaul). If there's enough positive
feedback from these posts, I'll be sure to add more!
