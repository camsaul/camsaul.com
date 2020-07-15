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
