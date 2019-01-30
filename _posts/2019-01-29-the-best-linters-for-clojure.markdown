---
layout: post
title: "The Best Linters for Clojure"
date: 2019-01-29 04:47:49 -0800
categories: clojure
---

Clojure linters have saved me a ton of time when managing huge open-source projects like [Metabase](https://github.com/metabase/metabase). It's a lot easier to enforce coding conventions
by having CI do it for you.

Linters didn't existing for some of the things I wanted to lint, so I invested a bit of time writing a few of my own. This turned out to be time well spent; the up-front time I spent writing the
linters was eventually repaid in the time I saved asking for people to fix things (or going in and fixing them myself).

I'll have to write a blog post in the future about how to write Clojure linters, but in the meantime here's the four linters I use in every Clojure project:

### The best Clojure linters

*  [Eastwood](https://github.com/jonase/eastwood) -- Probably the single best Clojure linter. Good all-around linter that checks for things like misplaced docstrings, shadowing `clojure.core` vars,
   and unused private vars.

*  [Bikeshed](https://github.com/dakrone/lein-bikeshed) -- Another great linter that offers file formatting checks that Eastwood does not. Can check for lines with trailing whitespace, lines
   longer than some predetermined max line length, trailing blank lines at the end of a file, etc.

*  [lein-docstring-checker](https://github.com/camsaul/lein-docstring-checker) -- As they say, code is write-once/read-many, i.e. you write a line of code once, but you and others read it perhaps
   hundreds or thousands of times after that (this is especially true of open source projects, and even more especially true of libraries). What a given line of code is doing/for might be clear
   when you write it, but somebody else or even your future self will not know what was going on in your head at that moment in time. Someone else might have to fix a bug in that code years from now;
   this is a much harder task when they need to spend hours working out what the code does. So do yourself and others a favor and write what's going on and why when you write your code.

   I haven't figured out how to write a linter that checks whether code comments are well-written yet, but `lein-docstring-checker` (written by yours truly) is a good first step in keeping your code
   well-documented. The linter requires every public var to have a docstring. So at the very least any code in your project that might be used in more than one namespace will have an explanation of what
   it does attached to it.

   As a nice side-effect, having the documentation requirement helps keep code cleaner and clearer; in the process of writing docstrings for functions, I often think of simpler or more flexible versions
   of the same function. Writing documentation for code really helps you pause and think clearly about the problem you are trying to solve and the purpose of the code you wrote for doing so.

*  [lein-check-namespace-decls](https://github.com/camsaul/lein-check-namespace-decls) -- Another linter written by yours truly. This Clojure linter checks whether your namespace declarations (i.e.,
   `ns` forms) are sorted properly, use `:require` instead of `:use`, and don't contain unused namespaces. This is more of a code prettiness thing than anything else, but I personally prefer nice-looking
   code over ugly code, and keeping `ns` forms organized helps keep the code looking nice.

### Honorable mentions

*  [Kibit](https://github.com/jonase/kibit) -- Suggests ways to write Clojure code more idiomatically, e.g. using `when` instead of `if` if there is no `else` form. Unfortunately it seems to complain
   a lot about using threading macros (e.g. `->` and `->>`) with only two args, which I like to do for readability in some situtations; I haven't figured out how to disable this yet, so I'm not currently
   using Kibit on any of my projects. I might have to look at implementing a fix for [Kibit issue #148](https://github.com/jonase/kibit/issues/148) so I can use this linter.

### Other thoughts on Clojure Linters

I think use of strict linter regimes indirectly improves code quality by giving contributors a sense that the code is expected to be very high quality. Not just any slapped-together code will do;
it should be well-tested, written in a idiomatic/conventional way, well-documented, and well-thought-out. Such carefully considered code tends to be cleaner and less buggy.

When I submit a PR for a personal project or someone else's, I usually read over the entire diff two or three times; making small improvements to things with each pass; sometimes I miss things, and the
linters catch them for me. Unfortunately I don't think most people are in this habit, and I've seen a lot of PRs where unused code gets left in or where weird indentation never gets fixed. For PRs
that haven't been given as much attention to detail, the linters can help nudge people into taking a second look at things as they make the tweaks needed to get CI to pass.
