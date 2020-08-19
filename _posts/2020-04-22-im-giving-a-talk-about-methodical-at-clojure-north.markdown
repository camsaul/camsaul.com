---
layout: post
title:  "I will be giving a talk about Methodical at Clojure/north this year"
date:   2020-04-22 20:26:00
categories: methodical
---

If I was better at updating my blog I would have posted this months ago. But better late than never!

[I'm giving a talk about a library I wrote called Methodical at Clojure/north
2020.](https://clojurenorth.com/cam-saul.html) [Methodical](https://github.com/camsaul/methodical) ports the "generic
method" parts of the Common Lisp Object System (CLOS) to Clojure in a Clojurey way. As far as I can tell, it's the
best/most complete port of the CLOS to Clojure out there, even though it only ports a subset of CLOS.

### Cool, but why?

I spend a lot of time missing some of the powerful features of the Common Lisp Object System while you're writing
Clojure. Specifically, the thing I miss most is support for auxiliary methods. The [Common Lisp
Cookbook](https://lispcookbook.github.io/cl-cookbook/clos.html#method-qualifiers-before-after-around) has a good
explanation of what exactly auxiliary methods are.
