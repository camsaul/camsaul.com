---
layout: post
title:  "Humane Are Just Dropped!"
date:   2022-09-01T20:40:00
categories: announcement
---

# Hello, world. It's me, your friendly neighborhood #cam.

You may have been concerned about my whereabouts for the last 2 years, but I was doing the HUMANE thing and working on
a new library for you all... In my free time while I'm not being a full time #birddad I have been working on a fun new
project that will help you write tests. Let me break it down.

I've been using some version of this or another for a while now; I thought it was finally time to spin it out into a
separate library, in case anyone else found it useful. `clojure.test/are` is great for writing lots of assertions
quickly, but it has two big problems that prevent me from using it everywhere:

1. Failing assertions give no indication as to which set of arguments failed if you're using anything that pretty
   prints test output, such as [Humane Test Output](https://github.com/pjstadig/humane-test-output),
   [CIDER](https://github.com/clojure-emacs/cider), or [eftest](https://github.com/weavejester/eftest)
2. `are` lets you shoot yourself in the foot by writing expressions that include `is` or `testing`, and wraps them in
   another `is` without complaining

You can learn more about it on [GitHub](https://github.com/camsaul/humane-are).

I hope everyone finds this as useful as I hope I will.

Peace, love, and bird seed.

xoxo CamSaul
