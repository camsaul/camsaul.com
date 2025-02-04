---
layout: post
title:  "My Experience Using Prolog in 2024 to Solve Advent of Code"
date:   2025-02-04T18:37:00
categories: prolog advent-of-code
---

This was my first year doing [Advent of Code](https://adventofcode.com/2024). Previous years I had other stuff I wanted
to do with my free time besides sit at a computer even more than I already do... after seeing some of my coworkers post
epic solutions I got FOMO and joined the challenge.

I decided to try to solve things in Prolog to "kill two pigs with one bird" as the Gen Zs say. Green pig #1 was learning
Prolog at more than just a really superficial level... I'm a big fan of logic programming languages and Prolog has been
on my list for a while. Green pig #2 was I thought it would be a good fit for solving some of these challenges since I'd
(in theory) mostly be encoding the rules and letting the computer do the work for me. (Foreshadowing: it was not really
this simple in practice.) A bonus pig was flexing on everyone with 1337 skillz which I think I did actually achieve a
little bit, at least I like to think so.

I made it all the way to Day 18 before I got sidetracked with family get-togethers and what not. I still think I'm going
to go back and finish the last week one of these days tho. At least before AoC 2025 hopefully, if the world still exists
by then. Checc out my code [on GitHub](https://github.com/camsaul/advent-of-code-2024/)

Someone convinced me to Twitch stream myself solving the challenges. I really didn't believe anyone would want to sit
around and watch someone program but I guess on the off chance any of the youfs in college might find it useful to see
how the "pros" (legends?) work I went ahead and did it... I actually had a lot of people tune in, and no hecklers. I
might be the first person to ever live stream coding in Prolog on Twitch. Will I be doing more of this in the future?
Maybe. My dream is to be a content creator/influencer and get free merch and a blue checkmark next to my name. Every day
I wake up and hang my head in disgust because I am only a minor local celebrity and my invite to the Met Gala gets lost
in the mail most years. Gotta stay on that grind!!!!

Anyways you can checc out my Twitch [here](https://www.twitch.tv/camsaul). Will I be doing more streaming in the future?
Was the moon landing faked? Scientists don't have answers to life's two biggest questions yet, but keep your eyes
peeled.

I also posted videos of some of the live streams on my [YouTube](https://www.youtube.com/@camsaul) for your viewing
pleasure.

## TL;DR How Was Using Prolog Anyway?

Some of the challenges this year were super easy to solve in Prolog, but others really were not. There was an excessive
amount of 2-D grid stuff this year, and Prolog is just really not great at this stuff. (I used [SWI
Prolog](https://www.swi-prolog.org/), and maybe one of the other implementations is better, but as a beginner I found
this easy to get started and it had a great Emacs integration.) There is no native array type in Prolog, so the
idiomatic way of representing a grid is probably a linked list of linked lists... this is fine for small grids but the
polynomial lookup/modification times really bite you in the butt when the grid is something larger like 2000 ✕ 2000 and
you have to do thousands or even millions of lookups.

At the end of it I spent way more time trying to get my Prolog code to run quickly than I would have with a language
that had a better selection of data types and libraries. I wanted to change my grid code to use [bit
sets](https://en.wikipedia.org/wiki/Bit_array) instead of lists-of-lists so I could use bit arithmetic for O(1) lookup
and modification, but there was no bit set data type or library (that I know of) in SWI prolog... so I had to [implement
a hacky version myself](https://github.com/camsaul/advent-of-code-2024/blob/master/bitset_grid_util.prolog) using big
integers. A lot faster than lists-of-lists for sure, but if I was writing this in Clojure I could have just used
something like [`java.util.BitSet`](https://docs.oracle.com/javase/8/docs/api/java/util/BitSet.html). This story kept
repeating itself... I had to [implement A*
myself](https://github.com/camsaul/advent-of-code-2024/blob/master/a_star.prolog)

Almost every challenge had me coming up with an elegant and straightforward working solution (for the examples)
relatively quickly that was just way too slow for the actual challenge input. I then had to spend time reworking my
pretty code and making it faster and uglier in order to complete the actual input in a reasonable amount of time.

I think like the semantics and power of Prolog a lot better than
[miniKanren](https://minikanren.org/)/[core.logic](https://github.com/clojure/core.logic). I really enjoyed wrapping my
head around Prolog, and really wish it was more practical to use for more things.

### Pits

I think the biggest missing things for me are:

* A real lack of built-in data types... if Clojure can have immutable arrays then why can't Prolog?

* A lack of libraries -- why am I wasting time implementing A* myself?

* Fractured ecosystem -- libraries and code written for SWI Prolog are incompatible with GNU Prolog or SICStus or
  Scryer.

* A lack of easy escape hatches to drop to lower-level stuff -- I wish I could have dropped to something
  `java.util.BitSet` or a raw byte array to implement my bitset code. I don't even know how I would go about
  implementing a custom data type, I'm sure it would require C code but I don't have a good sense of how well supported
  extensions like that are.

* Questionable performance. I think it's way to easy to shoot yourself in the foot here and write code that is too slow
  for real-world use... I spent way to much time performance tuning my code.

* Some basic things like lambdas (anonymous functions/closures) are missing from the core language, and you can use
  various competing libraries that utilize operator overloading to add support for them... the syntax is ugly and sad.
  Prolog syntax is not terrible but I would have preferred writing it in S-expressions.

* I wish [constraint logic programming](https://en.wikipedia.org/wiki/Constraint_logic_programming) was more generalized
  and not limited to specific libraries that apply constraints to specific data types. What if this was the way things
  worked normally instead of being something you have to opt in to?

* Thinking in terms of relations instead of functions really is harder, and made my head hurt sometimes. Generally not
  too bad once I got used to it, but it was a little sad when I had to rework some of my relations so they would work
  efficiently if say the first parameter was ungrounded versus the second -- this sort of "extra-logical" code needed to
  make things work efficiently takes away from the magic of pure logic programming

### Peaks

* The promise of writing declarative rules-based code (tell the computer what you want instead of how you want it to do
  something) is so alluring it still makes up for the harsh reality of actually using things in a lot of cases.

* Pattern matching is amazing -- this makes me want to use something like [Clojure
  `defun`](https://github.com/clojusc/defun) all over the place

* SWI Prolog's Emacs integration, [Sweep](https://github.com/SWI-Prolog/packages-sweep), is really good, and debugging
  my code was really easy -- especially compared to something like core.logic, which in my experience was a real
  nightmare to debug.

* I really like Prolog's built-in [definite clause grammar](https://en.wikipedia.org/wiki/Definite_clause_grammar)
  parsing capabilities. They worked really well for parsing the input for most of the challenges.

* I used Microsoft Copilot to tell me how to do basic stuff in Prolog, and it generally gave me pretty good answers --
  better than I expected given how niche Prolog is. I guess 50 years of Prolog code to train on worked well here. And
  while more JS code probably gets written in a given day than all the Prolog code ever written, because Prolog is so
  niche the AIs are probably training mostly on good code.

* I am happy Prolog is not as dogmatic about pure logic programming as miniKanren. To me it seemed like the Common Lisp
  vs. Scheme -- the former is for actually solving real problems and the latter is more for writing research papers and
  admiring the elegance of your solutions.

* There are lots of good books about learning Prolog from "back in the day"... they don't all cover a lot of modern
  stuff like CLP but they did a pretty good job of helping me wrap my head around thinking about things in a Prolog way
  (as opposed to "writing C in any language").

### Other thoughts

* I "learned" Prolog in a programming languages course in college, and wow did they really sell the language short. I
  really did not fully understand the power until I spent a month doing self-study

* The [Mercury language](https://mercurylang.org/) looks really interesting and makes bold promises about performance
  but it doesn't seem to have a lot of resources available for learning it.

* I would really love if there was a nice implementation of Prolog in Clojure that let me use Clojure goodness like
  S-expressions and macros and drop-down to low-level Java when needed. I think there are a few JVM implementations of
  Prolog, but I'm not sure what state they're in. I spent a little time playing around with things and got a basic
  Prolog implementation working in Clojure... it's honestly not rocket science. If I get more free time I might try to
  play around with this more and see if I can turn it into something actually useful. Maybe I can use it for AoC 2025.

* I know back in the day a lot of the Common Lisp people played around with implementing Prolog in CL -- I even have a
  few books on my bookshelf that walk thru how to write an implementation. Time to do some reading.

👇 Like and subscribe below! 👇
