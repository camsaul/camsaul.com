---
layout: post
title:  "Methodical now supports partial default methods"
date:   2020-04-22 20:36:00
categories: announcement
---

[Methodical](https://github.com/camsaul/methodical), my Clojure library that ports the "generic function" part of the
Common Lisp Object System (CLOS) to Clojure (and makes multimethods *way* more powerful) just got even more powerful.
Methodical now supports dispatch on partial defaults!

### What does that even mean?

Take a look at this Clojure code:

```clj
(defmulti vanilla-multimethod
  identity)

(defmethod vanilla-multimethod [:x :y]
  [_]
  :x-y)

(defmethod vanilla-multimethod [:default :y]
  [_]
  :y-default)

(defmethod vanilla-multimethod :default
  [_]
  :default)

(vanilla-multimethod [nil :y]) ; -> :default
```

Vanilla multimethods support exactly one default method, and that's it. If you want to have a method for any dispatch
value where the second value is `:y`, you are completely out of luck. The best you can do is some hackery like this:

```clj
(defmethod vanilla-multimethod :default
  [[x y]]
  (let [default-method   (get-method vanilla-multimethod :default)
        x-default-method (get-method vanilla-multimethod [:default y])
        y-default-method (get-method vanilla-multimethod [x :default])]
    (cond
      (not= x-default-method default-method)
      (x-default-method [x y])

      (not= y-default-method default-method)
      (y-default-method [x y])

      :else
      :default)))

(vanilla-multimethod [nil :y]) ; -> :y-default
```

That's something I've actually done in real life, in production!

### Why?

There's some real-world use cases where you'd want partial defaults. When writing
[Metabase](https://github.com/metabase/metabase/)'s query processsing code for JDBC databases, I wanted to write a
multimethod for reading columns in a `ResultSet` that dispatched on both the database and the JDBC type. For example,
we have a default method for reading `java.sql.Types/TIME_WITH_TIMEZONE` columns like:

```clj
(defmethod read-column-thunk [:default java.sql.Types/TIME_WITH_TIMEZONE]
  ...)
```

For some databases, we override this method like:

```clj
(defmethod read-column-thunk [:postgres java.sql.Types/TIME_WITH_TIMEZONE]
  ...)
```

So what happens when we call `read-column-thunk` with the dispatch value `[:mysql java.sql.Types/TIME_WITH_TIMEZONE]`?
By default, it doesn't work -- it will use the `:default` method rather than the `[:default java.sql.Types/TIME_WITH_TIMEZONE]` method
as we would like.

### Can't I use hierarchies?

The astute reader will note that you can create a keyword hierarchy and have both `:postgres` and `:mysql` derive from
a common parent keyword (e.g. `:database`). That's certainly true! But creating a hierarchy (or using the global
hierarchy with namespaced keywords) just for the sake of a multimethod is annoying, and it *still* wouldn't handle cases
like `[nil java.sql.Types/TIME_WITH_TIMEZONE]`, where the first part of the dispatch value doesn't derive from the
common parent. It doesn't work as a true fall-thru for any `java.sql.Types/TIME_WITH_TIMEZONE`.

### Enter Methodical

Methodical now supports partial defaults! Here's how to do it in Methodical:

```clj
(require '[methodical.core :as m])

(m/defmulti my-multimethod
  identity)

(m/defmethod my-multimethod [:x :y]
  [_]
  :x-y)

(m/defmethod my-multimethod [:default :y]
  [_]
  :y-default)

(m/defmethod my-multimethod :default
  [_]
  :default)

(my-multimethod [nil :y]) ; -> :y-default
```

The new `multi-default-dispatcher` is the default dispatcher, but with Methodical [you can customize method
dispatch](https://github.com/camsaul/methodical#advanced-customization) and have it work any way you'd like!

### How Can I Learn More?

[I'm giving a talk about Methodical at Clojure/north this year](https://clojurenorth.com/cam-saul.html), so stay
tuned! As far as I know, Clojure/north is going to be a remote conference due to Coronavirus concerns, so you can
attend from anywhere in the world!

👇 Like and subscribe below! 👇
