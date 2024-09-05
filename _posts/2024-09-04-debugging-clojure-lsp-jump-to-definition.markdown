---
layout: post
title:  "Debugging Custom Kondo Hooks and Clojure LSP Jump-to-Definition Functionality"
date:   2024-09-03T15:13:00
categories: clojure
---

At [Metabase](https://github.com/metabase/metabase) I wrote [a macro called
`defendpoint`](https://github.com/metabase/metabase/blob/339ff2433d4a14f2d153d211746b75bf334f798b/src/metabase/api/common.clj#L384-L417)
for defining (you guessed it!) REST API endpoints with a typical usage that looks something like

```clojure
(api/defendpoint POST "/:id/copy"
  "Copy a `Card`, with the new name 'Copy of _name_'"
  [id]
  ;; Malli schemas defined after the argslist are used for validation
  {id [:maybe ms/PositiveInt]}
  (let [orig-card (api/read-check Card id)
        new-name  (str (trs "Copy of ") (:name orig-card))
        new-card  (assoc orig-card :name new-name)]
    (-> (card/create-card! new-card @api/*current-user*)
        (assoc :last-edit-info (last-edit/edit-information-for-user @api/*current-user*)))))
```

I was spending a lot of time scratching my head wondering why [Clojure LSP](https://clojure-lsp.io/) jump-to-definition
functionality wasn't working as expected inside `defendpoint`. I wrote a [custom clj-kondo
hook](https://github.com/clj-kondo/clj-kondo/blob/master/doc/hooks.md) for it, but moving my cursor to something like
`api/read-check` and doing something `M-x lsp-find-definition` would just take me to the start of the `defendpoint` form
rather than what I was expecting.

Debugging this issue was pretty hard -- while there was clearly something wrong with my custom Kondo hook, but I
couldn't figure out what it was.

In the end I finally managed to figure out how to debug these sorts of things so I'm writing down my steps in case
anyone else runs into the same problem.

## Add Clojure LSP as a dependency

I added this to my `deps.edn`:

```clojure
{:aliases
 {:lsp
  {:extra-deps {com.github.clojure-lsp/clojure-lsp {:mvn/version "2024.08.05-18.16.00"}
                dev.weavejester/cljfmt             {:git/url "https://github.com/weavejester/cljfmt"
                                                    :git/sha "434408f6909924f524c8027b37422d32bb49622d"}}}}}
```

Note that at the time of this writing because of
[clojure-lsp/clojure-lsp#1867](https://github.com/clojure-lsp/clojure-lsp/issues/1867) I had to add a fake dependency
for [cljfmt](https://github.com/weavejester/cljfmt) to make this work as well -- I just used the latest Git SHA at the
time of this writing.

## Launch REPL with the `:lsp` alias

In Emacs I used

```
C-u M-x cider-jack-in
```

to let me edit the command it used to launch the nREPL to add the `:lsp` alias. Note that I launched the REPL from the
same project I was debugging.

## Debug from the REPL

This is the code I came up with while poking thru Kondo and LSP internals to debug things:

```clojure
(require 'clj-kondo.hooks-api
         'clojure-lsp.kondo
         'clojure-lsp.queries
         'clojure.java.io)

(defn find-element [path line col]
  (binding [clj-kondo.hooks-api/*reload* true]
    (letfn [(analysis [path]
              (let [db*              (atom {})
                    config           nil
                    file-analyzed-fn identity]
                (clojure-lsp.kondo/run-kondo-on-paths! [path] db* config file-analyzed-fn)))
            (db-with-analysis [path]
              (clojure-lsp.kondo/db-with-analysis {} (analysis path)))]
      (let [db  (db-with-analysis path)
            uri (str "file://" (.getAbsolutePath (clojure.java.io/file path)))]
        (clojure-lsp.queries/find-element-under-cursor db uri line col)))))
```

Binding `clj-kondo.hooks-api/*reload*` is important so Clojure LSP will reload your Kondo config (e.g., custom hooks)
after you make changes to it.

The basic idea here is to run Kondo on a specific file `path` and then pass that analysis along to LSP which can find
the element at a given position in a file based on that analysis.

All you need is a file name to debug and a line number and column number that positions the cursor over the symbol you
want to debug.

Example usage:

```clojure
(let [path "src/metabase/api/card_2.clj"
      line 17
      col  21]
  (some-> (find-element path line col)
          (select-keys [:name :alias :from :from-var :to])))
;;=> {:name read-check, :alias api, :from metabase.api.card-2, :from-var POST_:id_copy, :to metabase.api.common}
```

If your Kondo hook is broken then this will either return nothing or return the wrong information. If the information
looks something like the example result above then things should work as expected.

## What was broken?

My original version of the hook returned when I used the `find-element` code above.

```clojure
{:name POST_:id_copy}
```

My hook basically looked like this (simplified a bit):

```clojure
(ns hooks.metabase.api.common
  (:require
   [clj-kondo.hooks-api :as api]
   [clojure.string :as str])

(defn route-fn-name
  "route fn hook"
  [method route]
  (let [route (if (vector? route) (first route) route)]
    (-> (str (name method) route)
        (str/replace #"/" "_")
        symbol)))

;;; Original custom Kondo hook -- BROKEN!!
(defn defendpoint
  [arg]
  (letfn [(update-defendpoint [node]
            (let [[_defendpoint method route & body] (:children node)]
              (api/list-node
               (list
                (api/token-node 'do)
                (api/token-node (symbol "compojure.core" (str (api/sexpr method))))
                (-> (api/list-node
                     (list*
                      (api/token-node 'clojure.core/defn)
                      (api/token-node (route-fn-name (api/sexpr method) (api/sexpr route)))
                      body))
                    (with-meta (meta node)))))))]
    (update arg :node update-defendpoint)))
```

which means the example at the beginning would be transformed in a node representing a form like this:

```clojure
;;; Output of custom Kondo hook
'(do
   compojure.core/POST
   (clojure.core/defn POST_:id_copy
     "Copy a `Card`, with the new name 'Copy of _name_'"
     [id]
     {id [:maybe ms/PositiveInt]}
     (let [orig-card (api/read-check Card id)
           new-name (str (trs "Copy of ") (:name orig-card))
           new-card (assoc orig-card :name new-name)]
       (-> (card/create-card! new-card @api/*current-user*)
           (assoc :last-edit-info (last-edit/edit-information-for-user @api/*current-user*))))))
```

We're generating a `do` form here so we can capture the usage of `compojure.core/POST` so Kondo doesn't think it's
unused if it's in our `:require` form in the `ns` declaration.

Side note: this is mentioned in the Kondo docs but you can verify your Kondo hook output looks correct on the surface
level by Kondo itself as a dep and using `clj-kondo.hooks-api/parse-string` and `clj-kondo.hooks-api/sexpr`:

```clojure
;;; debugging custom Kondo hooks
(def form
  '(api/defendpoint POST "/:id/copy"
     "Copy a `Card`, with the new name 'Copy of _name_'"
     [id]
     {id [:maybe ms/PositiveInt]}
     (let [orig-card (api/read-check Card id)
           new-name  (str (trs "Copy of ") (:name orig-card))
           new-card  (assoc orig-card :name new-name)]
       (-> (card/create-card! new-card @api/*current-user*)
           (assoc :last-edit-info (last-edit/edit-information-for-user @api/*current-user*))))))

(-> {:node (-> form
               pr-str
               api/parse-string)}
    defendpoint
    :node
    api/sexpr)
```

I've actually started putting stuff like these in tests to make sure I don't break my dozens of custom Kondo hooks.

Anyways "looks correct at a surface level" is key here. Because while this output looks correct, what you do with the
metadata (specifically stuff like start and end line and column numbers) from the original nodes is actually super
important to Clojure LSP. (They're important to Kondo too to make sure warnings pop up in the right place.)

If you want to see what this metadata looks like in your REPL you can

```clojure
(set! *print-meta* true)
```

or just bind `*print-meta*` and use something like `clojure.pprint/pprint` e.g.

```clojure
(binding [*print-meta* true]
  (-> {:node (-> form
                 pr-str
                 api/parse-string)}
      hooks.metabase.api.common/defendpoint
      :node
      api/sexpr
      clojure.pprint/pprint))
```

The output is actually super noisy so I'll leave it out here.

The fixed version of the hook actually has the exact same output aside from some changes to
metadata.

Here's the fixed version of the hook above:

```clojure
;;; custom Kondo hook -- FIXED
(defn defendpoint
  [arg]
  (letfn [(update-defendpoint [node]
            (let [[_defendpoint method route & body] (:children node)]
              (-> (api/list-node
                   (list
                    (api/token-node 'do)
                    (-> (api/token-node (symbol "compojure.core" (str (api/sexpr method))))
                        (with-meta (meta method)))
                    (api/list-node
                     (list*
                      (api/token-node 'clojure.core/defn)
                      (-> (api/token-node (route-fn-name (api/sexpr method) (api/sexpr route)))
                          (with-meta (meta route)))
                      body))))
                  (with-meta (meta node)))))]
    (update arg :node update-defendpoint)))
```

The important change here was to give the generated function name symbol `POST_:id_copy` the metadata (line and column
start and end numbers) from the original `"/:id/copy` string. It seems like without knowing where a function name is
defined in the text file (or rather, where it is restricted to) LSP for whatever reason was assuming the entire
`defendpoint` form served as a the function name which meant any attempt to find the definition under the cursor
anywhere in that form would assume you were trying jump to the function definition itself.

Other changes I made which didn't seem to fix anything but seem more correct are:

* `compojure.core/POST` now gets the metadata (line and column start and end numbers) from the original `POST` symbol
  (named `method`) from the original node

* The overall new top-level list node gets the metadata from the original top-level node rather than putting it on the
  generated `defn` node. Not sure this makes a huge difference TBH.

## Conclusion

Custom Kondo hooks are powerful and worth the time it takes to wrap your head around
[rewrite-clj](https://github.com/clj-commons/rewrite-clj). Debugging them is tricky, and debugging them when used by LSP
is even trickier. I haven't seen it documented anywhere before, but it is possible. I hope you find this useful!
