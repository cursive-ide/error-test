# error-test

This is the code I demoed in my [Conj 2015 talk](https://www.youtube.com/watch?v=kt4haSH2xcs) about improving Clojure's error messages with macro grammars. 

## Status

A few people have asked about this so I put it up here, but it's in an as-is where-is state. Allegedly something like this may be in Clojure 1.9 so I won't update this until there's some indication of what that will look like.

Also note that this is based on a branch of Cursive that I've written off as more or less a failure, in particular defining the whole grammar as a single data structure is not ideal. This was experimental after discussions with Ghadi Shayban who has also been working on some PEG parsing over [here](https://github.com/ghadishayban/pex). 

*Notes from an email I sent about this:*

The version that Cursive currently uses has a combinator-like approach, so the non-terminals are declared like this:

```clojure
(defn arities []
  (alt (item :into :arities
         (vector :as :arglist
           (parameters)))
       (repeat (item :into :arities
                 (list :as :arity
                   (vector :as :arglist
                     (parameters)))))))
```

i.e. they’re just straight functions which return parsers which are exactly the parsers created by parse/compile-parser in this version. This has some nice benefits which I’ve come to realise after experimenting with the data structure approach: you get completion and navigation support for free, the parsers can have docstrings etc. It’s also easier to use an interactive REPL-based workflow since you can reload a single grammar and don’t have to recompile everything. Ghadi compiles down to a parsing VM so he’s essentially AOTing everything and has to recompile everything anyway, but I don’t and this is annoying during development.

However the biggest benefit of having them be normal vars is that they can take advantage of Clojure’s namespacing. I have many namespaces for patterns in third-party libs which reuse the destructuring grammar, for example. In order to do something similar with the data structure approach I was basically looking at reimplementing a namespace system, which is daft. So I think I’m going to consider that a failed experiment.

That said, there are some benefits which were pretty important. Using the defn approach, I don’t have the non-terminal name or access to the structure of the grammar, which means that I can’t pretty print the grammar. I was running pretty close to time during my talk and totally forgot to demo this - when you get the doc for a function in the experimental Cursive version, you also get a pretty-printed grammar along with the current docstring. This is really nice. This abstract representation of the grammar (as opposed to just a collection of functions which you get with the defn version) can also be used for context-sensitive completion and so forth, so it’s a huge win. I also want to be able to record source locations of grammar elements for error reporting and so that I can eventually have a stepping debugger for these parsers.

I think the best solution is going to be to use a macro here, defsyntax or something similar. I generally only use def* macros as a very last resort, but in this case I think it’s justified. The macro could create the functions exactly as the defn version does, but could attach metadata to the functions to allow the grammar structure to be extracted independently of execution. This would permit solutions to all those problems except for the stepping debugger - for that I need to be able to control execution, so I’d really need an abstract parsing machine of some kind unless I implement it as a layer on top of the JVM debugger. I’m not sure about this, it needs a lot more thought. 

## License

Copyright © 2015 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
