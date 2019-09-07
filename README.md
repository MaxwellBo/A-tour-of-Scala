# A tour of Scala

> Scala is what happens when somebody takes OCaml, mixes in some Haskell, adds some Java coloured sprinkles in the mix, and and randomly shoves the whole thing together into a garbage disposal and lights itbefore running away laughing maniacally.

> Which is to say, Scala is a bipolar, flawed, genius with a massive ego and huge ambitions. It is both utterly brilliant and utterly terrible at the same time, with the "Utterly Brilliant" shining more brightly. It is perhaps the programming language equivalent of Roman Polanski.

> More precisely, Scala is a functional programming language more closely related to SML or Ocaml, with some improvements to support something resembling Haskell style typeclasses and monadic programming that were, perhaps, completely by accident.

> It can be used surprisingly effectively as an imperative object orientated language, but it's a functional programming language at heart, and this is where its power lies.

~ [2bdb2](https://www.reddit.com/r/programming/comments/82wpiw/the_redmonk_programming_language_rankings_january/dvf316x/) 

---

Over the last 3 years, there's been times where I've needed to explain various functional concepts as they exist in Scala.  Typically, all that functional goodness is locked up in libraries like [Cats](https://github.com/scalaz/scalaz) and [Scalaz](https://scalaz.github.io/7/). Unfortunately, these libraries are labarinythian - a tall tower of abstractions, obscured by hacks to improve syntax ergonomics and performance. Their source code is nothing like the quite readable [Base](http://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.Base.html) package of Haskell.

This is my take on it. It's a [single file](https://github.com/MaxwellBo/A-tour-of-Scala/blob/master/src/main/scala/Main.scala) designed to be presented in a lecture format, scrolling start to finish. It is _not_ an attempt to teach functional programming^ from the ground up, but to teach developers who might need to briefly contribute to Scala production codebases enough to get by. It's also designed to be a useful as a 1-on-1 teaching tool, allowing the quick creation of copy-pastable example code, with little indirection for maximal `Go to definition`-ability. 

Feel free to contribute! If there's an abstraction that you think would be useful to document in here, open a PR. I already have stripped down examples of `Free`, and a couple of the standard monad transformers. Maybe lenses? 

^ If you do want to learn functional programming from the ground up, I personally recommend [Learn You a Haskell](http://learnyouahaskell.com/) or the [QFPL Applied Functional Programming course](https://github.com/qfpl/applied-fp-course). 


## Running

- `sbt run` in the root directory
