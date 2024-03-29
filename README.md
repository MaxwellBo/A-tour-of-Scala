# A tour of Scala

> Scala is what happens when somebody takes OCaml, mixes in some Haskell, adds some Java coloured sprinkles in the mix, and and randomly shoves the whole thing together into a garbage disposal and lights it before running away laughing maniacally.

> Which is to say, Scala is a bipolar, flawed, genius with a massive ego and huge ambitions. It is both utterly brilliant and utterly terrible at the same time, with the "Utterly Brilliant" shining more brightly. [...]

> More precisely, Scala is a functional programming language more closely related to SML or Ocaml, with some improvements to support something resembling Haskell style typeclasses and monadic programming that were, perhaps, completely by accident.

> It can be used surprisingly effectively as an imperative object orientated language, but it's a functional programming language at heart, and this is where its power lies.

~ [2bdb2](https://www.reddit.com/r/programming/comments/82wpiw/the_redmonk_programming_language_rankings_january/dvf316x/) 

---

Over the last 3 years, there've been times where I've needed to explain various functional concepts as they exist in Scala.  Typically, all that functional goodness is locked up in libraries like [Cats](https://github.com/scalaz/scalaz) and [Scalaz](https://scalaz.github.io/7/). Unfortunately, these libraries are labyrinthine - a tall tower of abstractions, obscured by hacks to improve syntax ergonomics and performance. Their source code is nothing like the quite readable [Base](http://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.Base.html) package of Haskell.

So recently I've:

### Implemented a Scala-flavoured `Base`...

It's a couple of largish files designed to be presented in a lecture format, scrolling start to finish.

It covers:

- Pattern matching
- Scala function calling semantics
- Scala implicit semantics
- The typeclass pattern
- Trait bound desugaring
- `Functor`, `Monad` for commonly used types (e.g. `R => ?`, `List`, `Option`)
- `for` notation desugaring
- `Monoid` and `Semigroup`
- Laziness
- A stripped down effect monad (`Sync`)
- `State`
- `Kleisli/ReaderT`
- `Free`
- `Validated`
- `Applicative`
- `FreeAp`
- Parser combinators

It does not yet cover:

- `OptionT` and `EitherT`
- Lenses
- A proper reimplementation of `Circe`

### ...and given lectures about it! 

#### Version 1

- Did not record Part 1
- [Part 2](https://www.youtube.com/watch?v=kF5MyY_7v2I)
- [Part 3](https://www.youtube.com/watch?v=nGnr61NfHac)


#### Version 2

- [Part 1](https://www.youtube.com/watch?v=XGbO7ibbrEk)
- [Part 2](https://www.youtube.com/watch?v=n2IBf5-u7ig)
- [Part 3](https://www.youtube.com/watch?v=cUZ6mg5Y_xE)

#### Parser combinators 

- [Megaman Cross Pollination](https://www.youtube.com/watch?v=Iv9rH6xEFfQ)
- [UQCS](https://www.youtube.com/watch?v=bvjBgAGq3E8)

---
 
This project is _not_ an attempt to teach functional programming from the ground up^^ - rather an attempt to teach developers who might need to briefly contribute to Scala production codebases _just_ enough to get by. It's also designed to be a useful as a 1-on-1 teaching tool, allowing the quick creation of example code and demos, where all abstractions can be copy-pasted into the snippet or Gist after a couple `Go to definition`s.


^ I've had to make some things simpler at the cost of expressiveness. An example includes `Alternative` not extending `Applicative` to prevent implicit resolution clashes. There are ways to prevent this from happening, but it's out of scope for this kind of tutorial. 

^^ If you do want to learn functional programming from the ground up, I personally recommend [Learn You a Haskell](http://learnyouahaskell.com/), or the QFPL [Functional Programming](https://github.com/data61/fp-course) + [Applied Functional Programming](https://github.com/qfpl/applied-fp-course) courses.


## Contributing

Feel free to contribute! If there's an abstraction that you think would be useful to document in here, open a PR. I intended on this being a useful teaching tool for _any_ team, so that individuals can pick and choose the content they want to cover. 

## Running

- `sbt run` in the root directory
