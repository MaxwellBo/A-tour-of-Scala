import pprint.log

object Step3 {
  // The Optics hierarchy in Monocle is here; https://julien-truffaut.github.io/Monocle/optics.html

  // We can see that the strongest form of Optic is the Iso, short for Isomorphism.
  // This says that two types are equivalent. We can transform between the two with no loss of information

  trait Iso[S, A] {
    val get: S => A
    val reverseGet: A => S
  }

  // Some places you might see this would be in purely wrapping types, with no validation
  final case class Name(value: String)

  val nameStringI = new Iso[Name, String] {
    val get: Name => String = _.value
    val reverseGet: String => Name = Name.apply
  }

  val myName = Name("Heisenberg")

  log(s"Say my name: $myName")
  log(s"Did you mumble?: ${nameStringI.get(myName)}")
  log(s"Okay, go the other way: ${nameStringI.reverseGet("Chilli P")}")

  object IsoLaws {
    def roundTripOneWay[S, A](i: Iso[S, A], s: S): Boolean =
      i.reverseGet(i.get(s)) == s

    def roundTripOtherWay[S, A](i: Iso[S, A], a: A): Boolean =
      i.get(i.reverseGet(a)) == a
  }

  log(s"roundTripOneWay: ${IsoLaws.roundTripOneWay(nameStringI, Name("Mikey"))}")
  log(s"roundTripOtherWay: ${IsoLaws.roundTripOtherWay(nameStringI, "Mikey")}")

  // Our old friend Lens is back, so what's the difference here?
  trait Lens[S, A] {
    val get: S => A
    val set: A => S => S
  }

  object LensLaws {
    def getSet[S, A](l: Lens[S, A], s: S): Boolean =
      l.set(l.get(s))(s) == s

    def setGet[S, A](l: Lens[S, A], s: S, a: A): Boolean =
      l.get(l.set(a)(s)) == a
  }













  // The other side of the tree is Prism. This has a weakened "Get" operation.
  // I've seen some writing characterise this as being a way of analysing Sum types.
  // I'm not 100% sure what that means, and it's 1am, so not a topic for tonight
  trait Prism[S, A] {
    val getOption: S => Option[A]
    val reverseGet: A => S
  }

  // This weakened get in a very real sense signifies that even if you give me a structure S, I might not be able to
  //  retrieve the item the prism focuses on. The head of a list is a really good simple example of this. The head might
  //  not exist, but if you give me an item, I can certainly make it the head of the list.
  def headP[A]: Prism[List[A], A] = new Prism[List[A], A] {
    val getOption: List[A] => Option[A] = _.headOption
    val reverseGet: A => List[A] = h => List(h)
  }

  object PrismLaws {
    def partialRoundTripOneWay[S, A](p: Prism[S, A], s: S): Boolean =
      p.getOption(s) match {
        case None    => true // nothing to prove
        case Some(a) => p.reverseGet(a) == s
      }

    def partialRoundTripOtherWay[S, A](p: Prism[S, A], a: A): Boolean =
      p.getOption(p.reverseGet(a)) == Some(a)
  }

  log(s"partialRoundTripOneWay: ${PrismLaws.partialRoundTripOneWay(headP[Int], List(21))}")
  log(s"partialRoundTripOtherWay: ${PrismLaws.partialRoundTripOtherWay(headP[Int], 21)}")

  //TODO: Add examples from our codebase


  // If we weaken both the getter and setter side of an Iso, we get...
  trait Optional[S, A] {
    val getOption: S => Option[A]
    val set: A => S => S
  }

  // So we had problems with composition before, and that was with only one kind of Optic! What do I do with this many
  //  Optics flying about!!

  // Answer: You write a bunch of custom composition operators... :truestory:
  // https://github.com/julien-truffaut/Monocle/blob/master/core/shared/src/main/scala/monocle/Lens.scala#L141
  // https://github.com/julien-truffaut/Monocle/blob/master/core/shared/src/main/scala/monocle/Optional.scala#L158

  // Okay, that was a bit of a whirlwind tour through some other types of Optic, but this talk was titled
  // "Profunctor Optics"...
}
