import java.time.Instant
import java.util.Date

import Step1.Types._
import pprint.log

object Step2 {
  // So what can we do?
  // Well, we can start by trying to wrap up this idea of retrieving and updating a structure in to a pair of functions
  trait Lens[S, A] {
    val get: S => A
    val set: A => S => S // Tee-hee
  }

  final case class Person(name: String, dateOfBirth: Date, favouriteColour: Colour, address: Address)

  val personAddressL: Lens[Person, Address] = new Lens[Person, Address] {
    val get: Person => Address            = _.address
    val set: Address => Person => Person  = a => p => p.copy(address = a)
  }

  val threeFourOne = Address(None, 341, "George St", "Sydney", NSW, Australia)
  val mikey = Person("Mikey", Date.from(Instant.ofEpochSecond(698862000)), Green, threeFourOne)

  // And it works!
  log(s"What is Mikey's address? ${personAddressL.get(mikey)}")

  val personNameL: Lens[Person, String] = new Lens[Person, String] {
    val get: Person => String           = _.name
    val set: String => Person => Person = s => p => p.copy(name = s)
  }

  // Setting should also work
  log(s"What if Mikey used his real name? ${personNameL.set("Miketholemeu")(mikey)}")

  val addressStateL: Lens[Address, State] = new Lens[Address, State] {
    val get: Address => State             = _.state
    val set: State => Address => Address  = s => a => a.copy(state = s)
  }

  // And we can chain these gets together
  def getPersonsState = (personAddressL.get andThen addressStateL.get)
  log(s"Which State is he in? ${getPersonsState(mikey)}")


  // This seems more structured than our previous go at wrapping this idea up in
  //  to reusable functions. It doesn't seem much better...
  // Especially since we still have to specially compose the gets and sets.
  // Well, since we're abstracting out this idea, we can actually just write that composition once


  implicit class LensComposition[S, A](l: Lens[S, A]) {
    def &[B](r: Lens[A, B]): Lens[S, B] = new Lens[S, B] {
      val get: S => B       = l.get andThen r.get
      val set: B => S => S  = inner => outer => l.set(r.set(inner)(l.get(outer)))(outer)
    }
  }

  val personsStateL = personAddressL & addressStateL

  // Wow, that's a lot neater.
  // Especially if you consider that we can automatically generate such lenses!
  // <hold for applause>
  // No applause? RUDE!
  // <drink beer>

  log(s"\n\nJust what state is mikey in again? ${personsStateL.get(mikey)}")
  log(s"What if Mikey was on holiday? ${personsStateL.set(QLD)(mikey)}")




  // We've really been limiting ourselves here.
  // Who says that when we focus in on a structure we are only looking at fields?
  val average: Lens[List[Double], Double] = new Lens[List[Double], Double] {
    val get: List[Double] => Double = l => l.sum / l.length
    val set: Double => List[Double] => List[Double] = d => _ => List(d)
  }

  // Show averages
  val nums: List[Double] = List(1,1,2,3,5,8,13)
  log(s"The avg of nums is ${average.get(nums)}")

  // Is this legal?
  // Lenses have LAWS!!

  // I'll just steal these from Monocle's website
  // If you get a value from the structure, then set it, it's basically a noop
  def getSet[S, A](l: Lens[S, A], s: S): Boolean =
    l.set(l.get(s))(s) == s

  // If you set a value, then get it, you should get the same value back
  def setGet[S, A](l: Lens[S, A], s: S, a: A): Boolean =
    l.get(l.set(a)(s)) == a

  // Is our average Lens lawful?
  log(s"getSetLaw: ${getSet(average, nums)}")
  log(s"setGetLaw: ${setGet(average, nums, 3.0)}")

  // Hmm, so this means we have half a Lens? What does that mean?
  // There's actually an Optics hierarchy!
}
