import pprint.log

object Part0 extends App {

  ///////////////////////////////////////////////////////////////////////////////
  // PREFACE
  ///////////////////////////////////////////////////////////////////////////////

  // If you haven't already, go to the README.md. Before we start, some
  // housekeeping.

  // 1. Things may appear in a weird order. I'm doing that so that you know every
  // thing you need to know before you get to it, so you don't see something
  // weird and think 'what the hell is that'

  ///////////////////////////////////////////////////////////////////////////////
  // Polymorphism
  ///////////////////////////////////////////////////////////////////////////////

  sealed trait Animal {
    def sound: String
  }

  case class Dog() extends Animal {
    def sound: String = "woof"
  }

  case class Cat(name: String) extends Animal {
    def sound: String = "meow"
  }

  ///////////////////////////////////////////////////////////////////////////////

  def makeSound(animal: Animal): Unit = {
    log(animal.sound)
  }

//     makeSound(Cat("Spock")) // meow
//     makeSound(Dog()) // woof

  ///////////////////////////////////////////////////////////////////////////////

  object ThirdPartyLibrary {

    case class Rabbit()

  }

  import ThirdPartyLibrary.Rabbit

//   makeSound(Rabbit()) // error: type mismatch

  ///////////////////////////////////////////////////////////////////////////////
  // Pattern matching = Type recognition + destructuring
  ///////////////////////////////////////////////////////////////////////////////

  val (x, y) = (1, 2) // destructuring a tuple


  val boris = Cat("Boris")

  val Cat(name) = boris
  //  log(name) // Boris

  val animal: Animal = boris

  val animalMatch = animal match { // demo case (exhaustive)
    case Cat(n) => s"Saw a cat with name $n"
    case Dog() => "Saw a dog"
  }

  log(animalMatch) // Saw a cat with name Boris

  ///////////////////////////////////////////////////////////////////////////////

  val intMatch = 5 match {
    case 1 => "Matching on a literal"
    case 2 | 3 => "Matching on multiple literals"
    case x if 1 until 10 contains x => s"Matching with guard: caught $x"
    case _ => "Didn't match anything"
  }

  //  log(intMatch) // Matching with guard: caught 5

  ///////////////////////////////////////////////////////////////////////////////

  val Pattern = "([a-c]+)".r

  val regexMatch = "abcdefg" match {
    case Pattern(c) => s"The capture group was $c"
    case _ => "Didn't match anything"
  }

  //  log(regexMatch) // The capture group was abc

  ///////////////////////////////////////////////////////////////////////////////

  val tupleMatch = (1, 2) match {
    case ab@(a, b) => s"Matching and destructuring a tuple, but keeping the original tuple bound to ab (${ab})"
  }

  //  log(tupleMatch) // Matching and destructuring a tuple, but keeping the original tuple bound to ab ((1,2))

  ///////////////////////////////////////////////////////////////////////////////
  // Scopes
  ///////////////////////////////////////////////////////////////////////////////

  val scopeReturnValue = {
    log("This will print")
    "This will be the return value of this scope"
  }

  log(scopeReturnValue) // "This will be the return value of this scope"`

  ///////////////////////////////////////////////////////////////////////////////
  // Scala function definition syntax variants - many ways to skin a cat
  ///////////////////////////////////////////////////////////////////////////////

  def add(x: Int, y: Int): Int =
    x + y

  def addBraced(x: Int, y: Int): Int = {
    val z = 5
    x + y // returns the last line within the braces
  }

  val addOuterAnon: (Int, Int) => Int = (x: Int, y: Int) => {
    x + y
  }

  val addInnerAnon: (Int, Int) => Int = { (x: Int, y: Int) =>
    x + y
  }

  val addDetestMyCoworkers: (Int, Int) => Int = {
    (_: Int) + (_: Int)
  }

  ///////////////////////////////////////////////////////////////////////////////
  // Currying - applying one parameter at a time
  ///////////////////////////////////////////////////////////////////////////////

  // log(add(3, 5))

  def addCurried(x: Int)(y: Int): Int =
    x + y

  // log(addCurried(3)(5))

  val addCurriedOuterAnon = (x: Int) => (y: Int) => { // might be familiar to people who've written JS
    x + y
  }

  // log(addCurriedOuterAnon(3)(5))

  val add3: Int => Int =
    addCurried(3)

  // log(add3(5))

  val add4: Int => Int =
    addOuterAnon.curried(4)

  ///////////////////////////////////////////////////////////////////////////////
  // Uncurrying - going back to the ol' fashioned way
  ///////////////////////////////////////////////////////////////////////////////

  val addUncurried: (Int, Int) => Int = Function.uncurried(addCurriedOuterAnon)

  //  log(addUncurried(3, 5)) // we can call it normally again!

  ///////////////////////////////////////////////////////////////////////////////
  // `def` enabling call by name
  ///////////////////////////////////////////////////////////////////////////////

  def emptyParameterList(): Unit =
    log("I'm a function that has an empty paramater list")

  // emptyParameterList()
  // emptyParameterList()

  def noParameterList: Unit =
    log("I'm a function that has no parameter list")

//   noParameterList
//   noParameterList


  ///////////////////////////////////////////////////////////////////////////////
  // Implicits
  ///////////////////////////////////////////////////////////////////////////////

  // Read the first four paragraphs (and stop):
  // https://dotty.epfl.ch/docs/reference/contextual/motivation.html

  implicit val implicitString: String = "implicit String"

  def getsImplicitString()(implicit x: String) = x

   log(getsImplicitString()) // implicit String

  ///////////////////////////////////////////////////////////////////////////////
  // A sensible usecase for implicits
  ///////////////////////////////////////////////////////////////////////////////

  case class TenantInfo(tenantId: String) // this is unique per request

  def dataAccessLayer()(implicit ctx: TenantInfo): Unit = {
    log(s"Accessing DB for tenant ${ctx.tenantId}")
  }

  def serviceLayer()(implicit ctx: TenantInfo): Unit = {
    log("Doing some business logic stuff")
    dataAccessLayer()
  }

  def controller(): Unit = {
    log("Doing some controller stuff to recover the `tenantId` from the request (maybe verifying a cookie)")
    implicit val tenantContext: TenantInfo = TenantInfo(tenantId = "3e4ff4a9-0631-49d7-b250-3318e8acfcc4")
    serviceLayer()
  }

  //  controller()

  ///////////////////////////////////////////////////////////////////////////////
  // Implicit resolution is type directed!
  ///////////////////////////////////////////////////////////////////////////////

  implicit val implicitBoolean: Boolean = true
  implicit val implicitInt: Int = 5

  def rawImplicitly[T](implicit x: T): T = {
    x
  }

   log(rawImplicitly[Boolean]) // true
   log(rawImplicitly[Int]) // 5

  ///////////////////////////////////////////////////////////////////////////////
  // Generic parameters can be higher-kinded, and is compatible with implicit resolution
  ///////////////////////////////////////////////////////////////////////////////

  implicit val implicitListInt: List[Int] = List(5)
  implicit val implicitOptionInt: Option[Int] = Some(5)

  def hktImplicitly[F[_]](implicit x: F[Int]): F[Int] = {
    x
  }

  // scala> :k List
  // List's kind is F[+A]

  //   log(hktImplicitly[List]) // List(5)
  //   log(hktImplicitly[Option]) // Some(5)

  ///////////////////////////////////////////////////////////////////////////////

  type Id[A] = A
  // * -> *

  // scala> :k Id
  // Id's kind is F[+A]

  implicit val implicitListString: List[String] = List("hello")
  implicit val implicitListBoolean: List[Boolean] = List(true)

  def hktAppImplicitly[F[_], A](implicit x: F[A]): F[A] = {
    x
  }

  // log(hktAppImplicitly[List, String]) // List("hello")
  // log(hktAppImplicitly[List, Boolean]) // List(true)
  // log(hktAppImplicitly[Id, String]) // "implicit String"

  ///////////////////////////////////////////////////////////////////////////////
  // Implicit defs - we can parameterize our implicit recovery
  ///////////////////////////////////////////////////////////////////////////////

  implicit def emptyOption[A]: Option[A] = None

  log(implicitly[Option[Cat]]) // None
  log(implicitly[Option[Dog]]) // None


  ///////////////////////////////////////////////////////////////////////////////
  // Implicit objects - implicits need not be primatives or class instances
  ///////////////////////////////////////////////////////////////////////////////

  implicit object StaticCat extends Cat(name = "Static Cat") {
    override def sound: String = s"static: ${super.sound}"
  }

  // don't have to construct StaticCat - is global
    log(StaticCat.sound) // static: meow

//    log(implicitly[Cat].sound) // static: meow

  ///////////////////////////////////////////////////////////////////////////////
  // Implicit classes - classes that auto-wrap themselves around a receiver
  ///////////////////////////////////////////////////////////////////////////////

  // They have a bunch of weird rules that you should probably read about
  // https://docs.scala-lang.org/overviews/core/implicit-classes.htmlz

  object IntSyntax {

    implicit class IntExtensions(private val self: Int) extends AnyVal {
      def increment(): Int = self + 1
    }

  }

  import IntSyntax._

  log(5.increment()) // 6

  ///////////////////////////////////////////////////////////////////////////////

  trait Sound[A] {
    def sound(a: A): String
  }

  object Sound {

    object Syntax {

      implicit class SoundIdExtensions[A](private val self: A) extends AnyVal {
        def sound_()(implicit instance: Sound[A]): String = {
          instance.sound(self)
        }
      }

    }

    object Instances {

      trait SoundDog extends Sound[Dog] {
        override def sound(a: Dog): String = "woof"
      }

      trait SoundCat extends Sound[Cat] {
        override def sound(a: Cat): String = "meow"
      }

      trait SoundRabbit extends Sound[Rabbit] {
        override def sound(a: Rabbit): String = "chitter"
      }

      implicit val dogInstance: Sound[Dog] = new SoundDog {}
      implicit val catInstance: Sound[Cat] = new SoundCat {}
      implicit val rabbitInstance: Sound[Rabbit] = new SoundRabbit {}
    }

  }

  ////////////////////////////////////////////////////////////////////////////////
  // `Sound` consumption
  ////////////////////////////////////////////////////////////////////////////////

  import Sound.Instances._
  import Sound.Syntax._

  def makeSoundImplicitParam[A](a: A)(implicit instance: Sound[A]): Unit = {
    log(a.sound_())
  }

    makeSoundImplicitParam(Dog())
    makeSoundImplicitParam(Cat("Boris"))
    makeSoundImplicitParam(Rabbit()) // this now works!

  // this desugars to the method above
  def makeSoundGenericRequirement[A: Sound](a: A): Unit = {
    // val instance = implicitly[Sound[A]] // we can still recover the instance
    log(a.sound_())
  }

  makeSoundGenericRequirement(Dog())

  ///////////////////////////////////////////////////////////////////////////////

  case class HasNoSound()

//   makeSoundImplicitParam(HasNoSound())
  // hkts.sc:184: could not find implicit value for parameter instance: ammonite.$file.hkts.Sound[ammonite.$file.hkts.HasNoSound]
  // val res_38 = makeSoundImplicitParam(HasNoSound())

  // makeSoundGenericRequirement(HasNoSound())
  // hkts.sc:194: could not find implicit value for evidence parameter of type ammonite.$file.hkts.Sound[ammonite.$file.hkts.HasNoSound]
  // val res_39 = makeSoundGenericRequirement(HasNoSound())

  ///////////////////////////////////////////////////////////////////////////////
  // What do we want the JSON encoding API to look like
  ///////////////////////////////////////////////////////////////////////////////

  // log(5.encode().value) // 5
  // log("hello".encode().value) // "hello"
  //
  val me = Person(name = "Max Bo", age = 22, alive = true)

//   log(me.encode().value) // { "name": "Max Bo", "age": 22, "alive": true }


  ///////////////////////////////////////////////////////////////////////////////
  // `Encode` implementation
  ///////////////////////////////////////////////////////////////////////////////

  case class Json(value: String)

  trait Encode[A] {
    def encode(a: A): Json
  }

  object Encode {

    object Syntax {

      implicit class EncodeIdExtensions[A](private val self: A) extends AnyVal {
        def encode()(implicit instance: Encode[A]): Json = {
          instance.encode(self)
        }
      }

    }

    object Instances {

      trait EncodeString extends Encode[String] {
        override def encode(x: String) = Json("\"" + x.toString + "\"")
      }

      trait EncodeInt extends Encode[Int] {
        override def encode(x: Int) = Json(x.toString)
      }

      trait EncodeBoolean extends Encode[Boolean] {
        override def encode(x: Boolean) = Json(x.toString)
      }

      trait EncodeMap extends Encode[Map[String, Json]] {
        override def encode(kv: Map[String, Json]): Json = {
          val inner: String =
            kv
              .toList
              .map { case (k, v) => s"${encodeString.encode(k).value}: ${v.value}" }
              .mkString(", ")

          val outer = s"{ $inner }"
          Json(outer)
        }
      }

      implicit val encodeString: Encode[String] = new EncodeString {}

      implicit val encodeInt: Encode[Int] = new EncodeInt {}

      implicit val encodeBoolean: Encode[Boolean] = new EncodeBoolean {}

      implicit val encodeMap: Encode[Map[String, Json]] = new EncodeMap {}
    }

  }


  ////////////////////////////////////////////////////////////////////////////////
  // `Encode` consumption
  ////////////////////////////////////////////////////////////////////////////////

  import Encode.Instances._
  import Encode.Syntax._

  case class Person(name: String, age: Int, alive: Boolean)

  // Note the gap between `Person` and `Encode[Person]`
  object Person {

    trait EncodePerson extends Encode[Person] {
      def encode(person: Person): Json =
      // we can obviously do this in a macro
        Map(
          "name" -> person.name.encode(),
          "age" -> person.age.encode(),
          "alive" -> person.alive.encode()
        ).encode()
    }

    implicit def encodePerson: Encode[Person] = new EncodePerson {}
  }

  // implicit search goes into the companion object
    log(me.encode().value) // { "name": "Max Bo", "age": 22, "alive": true }
  // this now works!

  // obviously these do as well
  // log(5.encode().value)
  // log("hello".encode().value)

  ////////////////////////////////////////////////////////////////////////////////
  // Declaring requirements of `Encode`
  ////////////////////////////////////////////////////////////////////////////////

  def needsAnEncoderGenericRequirement[A: Encode](a: A) {
//     val instance = implicitly[Encode[A]] // we can still recover the instance
    log(a.encode().value)
  }

  // needsAnEncoderGenericRequirement(me) // { "name": "Max Bo", "age": 22, "alive": true }

  case class HasNoEncoder()

  // hkts.sc:150: could not find implicit value for evidence parameter of type ammonite.$file.hkts.Encode[ammonite.$file.hkts.HasNoEncoder]
  // val res_28 = needsAnEncoder(HasNoEncoder())
  // needsAnEncoder(HasNoEncoder())

}
