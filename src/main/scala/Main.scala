import java.io.File

import scala.io.Source

object Main extends App {

  // Things may appear in a weird order. I'm doing that so that you know every
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
    println(animal.sound)
  }

  //    makeSound(Cat())
  //  makeSound(Dog())

  ///////////////////////////////////////////////////////////////////////////////

  object ThirdPartyLibrary {

    case class Rabbit()

  }

  import ThirdPartyLibrary.Rabbit

  //   makeSound(Rabbit())

  ///////////////////////////////////////////////////////////////////////////////
  // Pattern matching = Type recognition + destructuring
  ///////////////////////////////////////////////////////////////////////////////

  val (x, y) = (1, 2) // destructuring a tuple

  val Cat(name) = Cat("Boris")

  val animal: Animal = Dog()

  val result = animal match { // demo case (exhaustive)
    case Cat(n) => s"Saw a cat with name $n"
    case Dog() => "Saw a dog"
  }

  //  println(result)

  //  val Pattern = r"([a-cA-C])"
  //
  //    def matchExample(x: AnyVal): Unit = {
  //      x match {
  //        case i: Int     => "Matching on type"
  //        case 1          => "Matching on a literal"
  //        case 2 | 3      => "Matching on multiple literals"
  //        case x if 0 until 10 contains x => "Matching with guard"
  //        case ab@(a, b)  => "Matching and destructuring a tuple, but keeping the original tuple bound to ab"
  //        case x::xs      => "Matching and destructuring a list"
  //        case Pattern(c) => "c bound to capture group here (if x were a string). This will fail if the Regex doesn't match"
  //        case _          => "Matching wildcard"
  //      }
  //    }


  ///////////////////////////////////////////////////////////////////////////////
  // Scala function calling semantic oddities
  ///////////////////////////////////////////////////////////////////////////////

  def add(x: Int, y: Int): Int = x + y

  // println(add(3, 5))

  def addCurried(x: Int)(y: Int): Int = x + y

  // println(addCurried(3)(5))

  val add3: Int => Int = addCurried(3)

  // println(add3(5))

  ///////////////////////////////////////////////////////////////////////////////

  def emptyParamterList() = println("I'm a function that has an empty paramater list")

  // emptyParamterList()
  // emptyParamterList()

  def noParameterList = println("I'm a function that has no paramater list")

  // noParameterList
  //   noParameterList

  ///////////////////////////////////////////////////////////////////////////////
  // Implicits
  ///////////////////////////////////////////////////////////////////////////////
  // Read the first four paragraphs (and stop):
  // https://dotty.epfl.ch/docs/reference/contextual/motivation.html

  implicit val implicitString: String = "implicit String"

  def getsImplicitString()(implicit x: String) = x

  //   println(getsImplicitString()) // implicit String

  ///////////////////////////////////////////////////////////////////////////////
  // A sensible usecase for implicits
  ///////////////////////////////////////////////////////////////////////////////

  case class TenantInfo(tenantId: String) // this is unique per request

  def dataAccessLayer()(implicit ctx: TenantInfo): Unit = {
    println(s"Accessing DB for tenant ${ctx.tenantId}")
  }

  def serviceLayer()(implicit ctx: TenantInfo): Unit = {
    println("Doing some business logic stuff")
    dataAccessLayer()
  }

  def controller(): Unit = {
    println("Doing some controller stuff to recover the `tenantId` from the request (maybe verifying a cookie)")
    implicit val tenantContext = TenantInfo(tenantId = "3e4ff4a9-0631-49d7-b250-3318e8acfcc4")
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

  //   println(rawImplicitly[Boolean]) // true
  //   println(rawImplicitly[Int]) // 5

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

  //   println(hktImplicitly[List]) // List(5)
  //   println(hktImplicitly[Option]) // Some(5)

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

  // println(hktAppImplicitly[List, String]) // List("hello")
  // println(hktAppImplicitly[List, Boolean]) // List(true)
  // println(hktAppImplicitly[Id, String]) // "implicit String"


  ///////////////////////////////////////////////////////////////////////////////
  // Implicit classes
  ///////////////////////////////////////////////////////////////////////////////

  object IntSyntax {

    implicit final class IntExtensions(private val self: Int) extends AnyVal {
      def increment(): Int = self + 1
    }

  }

  import IntSyntax._

  // println(5.increment()) // 6

  ///////////////////////////////////////////////////////////////////////////////

  trait Sound[A] {
    def sound(a: A): String
  }

  object Sound {

    object Syntax {

      implicit class CanMakeSoundIdExtensions[A](private val self: A) extends AnyVal {
        def sound_()(implicit instance: Sound[A]): String = {
          instance.sound(self)
        }
      }

    }

    object Instances {
      implicit val dogInstance: Sound[Dog] = new Sound[Dog] {
        override def sound(a: Dog): String = "woof"
      }

      implicit val catInstance: Sound[Cat] = new Sound[Cat] {
        override def sound(a: Cat): String = "meow"
      }

      implicit val rabbitInstance: Sound[Rabbit] = new Sound[Rabbit] {
        override def sound(a: Rabbit): String = "what the fuck sound does a rabbit make lmao"
      }
    }

  }

  ////////////////////////////////////////////////////////////////////////////////
  // `Sound` consumption
  ////////////////////////////////////////////////////////////////////////////////

  import Sound.Instances._
  import Sound.Syntax._

  def makeSoundImplicitParam[A](a: A)(implicit instance: Sound[A]): Unit = {
    println(a.sound_())
  }

  //  makeSoundImplicitParam(Dog())
  //  makeSoundImplicitParam(Cat())
  //    makeSoundImplicitParam(Rabbit()) // this now works!

  def makeSoundGenericRequirement[A: Sound](a: A): Unit = {
    // val instance = implicitly[Sound[A]] // we can still recover the instance
    println(a.sound_())
  }

  ///////////////////////////////////////////////////////////////////////////////

  case class HasNoSound()

  // makeSoundImplicitParam(HasNoSound())
  // hkts.sc:184: could not find implicit value for parameter instance: ammonite.$file.hkts.Sound[ammonite.$file.hkts.HasNoSound]
  // val res_38 = makeSoundImplicitParam(HasNoSound())

  // makeSoundGenericRequirement(HasNoSound())
  // hkts.sc:194: could not find implicit value for evidence parameter of type ammonite.$file.hkts.Sound[ammonite.$file.hkts.HasNoSound]
  // val res_39 = makeSoundGenericRequirement(HasNoSound())

  ///////////////////////////////////////////////////////////////////////////////
  // What do we want the JSON encoding API to look like
  ///////////////////////////////////////////////////////////////////////////////

  // println(5.encode().value) // 5
  // println("hello".encode().value) // "hello"
  //
  //val me = Person(name="Max Bo", age=22, alive=true)

  // println(me.encode().value) // { "name": "Max Bo", "age": 22, "alive": true }


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
      implicit val encodeString: Encode[String] = new Encode[String] {
        override def encode(x: String) = Json("\"" + x.toString() + "\"")
      }

      implicit val encodeInt: Encode[Int] = new Encode[Int] {
        override def encode(x: Int) = Json(x.toString())
      }

      implicit val encodeBoolean: Encode[Boolean] = new Encode[Boolean] {
        override def encode(x: Boolean) = Json(x.toString())
      }

      implicit def encodeMap[A, B]: Encode[Map[String, Json]] = new Encode[Map[String, Json]] {
        override def encode(kv: Map[String, Json]) = {
          val inner =
            kv
              .toList
              .map { case (k, v) => s"${encodeString.encode(k).value}: ${v.value}" }
              .mkString(", ")

          val outer = s"{ ${inner} }"
          Json(outer)
        }
      }
    }

  }


  ////////////////////////////////////////////////////////////////////////////////
  // `Encode` consumption
  ////////////////////////////////////////////////////////////////////////////////

  import Encode.Instances._
  import Encode.Syntax._

  case class Person(name: String, age: Int, alive: Boolean)

  // Note the gap between `Person` and `Encode[Person`

  implicit def encodePerson: Encode[Person] = new Encode[Person] {
    def encode(person: Person): Json =
    // we can obviously do this in a macro
      Map(
        "name" -> person.name.encode(),
        "age" -> person.age.encode(),
        "alive" -> person.alive.encode()
      ).encode()
  }

  // println(me.encode().value) // { "name": "Max Bo", "age": 22, "alive": true }
  // this now works!

  // obviously these do as well
  // println(5.encode().value)
  // println("hello".encode().value)

  ////////////////////////////////////////////////////////////////////////////////
  // Declaring requirements of `Encode`
  ////////////////////////////////////////////////////////////////////////////////

  def needsAnEncoderGenericRequirement[A: Encode](a: A) {
    // val instance = implicitly[Encode[A]] // we can still recover the instance
    println(a.encode().value)
  }

  // needsAnEncoderGenericRequirement(me) // { "name": "Max Bo", "age": 22, "alive": true }

  case class HasNoEncoder()

  // hkts.sc:150: could not find implicit value for evidence parameter of type ammonite.$file.hkts.Encode[ammonite.$file.hkts.HasNoEncoder]
  // val res_28 = needsAnEncoder(HasNoEncoder())
  // needsAnEncoder(HasNoEncoder())

  ////////////////////////////////////////////////////////////////////////////////
  // Datatypes that support `.map`
  ////////////////////////////////////////////////////////////////////////////////

  // sealed trait Option[+A]
  // case class Some[+A](value: A) extends Option[A]
  // case object None extends Option[Nothing]

  case class FamilyMember(
    age: Int,
    mother: Option[FamilyMember] = None,
    father: Option[FamilyMember] = None
  ) {
    def parents: List[FamilyMember] = {
      List(mother, father).collect { // nice way to filter out None's
        case Some(member) => member
      }
    }
  }

  val nana = FamilyMember(age = 103)
  val greatGrandad = FamilyMember(age = 104)

  val grandma = FamilyMember(age = 79, mother = Some(nana))
  val grandad = FamilyMember(age = 82, father = Some(greatGrandad))

  val mum = FamilyMember(age = 55, mother = Some(grandma))
  val dad = FamilyMember(age = 56, father = Some(grandad))

  val son = FamilyMember(age = 22, mother = Some(mum), father = Some(dad))

  val family: List[FamilyMember] = List(grandma, grandad, mum, dad, son)

  def getParentsAges(familyMember: FamilyMember): List[Int] = {
    val parents: List[FamilyMember] = familyMember.parents
    parents.map(_.age)
  }

  def getMothersAge(member: FamilyMember): Option[Int] = {
    val mother: Option[FamilyMember] = member.mother
    mother.map(_.age)
  }

  //  println(getParentsAges(son)) // List(55, 56)
  //  println(getParentsAges(grandma)) // List()
  //  println(getMothersAge(son)) // Some(55)
  //  println(getMothersAge(grandma)) // None

  ////////////////////////////////////////////////////////////////////////////////

  def getAgeFromList(familyMembers: List[FamilyMember]): List[Int] = {
    familyMembers.map(familyMember => familyMember.age)
  }

  def getAgeFromOption(member: Option[FamilyMember]): Option[Int] = {
    member.map(familyMember => familyMember.age)
  }

  //  println(getAgeFromList(family)) // List(103, 79, 82, 55, 56, 22)
  //  println(getAgeFromList(grandma.parents)) // List()
  //  println(getAgeFromOption(son.mother)) // Some(55)
  //  println(getAgeFromOption(grandma.mother)) // None

  // How do we make something like this? 🤔
  //   def getAge[F[_]: ???](f: F[FamilyMember]): F[Int] = {
  //     f.map(_.age)
  //   }

  ////////////////////////////////////////////////////////////////////////////////
  // Functor implementation
  ////////////////////////////////////////////////////////////////////////////////

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  object Functor {

    object Syntax {

      implicit final class FunctorExtensions[F[_], A](private val self: F[A]) extends AnyVal {
        def map[B](f: A => B)(implicit instance: Functor[F]): F[B] = {
          instance.map(self)(f)
        }
      }
    }

    object Instances {
      // @ List(1, 2, 3).map(x => x + 1)
      // res0: List[Int] = List(2, 3, 4)
      implicit val listFunctorInstance: Functor[List] = new Functor[List] {
        def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f) // very scary reimplementation, let's not do that and use the stdlib
      }

      implicit val optionFunctorInstance: Functor[Option] = new Functor[Option] {
        def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa match {
          // unwrap              apply
          //             rewrap
          case Some(a) => Some(f(a))
          case None => None
        }
      }
    }

  }


  ////////////////////////////////////////////////////////////////////////////////
  // Functor usage
  ////////////////////////////////////////////////////////////////////////////////

  import Functor.Instances._
  import Functor.Syntax._

  def getAge[F[_] : Functor](f: F[FamilyMember]): F[Int] = {
    f.map(_.age)
  }

  // println(getAge(family)) // List(103, 79, 82, 55, 56, 22)
  // println(getAge(son.mother)) // Some(55)

  ////////////////////////////////////////////////////////////////////////////////
  // Why Functors might not be all we need
  ////////////////////////////////////////////////////////////////////////////////

  // Functors are not good at dealing with chained operations.
  // this type would get bigger and bigger as we traversed up the family tree
  // if only there was some way to flatten these down 🤔
  def getGreatGrandMatriarchAge(member: FamilyMember): Option[Option[Option[Int]]] = {
    member.mother
      .map(mother => mother.mother
        .map(grandmother => grandmother.mother
          .map(greatGrandmother => greatGrandmother.age)))
  }

  //  println(getGreatGrandMatriarchAge(son)) // Some(Some(Some(103)))
  //  println(getGreatGrandMatriarchAge(mum)) // Some(Some(None))

  def getAllGreatGrandparentAges(member: FamilyMember): List[List[List[Int]]] = {
    member.parents
      .map(parent => parent.parents
        .map(grandparent => grandparent.parents
          .map(greatGrandparent => greatGrandparent.age)))
  }

  //  println(getAllGreatGrandparentAges(son)) // List(List(List(103)), List(List(104)))

  def sumAgeOfPersonAndParents(member: FamilyMember): Option[Option[Int]] = {
    member.mother.map(mother =>
      member.father.map(father =>
        member.age + mother.age + father.age
      )
    )
  }

  //  println(sumAgeOfPersonAndParents(son)) // Some(Some(133))
  //  println(sumAgeOfPersonAndParents(mum)) // Some(None)

  ////////////////////////////////////////////////////////////////////////////////
  // A hacky way out?
  ////////////////////////////////////////////////////////////////////////////////

  def flattenOption[A](ffa: Option[Option[A]]): Option[A] = {
    ffa match {
      case Some(Some(a)) => Some(a)
      case _ => None
    }
  }

  def getGreatMatriarchAgeFlatten(member: FamilyMember): Option[Int] = {
    flattenOption(
      flattenOption(member.mother
        .map(mother => mother.mother)
      ).map(grandmother => grandmother.mother)
    ).map(greatGrandmother => greatGrandmother.age)
  }

  //  println(getGreatMatriarchAgeFlatten(son)) // Some(103)
  //  println(getGreatMatriarchAgeFlatten(mum)) // None

  ////////////////////////////////////////////////////////////////////////////////
  // Or a reasonable solution
  ////////////////////////////////////////////////////////////////////////////////
  // DOCUMENTATION:
  // https://typelevel.org/cats/typeclasses/monad.html

  trait Monad[F[_]] {
    implicit def functorInstance: Functor[F]

    def point[A](a: A): F[A]

    // you only have to implement one or the other, as each can be derived from each others impl
    def flatten[A](ffa: F[F[A]]): F[A] =
      flatMap(ffa)(x => x)

    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] =
      flatten(fa.map(f))
  }

  object Monad {

    object Syntax {

      implicit class MonadIdExtensions[A](private val self: A) extends AnyVal {
        def point[F[_]]()(implicit instance: Monad[F]): F[A] = {
          instance.point(self)
        }
      }

      implicit final class MonadMonadExtensions[F[_], A](private val self: F[F[A]]) extends AnyVal {
        def flatten[B](implicit instance: Monad[F]): F[A] = {
          instance.flatten(self)
        }
      }

      implicit final class MonadExtensions[F[_], A](private val self: F[A]) extends AnyVal {
        def flatMap[B](f: A => F[B])(implicit instance: Monad[F]): F[B] = {
          instance.flatMap(self)(f)
        }
      }

    }


    object Instances {
      implicit val listMonadInstance: Monad[List] = new Monad[List] {
        override implicit def functorInstance: Functor[List] = listFunctorInstance

        def point[A](a: A): List[A] = List(a)

        override def flatten[A](ffa: List[List[A]]): List[A] =
          ffa.flatten
      }

      implicit val optionMonadInstance: Monad[Option] = new Monad[Option] {
        override implicit def functorInstance: Functor[Option] = optionFunctorInstance

        def point[A](a: A): Option[A] = Some(a)

        override def flatten[A](ffa: Option[Option[A]]): Option[A] = ffa match {
          // unwrap unwrap      rewrap
          case Some(Some(a)) => Some(a)
          case _ => None
        }
      }
    }

  }

  ////////////////////////////////////////////////////////////////////////////////
  // Monad usage
  ////////////////////////////////////////////////////////////////////////////////

  import Monad.Instances._
  import Monad.Syntax._

  def getGreatGrandMatriarchAgeM(member: FamilyMember): Option[Int] = {
    member.mother
      .flatMap(mother => mother.mother)
      .flatMap(grandmother => grandmother.mother)
      .map(greatGrandmother => greatGrandmother.age)
    //           ^             ^ multiple successive Monad operations
    //                           causes _eliminates nesting
  }

  //  println(getGreatGrandMatriarchAgeM(son)) // Some(79)
  //  println(getGreatGrandMatriarchAgeM(mum) // None

  def getAllGreatGrandparentAgesM(member: FamilyMember): List[Int] = {
    member.parents
      .flatMap(parent => parent.parents)
      .flatMap(grandparent => grandparent.parents)
      .map(greatGrandparent => greatGrandparent.age)
  }

  //  println(getAllGreatGrandparentAgesM(son)) // List(103, 104)

  def sumAgeOfPersonAndParentsM(member: FamilyMember): Option[Int] = {
    member.mother.flatMap(mother =>
      member.father.map(father =>
        member.age + mother.age + father.age
      )
    )
  }

  //  println(sumAgeOfPersonAndParents(son)) // Some(133)
  //  println(sumAgeOfPersonAndParents(mum)) // None

  ///////////////////////////////////////////////////////////////////////////////
  // `for` notation
  ////////////////////////////////////////////////////////////////////////////////

  def getAllGreatGrandparentAgesC(member: FamilyMember): List[Int] = {
    for {
      parent <- member.parents
      grandparent <- parent.parents
      greatGrandparent <- grandparent.parents
    } yield greatGrandparent.age
  }

  def getGreatGrandMatriarchAgeC(member: FamilyMember): Option[Int] = {
    for {
      mother <- member.mother
      grandmother <- mother.mother
      greatGrandmother <- grandmother.mother
    } yield greatGrandmother.age
  }

  def sumAgeOfPersonAndParentsC(member: FamilyMember): Option[Int] = {
    for {
      mother <- member.mother
      father <- member.father
    } yield member.age + mother.age + father.age
  }

  ///////////////////////////////////////////////////////////////////////////////
  // Fake Async
  ///////////////////////////////////////////////////////////////////////////////

  class Sync[A](val unsafeInterpret: () => A)

  object Sync {
    def effect[A](eff: => A) = new Sync(() => eff)

    object Instances {
      implicit val ioFunctorInstance: Functor[Sync] = new Functor[Sync] {
        def map[A, B](fa: Sync[A])(f: A => B): Sync[B] = Sync.effect(f(fa.unsafeInterpret()))
      }

      implicit val ioMonadInstance: Monad[Sync] = new Monad[Sync] {
        override implicit def functorInstance: Functor[Sync] = ioFunctorInstance

        def point[A](a: A): Sync[A] = Sync.effect(a)

        override def flatten[A](ffa: Sync[Sync[A]]): Sync[A] =
        //                      unwrap unwrap
        // rewrap
          Sync.effect(ffa.unsafeInterpret().unsafeInterpret())
      }
    }

  }

  ///////////////////////////////////////////////////////////////////////////////
  // Sync usage
  ///////////////////////////////////////////////////////////////////////////////

  import Sync.Instances._

  def putStrLn(line: String): Sync[Unit] =
    Sync.effect(println(line))

  val getStrLn: Sync[String] =
    Sync.effect(scala.io.StdIn.readLine())

  val echo: Sync[Unit] = for {
    _ <- putStrLn("Please enter something to be echoed:")
    str <- getStrLn
    _ <- putStrLn("Echoing: " + str)
  } yield ()

  //   echo.unsafeInterpret()
  //   echo.unsafeInterpret()

  // is equivalent to
  //     putStrLn("Please enter something to be echoed:")
  //       .flatMap(_ => getStrLn
  //         .flatMap(str =>
  //           putStrLn("Echoing: " + str)
  //             .map(_ => ()
  //           )))

  ///////////////////////////////////////////////////////////////////////////////
  // Function composition
  ///////////////////////////////////////////////////////////////////////////////

  val showInt: Int => String = _.toString
  val isBigString: String => Boolean = _.length >= 5

  val isBigNumber: Int => Boolean = showInt.andThen(isBigString)

  //  println(isBigNumber(10000)) // true

  ///////////////////////////////////////////////////////////////////////////////
  // The reader Functor
  ///////////////////////////////////////////////////////////////////////////////
  // DOCUMENTATION
  // - https://typelevel.org/cats/typeclasses/functor.html

  object Function1FunctorInstances {
    implicit def function1FunctorInstance[R]: Functor[R => ?] = new Functor[R => ?] {
      def map[A, B](fa: R => A)(f: A => B): R => B = fa.andThen(f)

      //       def map[A, B](fa: R => A)(f: A => B): R => B =
      //        { r: R =>
      //          val a: A = fa(r) // unwrap
      //          val b: B = f(a) // apply
      //          b
      //        } // rewrap
    }

  }

  import Function1FunctorInstances._

  val isBigNumberM: Int => Boolean = showInt.map(isBigString)

  //  println(isBigNumberM(10000)) // true

  ///////////////////////////////////////////////////////////////////////////////

  object Function1MonadInstances {
    implicit def function1MonadInstance[R]: Monad[R => ?] = new Monad[R => ?] {
      override implicit def functorInstance: Functor[R => ?] = function1FunctorInstance

      def point[A](a: A): R => A =
        (_: R) => a

      override def flatMap[A, B](fa: R => A)(f: A => (R => B)): R => B = { r: R =>
        val a: A = fa(r) // unwrap
        val fb: R => B = f(a) // apply
        val b: B = fb(r) // unwrap
        b
      } // rewrap
    }
  }

  import Function1MonadInstances._

  val doTransforms: Int => (Int, String, Boolean) = for {
    addOne <- (x: Int) => x + 1
    toString <- (x: Int) => x.toString
    isEven <- (x: Int) => x % 2 == 0
  } yield (addOne, toString, isEven)

  //  val doTransformsDesugared: Int => (Int, String, List[Int]) =
  //    ((x: Int)   => x + 1)
  //      .flatMap(addOne => ((x: Int) => x.toString)
  //        .flatMap(toString => ((x: Int)   => x % 2 == 0)
  //          .map(isEven => (addOne, toString, isEven))))
  //
  //   println(doTransforms(0)) // (1, "0", true)


  ///////////////////////////////////////////////////////////////////////////////
  // State - where we're going, we don't need variables
  ///////////////////////////////////////////////////////////////////////////////

  final case class State[S, A](run: S => (S, A))

  object State {

    object Instances {
      implicit def stateFunctorInstance[S]: Functor[State[S, ?]] = new Functor[State[S, ?]] {
        def map[A, B](fa: State[S, A])(f: A => B): State[S, B] =
          State(s => {
            val (sp, a) = fa.run(s) // unwrap
            (sp, f(a)) // apply
          }) // rewrap
      }

      implicit def stateMonadInstance[S]: Monad[State[S, ?]] = new Monad[State[S, ?]] {
        def functorInstance: Functor[State[S, ?]] = stateFunctorInstance

        def point[A](a: A): State[S, A] =
          State(s => (s, a))

        override def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] =
          State(s => {
            val (sp, a) = fa.run(s) // unwrap
            val fb: State[S, B] = f(a) // apply
            fb.run(sp) // unwrap
          }) // rewrap
      }
    }

    def get[S](): State[S, S] =
      State(s => (s, s))

    def put[S](n: S): State[S, Unit] =
      State(s => (n, ()))

    def modify[S](f: S => S): State[S, Unit] =
      State(s => (f(s), ()))
  }

  ///////////////////////////////////////////////////////////////////////////////
  // State - usage
  ///////////////////////////////////////////////////////////////////////////////

  import State.Instances._

  val computation: State[Int, String] = for {
    _ <- State.modify[Int](_ + 1)
    _ <- State.put[Int](10)
    _ <- State.modify[Int](_ - 2)
    last <- State.get[Int]()
  } yield s"The final value is $last"

  // where 0 is the initial value
  // println(computation.run(0)) // What's the final value?

  //val computationTake2 = State.put(5)
  //      .flatMap(_ => State.get())
  //      .map(x => x + 1)

  // println(computationTake2.run(0)) // What's the final value?

  ///////////////////////////////////////////////////////////////////////////////
  //  Does composition always work?
  ///////////////////////////////////////////////////////////////////////////////

  def getMother: FamilyMember => Option[FamilyMember] =
    (familyMember: FamilyMember) => familyMember.mother

  // 😔
  //  val getGrandMother = getMother.map(getMother)

  // 😔
  //  val getMotherAge: FamilyMember => Option[Int]
  //    getMother.map(grandmother => grandmother.age)

  ///////////////////////////////////////////////////////////////////////////////
  // Kleisli - I'm really sorry
  ///////////////////////////////////////////////////////////////////////////////

  final case class Kleisli[F[_] : Monad, A, B](run: A => F[B]) {
    def andThen[C](k: Kleisli[F, B, C]): Kleisli[F, A, C] =
      Kleisli(a => run(a).flatMap(k.run))
  }

  object Kleisli {

    object Instances {
      implicit def kleisliFunctorInstance[F[_] : Monad, R]: Functor[Kleisli[F, R, ?]] = new Functor[Kleisli[F, R, ?]] {
        def map[A, B](rfa: Kleisli[F, R, A])(f: A => B): Kleisli[F, R, B] = {
          implicit val functorInstance: Functor[F] = implicitly[Monad[F]].functorInstance
          Kleisli(rfa.run.map(fa => fa.map(f)))
        }
      }

      implicit def kleisliMonadInstance[F[_] : Monad, R]: Monad[Kleisli[F, R, ?]] = new Monad[Kleisli[F, R, ?]] {
        override implicit def functorInstance: Functor[Kleisli[F, R, ?]] = kleisliFunctorInstance

        def point[A](a: A): Kleisli[F, R, A] =
          Kleisli(a.point[F].point[R => ?])

        override def flatten[A](ffa: Kleisli[F, R, Kleisli[F, R, A]]): Kleisli[F, R, A] =
        //              unwrap                unwrap
        // rewrap
          Kleisli(r => ffa.run(r).flatMap(fa => fa.run(r)))
      }
    }

  }

  import Kleisli.Instances._

  def getMotherK: Kleisli[Option, FamilyMember, FamilyMember] =
    Kleisli(getMother)

  val getGreatGrandmotherAgeR: FamilyMember => Option[Int] =
    getMotherK.andThen(getMotherK).andThen(getMotherK).map(_.age).run

  //  println(getGreatGrandmotherAgeR(son)) // Some(103)

  ///////////////////////////////////////////////////////////////////////////////
  // Fake Akka directives
  ///////////////////////////////////////////////////////////////////////////////

  case class HttpRequest(method: String, parameters: Map[String, String], body: Option[String])

  case class HttpResponse(body: Option[String])

  case class JwtToken(claims: List[String])

  def POST(request: HttpRequest): Option[Unit] = {
    if (request.method == "POST")
      Some(())
    else
      None
  }

  def extractBody(request: HttpRequest): Option[String] =
    request.body

  def extractHeader(name: String)(request: HttpRequest): Option[String] =
    request.parameters.get(name)

  def parseJwtToken(token: String): Option[JwtToken] =
    Some(JwtToken(claims = List(token)))

  def extractJWT(): HttpRequest => Option[JwtToken] =
    Kleisli(extractHeader("Authorisation"))
      .andThen(Kleisli(parseJwtToken))
      .run

  def isAdminJwt(jwt: JwtToken): Option[Unit] = {
    if (jwt.claims.contains("Admin"))
      Some(())
    else
      None
  }

  def isAdmin: HttpRequest => Option[Unit] =
    Kleisli(extractJWT())
      .andThen(Kleisli(isAdminJwt))
      .run

  def postHandler: Kleisli[Option, HttpRequest, HttpResponse] = for {
    _ <- Kleisli(POST)
    _ <- Kleisli(isAdmin)
    body <- Kleisli(extractBody)
  } yield HttpResponse(Some(body))

  //  println(postHandler.run(
  //    HttpRequest(
  //      "POST",
  //      Map("Authorisation" -> "Admin"),
  //      Some("Hello world")
  //  ))) // Some(HttpResponse("Hello world"))

  //  println(postHandler.run(
  //    HttpRequest(
  //      "POST",
  //      Map("Authorisation" -> "Not Admin"),
  //      Some("Hello world")
  //  ))) // None

  //  println(postHandler.run(
  //    HttpRequest(
  //      "GET",
  //      Map("Authorisation" -> "Admin"),
  //      Some("Hello world")
  //  ))) // None

  type ReaderT[F[_], A, B] = Kleisli[F, A, B]

  ///////////////////////////////////////////////////////////////////////////////

  val friendlyEcho: Kleisli[Sync, String, Unit] = for {
    _ <- Kleisli((name: String) => putStrLn(s"Hello $name! Please enter something to be echoed:"))
    str <- Kleisli((_: String) => getStrLn)
    _ <- Kleisli((name: String) => putStrLn("Echoing: " + str + s". Have a nice day $name!"))
  } yield ()

  //  friendlyEcho.run("Max").unsafeInterpret()

  ///////////////////////////////////////////////////////////////////////////////
  // Semigroups and Monoids
  ///////////////////////////////////////////////////////////////////////////////
  // DOCUMENTATION:
  // - https://en.wikipedia.org/wiki/Algebraic_structure
  // - https://typelevel.org/cats/typeclasses/monoid.html

  trait Semigroup[A] {
    def <>(here: A)(there: A): A
  }

  object Semigroup {

    object Syntax {

      implicit class SemigroupIdSyntax[M](here: M) {
        def <>(there: M)(implicit instance: Semigroup[M]): M = {
          instance.<>(here)(there)
        }
      }

    }

    object Instances {
      implicit def listSemigroupInstance[A]: Semigroup[List[A]] = new Semigroup[List[A]] {
        override def <>(here: List[A])(there: List[A]): List[A] =
          here ++ there
      }

      implicit def sumSemigroupInstance[A]: Semigroup[Int] = new Semigroup[Int] {
        override def <>(here: Int)(there: Int): Int =
          here + there
      }

      implicit def mapRightBiasSemigroupInstance[K, V]: Semigroup[Map[K, V]] = new Semigroup[Map[K, V]] {
        override def <>(here: Map[K, V])(there: Map[K, V]): Map[K, V] = {
          var collector: Map[K, V] = Map.empty

          here.foreach { kv =>
            collector = collector + kv
          }

          there.foreach { kv =>
            collector = collector + kv
          }

          collector
        }
      }
    }

  }

  trait Monoid[A] {
    implicit def semigroupInstance: Semigroup[A]

    def mempty: A
  }

  object Monoid {

    object Instances {
      implicit def listMonoidInstance[A]: Monoid[List[A]] = new Monoid[List[A]] {
        override def semigroupInstance: Semigroup[List[A]] = Semigroup.Instances.listSemigroupInstance

        override def mempty: List[A] = List.empty
      }

      implicit def sumMonoidInstance: Monoid[Int] = new Monoid[Int] {
        override def semigroupInstance: Semigroup[Int] = Semigroup.Instances.sumSemigroupInstance

        override def mempty: Int = 0
      }

      implicit def mapRightBiasMonoidInstance[K, V]: Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
        override def semigroupInstance: Semigroup[Map[K, V]] = Semigroup.Instances.mapRightBiasSemigroupInstance

        override def mempty: Map[K, V] = Map.empty
      }
    }

    import Semigroup.Syntax._

    def mempty[M](implicit instance: Monoid[M]): M = instance.mempty

    def mconcat[M: Monoid](xs: List[M]): M = {
      implicit val semigroupInstance: Semigroup[M] = implicitly[Monoid[M]].semigroupInstance
      xs.foldRight(mempty)(_ <> _)
    }

    def mconcatMap[M: Monoid, G](xs: List[G])(f: G => M): M = {
      mconcat(xs.map(f))
    }
  }

  import Monoid.Instances._
  import Semigroup.Instances._
  import Semigroup.Syntax._

  println(1 <> Monoid.mempty[Int]) // 1
  println(Monoid.mempty[Int] <> 1) // 1
  println(1 <> 2) // 3

  println(Monoid.mconcat(List(1, 2, 3))) // 6

  println(Map("hello" -> "world") <> Map("my name" -> "is Max")) // // Map(hello -> world, my name -> is Max)

  println(Monoid.mconcat(
    List(
      Map("hello" -> "world"),
      Map("my name" -> "is Max"),
    ))
  ) // Map(hello -> world, my name -> is Max)

  ///////////////////////////////////////////////////////////////////////////////
  // The Free Monoid
  ///////////////////////////////////////////////////////////////////////////////

  sealed trait FileOp

  case class WriteFile(filename: String, contents: String) extends FileOp

  case class ReadFile(filename: String) extends FileOp

  case class DeleteFile(filename: String) extends FileOp

  def writeFile(filename: String, contents: String): List[FileOp] =
    List(WriteFile(filename, contents))

  def readFile(name: String): List[FileOp] =
    List(ReadFile(name))

  def deleteFile(filename: String): List[FileOp] =
    List(DeleteFile(filename))

  import Semigroup.Instances._
  import Semigroup.Syntax._
  import Monoid.Instances._

  val program: List[FileOp] = {
    writeFile("a.txt", "a") <>
      deleteFile("b.txt") <>
      writeFile("b.txt", "b")
  }

  import java.io.PrintWriter

  def prodInterpreter(op: FileOp): List[Unit] = {
    op match {
      case WriteFile(filename, contents) => {
        new PrintWriter(filename) {
          write(contents)
          close()
        }
      }
      case ReadFile(filename) => {
        val source = Source.fromFile(filename)
        val lines: String = try source.mkString finally source.close()
        println(lines)
      }
      case DeleteFile(filename) => {
        new File(filename).delete()
      }
    }

    List()
  }

  def testInterpreter(op: FileOp): Map[String, Option[String]] = {
    op match {
      case WriteFile(filename, contents) =>
        Map(filename -> Some(contents))
      case ReadFile(filename) =>
        Map.empty
      case DeleteFile(filename) =>
        Map[String, Option[String]](filename -> None)
    }
  }

  //  println(Monoid.mconcatMap(program)(prodInterpreter))
  //  println(Monoid.mconcatMap(program)(testInterpreter))

  // 🤔
  def appendFile(filename: String, contents: String): List[FileOp] = {
    val oldContents = readFile(filename)
    deleteFile(filename) <> writeFile(filename, oldContents + contents)
  }

  ///////////////////////////////////////////////////////////////////////////////

  sealed trait LinkedList[+A] {
    self =>
    def map[B](f: A => B): LinkedList[B] = {
      self match {
        case Link(head, tail) => Link(f(head), tail.map(f))
        case End => End
      }
    }
  }

  case class Link[+A](head: A, tail: LinkedList[A]) extends LinkedList[A]

  case object End extends LinkedList[Nothing]

  val ll = Link(1, Link(2, Link(3, End)))

  //  println(ll.map(_ + 1))

  ///////////////////////////////////////////////////////////////////////////////
  // The Free Monad
  ///////////////////////////////////////////////////////////////////////////////
  // https://stackoverflow.com/a/13388966/5835579

  type ~>[F[_], G[_]] = NaturalTransformation[F, G]

  trait NaturalTransformation[F[_], G[_]] {
    self =>
    def apply[A](fa: F[A]): G[A]
  }

  sealed trait Free[G[_], A] {
    self: Free[G, A] =>
    def foldMap[F[_] : Monad](nt: G ~> F): F[A] = {
      self match {
        case Point(a) => a.point[F]
        case Suspend(ga) => nt(ga)
        case FlatMapped(fa, f) =>
          // HERE BE DRAGONS: This is not stack safe - you have to use a
          // "Trampoline" because we can't do TCO polymorphically.
          // DO NOT USE THIS IN PROD
          fa.foldMap[F](nt).flatMap(a => f(a).foldMap[F](nt))
      }
    }
  }

  final case class Point[G[_], A](a: A) extends Free[G, A]

  final case class Suspend[G[_], A](ga: G[A]) extends Free[G, A]

  final case class FlatMapped[G[_], A, B](fa: Free[G, A], f: A => Free[G, B]) extends Free[G, B]

  object Free {
    def suspend[G[_], A](ga: G[A]): Free[G, A] =
      Suspend(ga)

    object Instances {
      implicit def freeFunctorInstance[G[_]]: Functor[Free[G, ?]] = new Functor[Free[G, ?]] {
        def map[A, B](fa: Free[G, A])(f: A => B): Free[G, B] =
          freeMonadInstance.flatMap(fa)(a => Point(f(a)))
      }

      implicit def freeMonadInstance[G[_]]: Monad[Free[G, ?]] = new Monad[Free[G, ?]] {
        override implicit def functorInstance: Functor[Free[G, ?]] = freeFunctorInstance

        def point[A](a: A): Free[G, A] = Point(a)

        override def flatMap[A, B](fa: Free[G, A])(f: A => Free[G, B]): Free[G, B] =
          FlatMapped(fa, f)
      }
    }

  }

  ///////////////////////////////////////////////////////////////////////////////
  // The Free Monad Usage
  ///////////////////////////////////////////////////////////////////////////////

  sealed trait FileOpM[A]

  case class WriteFileM(filename: String, contents: String) extends FileOpM[Unit]

  case class ReadFileM(filename: String) extends FileOpM[String]

  case class DeleteFileM(filename: String) extends FileOpM[Unit]

  def writeFileM(filename: String, contents: String): Free[FileOpM, Unit] =
    Free.suspend(WriteFileM(filename, contents))

  def readFileM(filename: String): Free[FileOpM, String] =
    Free.suspend(ReadFileM(filename))

  def deleteFileM(filename: String): Free[FileOpM, Unit] =
    Free.suspend(DeleteFileM(filename))

  import Free.Instances._

  def appendFileM(filename: String, contents: String): Free[FileOpM, Unit] = for {
    oldContents <- readFileM(filename)
    _ <- deleteFileM(filename)
    _ <- writeFileM(filename, oldContents + contents)
  } yield ()

  val programM = for {
    _ <- writeFileM("aM.txt", "aM")
    _ <- deleteFileM("aM.txt")
    _ <- writeFileM("bM.txt", "bM")
    _ <- appendFileM("bM.txt", "cM")
  } yield ()

  def prodInterpreterM: FileOpM ~> Sync = new (FileOpM ~> Sync) {
    override def apply[A](fa: FileOpM[A]): Sync[A] =
      fa match {
        case WriteFileM(filename, contents) => Sync.effect {
          new PrintWriter(filename) {
            write(contents)
            close()
          }
          ()
        }
        case ReadFileM(filename) => Sync.effect {
          val source = Source.fromFile(filename)
          val lines: String = try source.mkString finally source.close()
          lines
        }
        case DeleteFileM(filename) => Sync.effect {
          new File(filename).delete()
          ()
        }
      }
  }

  type TestState = Map[String, String]
  type TestResult[A] = State[TestState, A]

  def testInterpreterM: FileOpM ~> TestResult = new (FileOpM ~> TestResult) {
    override def apply[A](fa: FileOpM[A]): TestResult[A] =
      fa match {
        case WriteFileM(filename, contents) =>
          State.modify[TestState] { s: TestState =>
            s.+((filename, contents))
          }
        case ReadFileM(filename) =>
          State.get[TestState]().map { s =>
            s(filename)
          }
        case DeleteFileM(filename) =>
          State.modify[TestState] { s: TestState =>
            s.view.filterKeys(_ != filename).toMap
          }
      }
  }

  programM.foldMap(prodInterpreterM).unsafeInterpret()
  programM.foldMap(testInterpreterM).run(Map.empty)

  ///////////////////////////////////////////////////////////////////////////////
  // Why is it called the Free Monad
  ///////////////////////////////////////////////////////////////////////////////

  // def mconcatMap[M: Monoid, G]  (xs: List[G])    (f: G => M):   M = {
  // def foldMap[F[_]: Monad, G, A](fa: Free[G, A])(nt: G ~> F): F[A] = {

  ///////////////////////////////////////////////////////////////////////////////

  trait Applicative[F[_]] {
    implicit def functorInstance: Functor[F]

    def pure[A](a: A): F[A]

    // can define one or the other
    def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] = {
      val apply =
        (f: A => B) => (a: A) => f(a)

      liftA2(apply)(ff)(fa)
    }

    // you might want to define `liftA2` because mapping over your `F` might
    // be an expensive operation
    def liftA2[A, B, C](f: A => B => C): F[A] => F[B] => F[C] =
      (fa: F[A]) => (fb: F[B]) => ap(fa.map(f))(fb)
  }

  object Applicative {
    object Syntax {
      implicit class ApplicativeIdExtensions[A](private val self: A) extends AnyVal {
        def point[F[_]]()(implicit instance: Applicative[F]): F[A] = {
          instance.pure(self)
        }
      }

      implicit final class ApplicativeExtensions[F[_], A, B](private val self: F[A => B]) extends AnyVal {
        def ap(fa: F[A])(implicit instance: Applicative[F]): F[B] = {
          instance.ap(self)(fa)
        }
      }
    }

    object Instances {
      implicit val optionApplicative: Applicative[Option] = new Applicative[Option] {

        override def functorInstance: Functor[Option] = Functor.Instances.optionFunctorInstance

        override def pure[A](a: A): Option[A] = Some(a)

        override def ap[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] =
          (ff, fa) match {
            case (Some(f), Some(a)) => Some(f(a))
            case _ => None
        }
      }
    }
  }

  // Seen in a previous episode
//  def sumAgeOfPersonAndParentsC(member: FamilyMember): Option[Int] = {
//    for {
//      mother <- member.mother
//      father <- member.father
//    } yield member.age + mother.age + father.age
//  }

  import Applicative.Syntax._
  import Applicative.Instances._

  def sumAgeOfPersonAndParentsA(member: FamilyMember): Option[Int] = {

    val mother: Option[FamilyMember] = member.mother
    val father: Option[FamilyMember] = member.father


    val add = (x: Int) => (y: Int) => x + y

    mother.map(add).ap(father)
  }

  object ApplicativeUtils {
    def deriveAp[F[_]: Monad, A, B]: F[A => B] => F[A] => F[B] = {
      implicit val functorInstance = implicitly[Monad[F]].functorInstance

      (ff: F[A => B]) => (fa: F[A]) => for {
          f <- ff
          a <- fa
      } yield f(a)
    }
  }
}
