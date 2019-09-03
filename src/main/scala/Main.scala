
object Main extends App {

  // Things may appear in a weird order. I'm doing that so that you know every
  // thing you need to know before you get to it, so you don't see something
  // weird and think 'what the hell is that'

  ///////////////////////////////////////////////////////////////////////////////
  // Polymorphism
  ///////////////////////////////////////////////////////////////////////////////

  trait Animal {
    def sound: String
  }

  case class Dog() extends Animal {
    def sound: String = "woof"
  }

  case class Cat() extends Animal {
    def sound: String = "meow"
  }

  ///////////////////////////////////////////////////////////////////////////////

  def makeSound(animal: Animal): Unit = {
    println(animal.sound)
  }

  //  makeSound(Cat())
  //  makeSound(Dog())

  ///////////////////////////////////////////////////////////////////////////////

  object ThirdPartyLibrary {

    case class Rabbit()

  }

  import ThirdPartyLibrary.Rabbit

  // makeSound(Rabbit())

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
  // noParameterList

  ///////////////////////////////////////////////////////////////////////////////
  // Implicits
  ///////////////////////////////////////////////////////////////////////////////

  implicit val implicitString: String = "implicit String"

  def implicitString()(implicit x: String) = x

  // println(implicitString()) // implicit String

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

  implicit val implicitBoolean: Boolean = true
  implicit val implicitInt: Int = 5

  def rawImplicitly[T](implicit x: T): T = {
    x
  }

//   println(rawImplicitly[Boolean]) // true
//   println(rawImplicitly[Int]) // 5

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

  object SoundSyntax {

    implicit class CanMakeSoundIdExtensions[A](private val self: A) extends AnyVal {
      def sound_()(implicit instance: Sound[A]): String = {
        instance.sound(self)
      }
    }

  }

  object SoundInstances {
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

  ////////////////////////////////////////////////////////////////////////////////
  // `Sound` consumption
  ////////////////////////////////////////////////////////////////////////////////

  import SoundInstances._
  import SoundSyntax._

  def makeSoundImplicitParam[A](a: A)(implicit instance: Sound[A]): Unit = {
    println(a.sound_())
  }

  //  makeSoundImplicitParam(Dog())
  //  makeSoundImplicitParam(Cat())
  //  makeSoundImplicitParam(Rabbit()) // this now works!

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

  object EncodeSyntax {

    implicit class EncodeIdExtensions[A](private val self: A) extends AnyVal {
      def encode()(implicit instance: Encode[A]): Json = {
        instance.encode(self)
      }
    }

  }

  object EncodeInstances {
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


  ////////////////////////////////////////////////////////////////////////////////
  // `Encode` consumption
  ////////////////////////////////////////////////////////////////////////////////

  import EncodeInstances._
  import EncodeSyntax._

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

  object FunctorSyntax {

    implicit final class FunctorExtensions[F[_], A](private val self: F[A]) extends AnyVal {
      def map[B](f: A => B)(implicit instance: Functor[F]): F[B] = {
        instance.map(self)(f)
      }
    }

  }

  object FunctorInstances {
    // @ List(1, 2, 3).map(x => x + 1)
    // res0: List[Int] = List(2, 3, 4)
    implicit val listFunctorInstance: Functor[List] = new Functor[List] {
      def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f) // very scary reimplementation, let's not do that and use the stdlib
    }

    implicit val optionFunctorInstance: Functor[Option] = new Functor[Option] {
      def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa match {
        case Some(a) => Some(f(a))
        case None => None
      }
    }
  }

  ////////////////////////////////////////////////////////////////////////////////
  // Functor usage
  ////////////////////////////////////////////////////////////////////////////////

  import FunctorInstances._
  import FunctorSyntax._

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

  trait Monad[F[_]] {
    implicit def functorInstance: Functor[F]

    def pure[A](a: A): F[A]

    // you only have to implement one or the other, as each can be derived from each others impl
    def flatten[A](ffa: F[F[A]]): F[A] =
      flatMap(ffa)(x => x)

    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] =
      flatten(fa.map(f))
  }

  object MonadSyntax {

    implicit class MonadIdExtensions[A](private val self: A) extends AnyVal {
      def pure[F[_]]()(implicit instance: Monad[F]): F[A] = {
        instance.pure(self)
      }
    }

    implicit final class MonadMonadExtensions[F[_], A](private val self: F[F[A]]) extends AnyVal {
      def flatten[B](implicit instance: Monad[F]): F[A] = {
        instance.flatten(self)
      }
    }

    implicit final class MonadExtensions[F[_], A](private val self: F[A]) extends AnyVal {
      def flatMap[B](f: A => F[B])(implicit monadInstance: Monad[F]): F[B] = {
        monadInstance.flatMap(self)(f)
      }
    }

  }

  object MonadInstances {
    implicit val listMonadInstance: Monad[List] = new Monad[List] {
      override implicit def functorInstance: Functor[List] = listFunctorInstance

      def pure[A](a: A): List[A] = List(a)

      override def flatten[A](ffa: List[List[A]]): List[A] =
        ffa.flatten
    }

    implicit val optionMonadInstance: Monad[Option] = new Monad[Option] {
      override implicit def functorInstance: Functor[Option] = optionFunctorInstance

      def pure[A](a: A): Option[A] = Some(a)

      override def flatten[A](ffa: Option[Option[A]]): Option[A] = ffa match {
        case Some(Some(a)) => Some(a)
        case _ => None
      }
    }
  }

  ////////////////////////////////////////////////////////////////////////////////
  // Monad usage
  ////////////////////////////////////////////////////////////////////////////////

  import MonadInstances._
  import MonadSyntax._

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

        def pure[A](a: A): Sync[A] = Sync.effect(a)

        override def flatten[A](ffa: Sync[Sync[A]]): Sync[A] =
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

  // is equivalent equivalent to
  //   putStrLn("Please enter something to be echoed:")
  //     .flatMap(_ => getStrLn
  //       .flatMap(str =>
  //         putStrLn("Echoing: " + str)
  //     )
  //   )

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

  object Function1FunctorInstances {
    implicit def function1FunctorInstance[R]: Functor[R => ?] = new Functor[R => ?] {
      def map[A, B](fa: R => A)(f: A => B): R => B = fa.andThen(f)
//       def map[A, B](fa: R => A)(f: A => B): R => B =
//        { r: R =>
//          val a: A = fa(r)
//          val b: B = f(a)
//          b
//        }
    }

  }

  import FunctorSyntax._
  import Function1FunctorInstances._

  val isBigNumberM: Int => Boolean = showInt.map(isBigString)

//  println(isBigNumberM(10000)) // true

  ///////////////////////////////////////////////////////////////////////////////

  object Function1MonadInstances {
    implicit def function1MonadInstance[R]: Monad[R => ?] = new Monad[R => ?] {
      override implicit def functorInstance: Functor[R => ?] = function1FunctorInstance

      def pure[A](a: A): R => A =
        (_: R) => a

      override def flatMap[A, B](ffa: R => A)(f: A => (R => B)): R => B =
      { r: R =>
        val a: A = ffa(r)
        val fb: R => B = f(a)
        val b: B = fb(r)
        b
      }
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
            val (sp, a) = fa.run(s)
            (sp, f(a))
          })
      }

      implicit def stateMonadInstance[S]: Monad[State[S, ?]] = new Monad[State[S, ?]] {
        def functorInstance: Functor[State[S, ?]] = stateFunctorInstance

        def pure[A](a: A): State[S, A] =
          State(s => (s, a))

        override def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] =
          State(s => {
            val (sp, a) = fa.run(s)
            val fb: State[S, B] = f(a)
            fb.run(sp)
          })
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
//  println(computation.run(0)) // (8, The final value is 8)

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
        def map[A, B](fa: Kleisli[F, R, A])(f: A => B): Kleisli[F, R, B] = {
          implicit val functorInstance: Functor[F] = implicitly[Monad[F]].functorInstance

          Kleisli(r => fa.run(r).map(f)) // demo types
        }
      }

      implicit def kleisliMonadInstance[F[_] : Monad, R]: Monad[Kleisli[F, R, ?]] = new Monad[Kleisli[F, R, ?]] {
        override implicit def functorInstance: Functor[Kleisli[F, R, ?]] = kleisliFunctorInstance

        def pure[A](a: A): Kleisli[F, R, A] =
          Kleisli(r => a.pure[F])

        override def flatten[A](ffa: Kleisli[F, R, Kleisli[F, R, A]]): Kleisli[F, R, A] =
          Kleisli(r => ffa.run(r).flatMap(_.run(r))) // demo with rewrite
      }
    }

  }

  import Kleisli.Instances._

  def getMother(familyMember: FamilyMember): Option[FamilyMember] = {
    familyMember.mother
  }

  def getMotherK: Kleisli[Option, FamilyMember, FamilyMember] = {
    Kleisli(getMother)
  }

  val getGreatGrandmotherAgeR: FamilyMember => Option[Int] =
    getMotherK.andThen(getMotherK).andThen(getMotherK).map(_.age).run

  //  println(getGreatGrandmotherAgeR(son)) // Some(103)

  ///////////////////////////////////////////////////////////////////////////////
  // Fake Akka directives
  ///////////////////////////////////////////////////////////////////////////////

  case class HttpRequest(method: String, parameters: Map[String, String], body: Option[String])

  case class HttpResponse(body: String)

  case class JwtToken()

  def POST(request: HttpRequest): Option[Unit] = {
    if (request.method == "POST")
      Some(())
    else
      None
  }

  def PUT(request: HttpRequest): Option[Unit] = {
    if (request.method == "PUT")
      Some(())
    else
      None
  }

  def extractBody(request: HttpRequest): Option[String] =
    request.body

  def extractHeader(name: String)(request: HttpRequest): Option[String] =
    request.parameters.get(name)

  def putHandler: Kleisli[Option, HttpRequest, HttpResponse] = for {
    _ <- Kleisli(PUT)
    authHeader <- Kleisli(extractHeader("Authorisation"))
  } yield HttpResponse("")

  def postHandler: Kleisli[Option, HttpRequest, HttpResponse] = for {
    _ <- Kleisli(POST)
    authHeader <- Kleisli(extractHeader("Authorisation"))
    body <- Kleisli(extractBody)
  } yield HttpResponse(body)

  //  println(echoController.run(
  //    HttpRequest(
  //      "POST",
  //      Map("Authorisation" -> "Yes"),
  //      Some("Hello world")
  //  ))) // Some(HttpResponse("Hello world"))

  //  println(echoController.run(
  //    HttpRequest(
  //      "GET",
  //      Map("Authorisation" -> "Yes"),
  //      Some("Hello world")
  //  ))) // None

  type ReaderT[F[_], A, B] = Kleisli[F, A, B]

  ///////////////////////////////////////////////////////////////////////////////
  // Semigroups and Monoids
  ///////////////////////////////////////////////////////////////////////////////
  //  https://en.wikipedia.org/wiki/Algebraic_structure

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
    }

  }

  trait Monoid[A] {
    implicit def semigroupInstance: Semigroup[A]

    def identity: A
  }

  object Monoid {
    object Syntax {
      def identity[M](implicit instance: Monoid[M]): M = instance.identity
    }

    object Instances {
      implicit def listMonoidInstance[A]: Monoid[List[A]] = new Monoid[List[A]] {
        override def semigroupInstance: Semigroup[List[A]] = Semigroup.Instances.listSemigroupInstance
        override def identity: List[A] = List.empty
      }

      implicit def sumMonoidInstance: Monoid[Int] = new Monoid[Int] {
        override def semigroupInstance: Semigroup[Int] = Semigroup.Instances.sumSemigroupInstance
        override def identity: Int = 0
      }
    }

    import Semigroup.Syntax._
    import Syntax._

    def mconcat[M: Monoid](xs: List[M]): M = {
      implicit val semigroupInstance: Semigroup[M] = implicitly[Monoid[M]].semigroupInstance
      xs.foldRight(identity)(_ <> _)
    }

    def mconcatMap[M: Monoid, A](xs: List[A])(f: A => M): M = {
      mconcat(xs.map(f))
    }
  }

  ///////////////////////////////////////////////////////////////////////////////
  // The Free Monoid
  ///////////////////////////////////////////////////////////////////////////////

  sealed trait Operation

  case object SpecifyTarget extends Operation
  case class FindTarget(name: String) extends Operation
  case class LoadTarget(location: (Int, Int)) extends Operation

  case class CheckTarget(location: (Int, Int)) extends Operation
  case object Fire extends Operation

  def specifyTarget(): List[Operation] =
    List(SpecifyTarget)

  def findTarget(name: String): List[Operation] =
    List(FindTarget(name))

  def loadTarget(location: (Int, Int)): List[Operation] =
    List(LoadTarget(location))

  def checkTarget(location: (Int, Int)): List[Operation] =
    List(CheckTarget(location))

  def fire(): List[Operation] =
    List(Fire)

  import Semigroup.Instances._
  import Semigroup.Syntax._
  import Monoid.Instances._

  def prepare(): List[Operation] =
    specifyTarget() <> findTarget("Max") <> loadTarget((82, 43))

  def launch(): List[Operation] =
    checkTarget((82, 43)) <> fire()

  def mission: List[Operation] =
    prepare() <> launch()

  def prodInterpreter(op: Operation): List[Unit] = {
    op match {
      case SpecifyTarget => println("Accepting target specification")
      case FindTarget(name) => println(s"Locating target called $name")
      case LoadTarget(location) => println(s"Locking missile onto target of coordinate $location")
      case CheckTarget(location) => println(s"Checking for civilians at coordinate $location")
      case Fire => println("Fox-3")
    }

    List(())
  }

  def testInterpreter(op: Operation): List[Operation] = {
    List(op)
  }

//  Monoid.mconcatMap(mission)(prodInterpreter)
//  println(Monoid.mconcatMap(mission)(testInterpreter))

  ///////////////////////////////////////////////////////////////////////////////
  // The Free Monad
  ///////////////////////////////////////////////////////////////////////////////
  // https://stackoverflow.com/a/13388966/5835579

  type ~>[F[_], G[_]] = NaturalTransformation[F, G]

  trait NaturalTransformation[F[_], G[_]] { self =>
    def apply[A](fa: F[A]): G[A]
  }

  sealed trait Free[G[_], A] { self: Free[G, A] =>
    def foldMap[F[_]: Monad](nt: G ~> F): F[A] = {
      self match {
        case Pure(a) => a.pure[F]
        case Suspend(ga) => nt(ga)
        case FlatMapped(free, f) =>
          // do not try this at home
          free.foldMap[F](nt).flatMap(a => f(a).foldMap[F](nt))
      }
    }
  }

  final case class Pure[G[_], A](a: A) extends Free[G, A]
  final case class Suspend[G[_], A](ga: G[A]) extends Free[G, A]
  final case class FlatMapped[G[_], A, B](fa: Free[G, A], f: A => Free[G, B]) extends Free[G, B]

  object Free {
    def suspend[G[_], A](ga: G[A]): Free[G, A] =
      Suspend(ga)

    object Instances {
      implicit def freeFunctorInstance[G[_]]: Functor[Free[G, ?]] = new Functor[Free[G, ?]] {
        def map[A, B](fa: Free[G, A])(f: A => B): Free[G, B] =
          freeMonadInstance.flatMap(fa)(a => Pure(f(a)))
      }

      implicit def freeMonadInstance[G[_]]: Monad[Free[G, ?]] = new Monad[Free[G, ?]] {
        override implicit def functorInstance: Functor[Free[G, ?]] = freeFunctorInstance

        def pure[A](a: A): Free[G, A] = Pure(a)

        override def flatMap[A, B](fa: Free[G, A])(f: A => Free[G, B]): Free[G, B] =
          FlatMapped(fa, f)
      }
    }
  }

  ///////////////////////////////////////////////////////////////////////////////
  // The Free Monad Usage
  ///////////////////////////////////////////////////////////////////////////////

  sealed trait OperationM[A]

  case object SpecifyTargetM extends OperationM[String]
  case class FindTargetM(name: String) extends OperationM[(Int, Int)]
  case class LoadTargetM(location: (Int, Int)) extends OperationM[Unit]

  case class CheckTargetM(location: (Int, Int)) extends OperationM[Boolean]
  case object FireM extends OperationM[Unit]
  case object AbortM extends OperationM[Unit]

  def specifyTargetM(): Free[OperationM, String] =
    Free.suspend(SpecifyTargetM)

  def findTargetM(name: String): Free[OperationM, (Int, Int)] =
    Free.suspend(FindTargetM(name: String))

  def loadTargetM(location: (Int, Int)): Free[OperationM, Unit] =
    Free.suspend(LoadTargetM(location))

  def checkTargetM(location: (Int, Int)): Free[OperationM, Boolean] =
    Free.suspend(CheckTargetM(location))

  def fireM(): Free[OperationM, Unit] =
    Free.suspend(FireM)

  def abortM(): Free[OperationM, Unit] =
    Free.suspend(AbortM)


  import Free.Instances._

  def prepareM(): Free[OperationM, (Int, Int)] = for {
    name <- specifyTargetM()
    coords <- findTargetM(name)
    _ <- loadTargetM(coords)
  } yield coords

  def launchM(location: (Int, Int)): Free[OperationM, Unit] = {
    for {
      valid <- checkTargetM(location)
      _ <- if (valid)
        fireM()
      else
        abortM()
    } yield ()
  }

  def missionM: Free[OperationM, Unit] = for {
    target <- prepareM()
    _ <- launchM(target)
  } yield ()

  def prodInterpreterM: OperationM ~> Sync = new (OperationM ~> Sync) {
    override def apply[A](fa: OperationM[A]): Sync[A] =
      fa match {
        case SpecifyTargetM =>
          getStrLn
        case FindTargetM(name) =>
          if (name == "Max")
            (82, 43).pure[Sync]
          else
            (20, 36).pure[Sync]
        case LoadTargetM(location) =>
          putStrLn(s"Locking missile onto target of coordinate $location")
        case CheckTargetM(location) =>
          ((82, 43) != location).pure[Sync]
        case FireM =>
          putStrLn("Fox-3")
        case AbortM =>
          putStrLn("Don't try and kill Max")
      }
  }

  val syncMission: Sync[Unit] = missionM.foldMap(prodInterpreterM)

  syncMission.unsafeInterpret()
  syncMission.unsafeInterpret()

  ///////////////////////////////////////////////////////////////////////////////
  // "Mapped" types
  ///////////////////////////////////////////////////////////////////////////////

  // https://www.typescriptlang.org/docs/handbook/advanced-types.html#mapped-types
  type Partial[T[_[_]]] = T[Option]
  type All[T[_[_]]] = T[Id]

  sealed trait CustomerShape[F[_]] {
    def name: F[String]

    def description: F[String]
  }

  final case class InsertCustomer(
    name: String,
    description: String
  ) extends All[CustomerShape]

  final case class UpdateCustomer(
    name: Option[String],
    description: Option[String]
  ) extends Partial[CustomerShape]

  ///////////////////////////////////////////////////////////////////////////////


  //  def concatMap[A, M: Monoid](xs: List[A], interp: A => M): M
  //  def concatFlatMap[A, G, M: Monad](xs: Free[F, A], interp: G[A] ~> M[A]): M[A]

  trait Applicative[F[_]] {
//    implicit def functorInstance: Functor[F]
//
//    def pure[A](a: A): F[A]
    def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]
  }

  object ApplicativeInstances {
    implicit val optionApplicative: Applicative[Option] = new Applicative[Option] {
      override def ap[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] = {
        (ff, fa) match {
          case (Some(f), Some(a)) => Some(f(a))
          case _ => None
        }
      }
    }
  }
}