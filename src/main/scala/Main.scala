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

  implicit val implicitBoolean:   Boolean = true
  implicit val implicitInt:       Int     = 5

  def rawImplicitly[T](implicit x: T): T  = {
    x
  }

  // println(rawImplicitly[Boolean]) // true
  // println(rawImplicitly[Int]) // 5

  ///////////////////////////////////////////////////////////////////////////////

  implicit val implicitListInt:       List[Int]   = List(5)
  implicit val implicitOptionInt:     Option[Int] = Some(5)

  def hktImplicitly[F[_]](implicit x: F[Int]): F[Int]  = {
    x
  }

  // scala> :k List
  // List's kind is F[+A]

  // println(hktImplicitly[List]) // List(5)
  // println(hktImplicitly[Option]) // Some(5)

  ///////////////////////////////////////////////////////////////////////////////

  type Id[A] = A
  // * -> *

  // scala> :k Id
  // Id's kind is F[+A]

  implicit val implicitListString: List[String]   = List("hello")
  implicit val implicitListBoolean: List[Boolean]  = List(true)

  def hktAppImplicitly[F[_], A](implicit x: F[A]): F[A]  = {
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
        instance.sound(self)}
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

  val nana = FamilyMember(age=103)

  val grandma = FamilyMember(age=79, mother=Some(nana));     val grandad = FamilyMember(age=82)

  val mum = FamilyMember(age=55, mother=Some(grandma));     val dad = FamilyMember(age=56, father=Some(grandad))

  val son = FamilyMember(age=22, mother=Some(mum), father=Some(dad))

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
    familyMembers.map(_.age)
  }

  def getAgeFromOption(member: Option[FamilyMember]): Option[Int] = {
    member.map(_.age)
  }

//  println(getAgeFromList(family)) // List(103, 79, 82, 55, 56, 22)
//  println(getAgeFromList(grandma.parents)) // List()
//  println(getAgeFromOption(son.mother)) // Some(55)
//  println(getAgeFromOption(grandma.mother)) // None

  // How do we make something like this? 🤔
  // def getAge[F[_]: ???](f: F[FamilyMember]): F[Int] = {
  //   f.map(_.age)
  // }

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

  def getAge[F[_]: Functor](f: F[FamilyMember]): F[Int] = {
    f.map(_.age)
  }

  // println(getAge(family)) // List(79, 82, 55, 56, 22)
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

  def getAllAgesUpToGreatGrandparents(member: FamilyMember): List[List[List[Int]]] = {
    member.parents
      .map(parent => parent.parents
        .map(grandparent => grandparent.parents
          .map(greatGrandparent => greatGrandparent.age)))
  }

  ///////////////////////////////////////////////////////////////////////////////

  def flattenOption[A](ffa: Option[Option[A]]): Option[A] = {
    ffa match {
      case Some(Some(a)) => Some(a)
      case _ => None
    }
  }

  def getGrandparentAgeFlatten(member: FamilyMember): Option[Int] = {
    flattenOption(
      flattenOption(member.mother
        .map(mother => mother.mother)
      ).map(grandmother => grandmother.mother)
    ).map(greatGrandmother => greatGrandmother.age)
  }

  ///////////////////////////////////////////////////////////////////////////////

  trait Monad[F[_]] {
    def pure[A](a: A): F[A]
    def flatten[A](ffa: F[F[A]]): F[A]
    def flatMap[A, B](fa: F[A])(f: A => F[B])(implicit functor: Functor[F]): F[B] = {
      flatten(fa.map(f))
    }
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
      def flatMap[B](f: A => F[B])(implicit monadInstance: Monad[F], functorInstance: Functor[F]): F[B] = {
        monadInstance.flatMap(self)(f)
      }
    }
  }

  object MonadInstances {
    implicit val listMonadInstance: Monad[List] = new Monad[List] {
      def pure[A](a: A): List[A] = List(a)
      def flatten[A](ffa: List[List[A]]): List[A] = ffa.flatten
    }

    implicit val optionMonadInstance: Monad[Option] = new Monad[Option] {
      def pure[A](a: A): Option[A] = Some(a)

      def flatten[A](ffa: Option[Option[A]]): Option[A] = ffa match {
        case Some(Some(a)) => Some(a)
        case _ => None
      }
    }
  }

  ////////////////////////////////////////////////////////////////////////////////
  // Monad usage
  ////////////////////////////////////////////////////////////////////////////////

   def getGreatGrandMatriarchAgeM(member: FamilyMember): Option[Int] = {
     member.mother
       .flatMap(mother => mother.mother)
         .flatMap(grandmother => grandmother.mother)
            .map(greatGrandmother => greatGrandmother.age)
     //           ^             ^ multiple successive Monad operations
     //                           causes _eliminates nesting
   }

  def getAllAgesUpToGreatGrandparentsM(member: FamilyMember): List[Int] = {
    member.parents.flatMap(_.parents.map(_.age))
  }

//  println(getGrandMatriarchAgeM(son)) // Some(79)
//  println(getAllAgesUpToGrandparents_(son)) // List(79, 82, 55, 56, 22)


  ////////////////////////////////////////////////////////////////////////////////

  import FunctorInstances._
  import FunctorSyntax._
  import MonadInstances._
  import MonadSyntax._


  def getAllAgesUpToGrandparentsM(member: FamilyMember): List[Int] = {
    for {
      parent <- member.parents
      grandparents <- parent.parents

    } yield
  }


  def getGrandMatriarchAgeC(member: FamilyMember): Option[Int] = {
    for {
      mother <- member.mother
      grandmother <- mother.mother
    } yield grandmother.age
  }

//   println(getGrandMatriarchAgeC(son)) // Some(79)

  def getAllAgesUpToGrandparentsM(member: FamilyMember): List[Int] = {
    for {

    }
  }

  ///////////////////////////////////////////////////////////////////////////////

  class IO[A](val unsafeInterpret: () => A)

  object IO {
    def effect[A](eff: => A) = new IO(() => eff)
  }

  object MoreFunctorInstances {
    implicit val ioFunctorInstance: Functor[IO] = new Functor[IO] {
      def map[A, B](fa: IO[A])(f: A => B): IO[B] = IO.effect(f(fa.unsafeInterpret()))
    }
  }

  object MoreMonadInstances {
    implicit val ioMonadInstance: Monad[IO] = new Monad[IO] {
      def pure[A](a: A): IO[A] = IO.effect(a)

      def flatten[A](ffa: IO[IO[A]]): IO[A] =
        IO.effect(ffa.unsafeInterpret().unsafeInterpret())
    }
  }

  import FunctorSyntax._
  import MonadSyntax._
  import MoreFunctorInstances._
  import MoreMonadInstances._

  def putStrLn(line: String): IO[Unit] =
    IO.effect(println(line))

  val getStrLn: IO[String] =
    IO.effect(scala.io.StdIn.readLine())

  val echo: IO[Unit] = for {
    _        <- putStrLn("Please enter something to be echoed:")
    str      <- getStrLn
    _        <- putStrLn("Echoing: " + str)
  } yield ()

  // echo.unsafeInterpret()

  // is roughly equivalent to

  // putStrLn("Please enter something to be echoed:").flatMap(_ =>
  //   getStrLn.flatMap(str =>
  //     putStrLn("Echoing: " + str)
  //   )
  // )

  def poll(backoff: Int): IO[Int] = {
    // wait for backoff seconds before polling again
    // poll some endpoint
    // get the new endpoint provided backoff value
    IO.effect(backoff + 1)
  }

  def workingPoll3Times(): IO[Int] = {
    poll(backoff=1)
      .flatMap(poll)
      .flatMap(poll)
  }

  def workingPoll3TimesWithForNation(): IO[Int] = for {
    newBackoff <- poll(backoff=1)
    newBackoff2 <- poll(newBackoff)
    finalBackoff <- poll(newBackoff2)
  } yield finalBackoff

  // println(workingPoll3Times().unsafeInterpret())
  // println(workingPoll3TimesWithForNation().unsafeInterpret())

  ///////////////////////////////////////////////////////////////////////////////

  val showInt: Int => String = _.toString

  val unshowInt: String => Int = _.toInt

  object EvenMoreFunctorInstances {
    implicit def function1FunctorInstance[R <: AnyVal]: Functor[R => ?] = new Functor[R => ?] {
      def map[A, B](fa: R => A)(f: A => B): R => B = fa.andThen(f)
    }
  }

  import FunctorSyntax._
  import EvenMoreFunctorInstances._

  val roundtrip: Int => Int = showInt.map(unshowInt)

//   println(roundtrip(10))

  object EvenMoreMonadInstances {
    implicit def function1MonadInstance[R]: Monad[R => ?] = new Monad[R => ?] {

      def pure[A](a: A): R => A =
        (r: R) => a

      def flatten[A](ffa: R => (R => A)): R => A =
      { r: R =>
        val fa: (R) => A = ffa(r)
        val a: A = fa(r)
        a
      }
    }
  }

  import EvenMoreMonadInstances._

   val addDigits: Int => (Int, Int, Int) = for {
     addOne <- (x: Int) => x + 1
     addTwo <- (x: Int) => x + 2
     addThree <- (x: Int) => x + 3
   } yield (addOne, addTwo, addThree)

//   println(addDigits(0)) // (1, 2, 3)

  ///////////////////////////////////////////////////////////////////////////////
  // OUR NORTHSTAR

  case class HttpRequest(method: String, body: Option[String], parameters: Map[String, String])
  case class HttpResponse(body: String)
  case class JwtToken()

  // def POST(request: HttpRequest): Option[Unit]
  // def extractBody(request: HttpRequest): Option[String]
  // def extractHeader(name: String)(request: HttpRequest): Option[String]

  // def parseJWT(token: String): Option[JwtToken]

  // def echoController: HttpRequest => Option[HttpResponse] = for {
  //   _ <- POST
  //   body <- extractBody
  //   header <- extractHeader("Authorisation").map(_.flatMap(parseJWT))
  // } yield HttpResponse(body)


  // println(handler())

  //final case class Kleisli[F[_]: Monad, A, B](run: A => F[B]) {
  //  def compose[Z](k: Kleisli[F, Z, A]): Kleisli[F, Z, B] =
  //    Kleisli[F, Z, B](z => k.run(z).flatMap(run))
  //}

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
}