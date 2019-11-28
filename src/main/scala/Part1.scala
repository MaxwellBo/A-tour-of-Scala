import java.io.{File, PrintWriter}

import scala.concurrent.Future
import scala.io.Source
import pprint.log

object Part1 extends App {

  ////////////////////////////////////////////////////////////////////////////////
  // Datatypes that support `.map`
  ////////////////////////////////////////////////////////////////////////////////

//   sealed trait Option[A]
//   case class Some[A](value: A) extends Option[A]
//   case object None extends Option[Nothing]

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

  //  log(getParentsAges(son)) // List(55, 56)
  //  log(getParentsAges(grandma)) // List()
  //  log(getMothersAge(son)) // Some(55)
  //  log(getMothersAge(grandad)) // None

  ////////////////////////////////////////////////////////////////////////////////

  def getAgeFromList(familyMembers: List[FamilyMember]): List[Int] = {
    familyMembers.map(familyMember => familyMember.age)
  }

  def getAgeFromOption(member: Option[FamilyMember]): Option[Int] = {
    member.map(familyMember => familyMember.age)
  }

//    log(getAgeFromList(family)) // List(103, 79, 82, 55, 56, 22)
//    log(getAgeFromList(grandma.parents)) // List()
//    log(getAgeFromOption(son.mother)) // Some(55)
//    log(getAgeFromOption(grandad.mother)) // None

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

      implicit class FunctorExtensions[F[_], A](private val self: F[A]) extends AnyVal {
        def map[B](f: A => B)(implicit instance: Functor[F]): F[B] = {
          instance.map(self)(f)
        }
      }

    }

    object Instances {

      // @ List(1, 2, 3).map(x => x + 1)
      // res0: List[Int] = List(2, 3, 4)

      trait FunctorList extends Functor[List] {
        def map[A, B](fa: List[A])(f: A => B): List[B] =
          fa.map(f) // very scary reimplementation, let's not do that and use the stdlib
      }

      trait FunctorOption extends Functor[Option] {
        def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa match {
          // unwrap              apply
          //             rewrap
          case Some(a) => Some(f(a))
          case None => None
        }
      }

      implicit val listFunctorInstance: Functor[List] = new FunctorList {}
      implicit val optionFunctorInstance: Functor[Option] = new FunctorOption {}
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

  // equivalent to

//  def getAge[F[_]](f: F[FamilyMember])(implicit instance: Functor[F]): F[Int] = {
//    f.map(_.age)
//  }

   log(getAge(family)) // List(103, 79, 82, 55, 56, 22)
   log(getAge(son.mother)) // Some(55)

  ////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////

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

  //  log(getGreatGrandMatriarchAge(son)) // Some(Some(Some(103)))
  //  log(getGreatGrandMatriarchAge(mum)) // Some(Some(None))

  def getAllGreatGrandparentAges(member: FamilyMember): List[List[List[Int]]] = {
    member.parents
      .map(parent => parent.parents
        .map(grandparent => grandparent.parents
          .map(greatGrandparent => greatGrandparent.age)))
  }

  //  log(getAllGreatGrandparentAges(son)) // List(List(List(103)), List(List(104)))

  def sumAgeOfPersonAndParents(member: FamilyMember): Option[Option[Int]] = {
    member.mother.map(mother =>
      member.father.map(father =>
        member.age + mother.age + father.age
      )
    )
  }

  //  log(sumAgeOfPersonAndParents(son)) // Some(Some(133))
  //  log(sumAgeOfPersonAndParents(mum)) // Some(None)

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

  //  log(getGreatMatriarchAgeFlatten(son)) // Some(103)
  //  log(getGreatMatriarchAgeFlatten(mum)) // None

  ////////////////////////////////////////////////////////////////////////////////
  // Or a reasonable solution
  ////////////////////////////////////////////////////////////////////////////////
  // DOCUMENTATION:
  // https://typelevel.org/cats/typeclasses/monad.html

  trait Monad[F[_]] extends Applicative[F] {
    self: Applicative[F] =>
    def point[A](a: A): F[A] =
      pure(a)

    // you only have to implement one or the other, as each can be derived from each others impl
    def flatten[A](ffa: F[F[A]]): F[A] =
      flatMap(ffa)(x => x)

    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] =
      flatten(fa.map(f)(self))
  }

  object Monad {

    object Syntax {

      implicit class MonadIdExtensions[A](private val self: A) extends AnyVal {
        def point[F[_]](implicit instance: Monad[F]): F[A] = {
          instance.point(self)
        }
      }

      implicit class MonadMonadExtensions[F[_], A](private val self: F[F[A]]) extends AnyVal {
        def flatten[B](implicit instance: Monad[F]): F[A] = {
          instance.flatten(self)
        }
      }

      implicit class MonadExtensions[F[_], A](private val self: F[A]) extends AnyVal {
        def flatMap[B](f: A => F[B])(implicit instance: Monad[F]): F[B] = {
          instance.flatMap(self)(f)
        }
      }
    }

    object Instances {

      import Applicative.Instances._

      trait MonadList extends Monad[List] with ApplicativeList {
        override def point[A](a: A): List[A] =
          List(a)

        override def flatten[A](ffa: List[List[A]]): List[A] =
          ffa.flatten
      }

      trait MonadOption extends Monad[Option] with ApplicativeOption {
        override def point[A](a: A): Option[A] =
          Some(a)

        override def flatten[A](ffa: Option[Option[A]]): Option[A] = ffa match {
          // unwrap unwrap      rewrap
          case Some(Some(a)) => Some(a)
          case _ => None
        }
      }

      implicit val optionMonadInstance: Monad[Option] = new MonadOption {}
      implicit val listMonadInstance: Monad[List] = new MonadList {}
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
    //                           _eliminates_ nesting
  }

  //  log(getGreatGrandMatriarchAgeM(son)) // Some(79)
  //  log(getGreatGrandMatriarchAgeM(mum) // None

  def getAllGreatGrandparentAgesM(member: FamilyMember): List[Int] = {
    member.parents
      .flatMap(parent => parent.parents)
      .flatMap(grandparent => grandparent.parents)
      .map(greatGrandparent => greatGrandparent.age)
  }

  //  log(getAllGreatGrandparentAgesM(son)) // List(103, 104)

  def sumAgeOfPersonAndParentsM(member: FamilyMember): Option[Int] = {
    member.mother.flatMap(mother =>
      member.father.map(father =>
        member.age + mother.age + father.age
      )
    )
  }

  //  log(sumAgeOfPersonAndParents(son)) // Some(133)
  //  log(sumAgeOfPersonAndParents(mum)) // None

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
  // Laziness
  ///////////////////////////////////////////////////////////////////////////////

  val strictVal = {
    log("I will always be evaluated")
    "Strict val return value"
  }


  lazy val lazyVal = {
    log("I will only be evaluated when I'm used")
    "Lazy val return value"
  }

  def lazyDef = {
    log("I will be evaluated when I'm used")
    "Def return value"
  }

  def strictParameter(s: String) = {
  }

  def lazyParameter(s: => String) = {
  }

  //  log("Strict parameter, strict val:")
  //  strictParameter(strictVal) // doesn't print, evaluated previously
  //
  //  log("Strict parameter, lazy val:")
  //  strictParameter(lazyVal) // prints
  //
  //  log("Strict parameter, lazy def:")
  //  strictParameter(lazyDef) // prints
  //
  //  log("Lazy parameter, strict val:")
  //  lazyParameter(strictVal) // doesn't print, previously evaluated
  //
  //  log("Lazy parameter, lazy val:")
  //  lazyParameter(lazyVal) // doesn't print
  //
  //  log("Lazy parameter, lazy def:")
  //  lazyParameter(lazyDef) // doesn't print



  ///////////////////////////////////////////////////////////////////////////////
  // Fake Async
  ///////////////////////////////////////////////////////////////////////////////

  class Sync[A](val unsafeInterpret: () => A)

  object Sync {
    def suspend[A](eff: => A) = new Sync(() => eff)

    object Instances {

      import MoreSyncInstances._

      trait FunctorSync extends Functor[Sync] {
        def map[A, B](fa: Sync[A])(f: A => B): Sync[B] = Sync.suspend(f(fa.unsafeInterpret()))
      }

      trait MonadSync extends Monad[Sync] with ApplicativeSync {
        override def point[A](a: A): Sync[A] =
          Sync.suspend(a)

        override def flatten[A](ffa: Sync[Sync[A]]): Sync[A] =
        //                      unwrap unwrap
        // rewrap
          Sync.suspend(ffa.unsafeInterpret().unsafeInterpret())
      }

      implicit val syncFunctorInstance: Functor[Sync] = new FunctorSync {}
      implicit val syncMonadInstance: Monad[Sync] = new MonadSync {}
    }

  }

  ///////////////////////////////////////////////////////////////////////////////
  // Utilities
  ///////////////////////////////////////////////////////////////////////////////

  object IO {

    object Unsafe {
      def writeFile(filename: String, contents: String): Unit = {
        new PrintWriter(filename) {
          write(contents)
          close()
        }
      }

      def readFile(filename: String): String = {
        val source = Source.fromFile(filename)
        try source.mkString finally source.close()
      }

      def deleteFile(filename: String): Unit = {
        new File(filename).delete()
      }
    }

    object Safe {
      def putStrLn(line: String): Sync[Unit] =
        Sync.suspend(log(line))

      def getStrLn(): Sync[String] =
        Sync.suspend(scala.io.StdIn.readLine())

      def writeFile(filename: String, contents: String): Sync[Unit] =
        Sync.suspend(IO.Unsafe.writeFile(filename, contents))

      def readFile(filename: String): Sync[String] =
        Sync.suspend(IO.Unsafe.readFile(filename))

      def deleteFile(filename: String): Sync[Unit] =
        Sync.suspend(IO.Unsafe.deleteFile(filename))
    }

  }

  ///////////////////////////////////////////////////////////////////////////////
  // Sync usage
  ///////////////////////////////////////////////////////////////////////////////

  import Sync.Instances._

  val echo: Sync[Unit] = for {
    _ <- IO.Safe.putStrLn("Please enter something to be echoed:")
    str <- IO.Safe.getStrLn()
    _ <- IO.Safe.putStrLn("Echoing: " + str)
  } yield ()

  //   echo.unsafeInterpret()
  //   echo.unsafeInterpret()

  // is equivalent to
  //     IO.Safe.putStrLn("Please enter something to be echoed:")
  //       .flatMap(_ => IO.Safe.getStrLn()
  //         .flatMap(str =>
  //           IO.Safe.putStrLn("Echoing: " + str)
  //             .map(_ => ()
  //           )))

  def copy(from: String, to: String): Sync[Unit] = for {
    contents <- IO.Safe.readFile(from)
    _ <- IO.Safe.writeFile(to, contents)
  } yield ()

  // is equivalent to
  //       IO.Safe.readFile(contents)
  //         .flatMap(contents => IO.Safe.writeFile(to, contents)
  //            .map(_ => ())
  //         )

  copy("a.txt", "b.txt") // won't do anything
  copy("a.txt", "b.txt").unsafeInterpret() // will actually do stuff

  ///////////////////////////////////////////////////////////////////////////////
  // Function composition
  ///////////////////////////////////////////////////////////////////////////////

  val showInt: Int => String = _.toString
  val isBigString: String => Boolean = _.length >= 5

  val isBigNumber: Int => Boolean = showInt.andThen(isBigString)

  //  log(isBigNumber(10000)) // true

  ///////////////////////////////////////////////////////////////////////////////
  // The reader Functor
  ///////////////////////////////////////////////////////////////////////////////
  // DOCUMENTATION
  // - https://typelevel.org/cats/typeclasses/functor.html

  object Function1FunctorInstances {

    trait FunctorFunction1[R] extends Functor[R => ?] {
      def map[A, B](fa: R => A)(f: A => B): R => B = fa.andThen(f)

      //       def map[A, B](fa: R => A)(f: A => B): R => B =
      //        { r: R =>
      //          val a: A = fa(r) // unwrap
      //          val b: B = f(a) // apply
      //          b
      //        } // rewrap
    }

    implicit def function1FunctorInstance[R]: Functor[R => ?] = new FunctorFunction1[R] {}
  }

  import Function1FunctorInstances._

  val isBigNumberM: Int => Boolean = showInt.map(isBigString)

  //  log(isBigNumberM(10000)) // true

  ///////////////////////////////////////////////////////////////////////////////

  object Function1MonadInstances {

    import Applicative.Instances._

    trait MonadFunction1[R] extends Monad[R => ?] with Applicative1Function1[R] {
      override def point[A](a: A): R => A =
        (_: R) => a

      override def flatMap[A, B](fa: R => A)(f: A => (R => B)): R => B = { r: R =>
        val a: A = fa(r) // unwrap
        val fb: R => B = f(a) // apply
        val b: B = fb(r) // unwrap
        b
      } // rewrap
    }

    implicit def function1MonadInstance[R]: Monad[R => ?] = new MonadFunction1[R] {}
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
  //   log(doTransforms(0)) // (1, "0", true)


  ///////////////////////////////////////////////////////////////////////////////
  // State - where we're going, we don't need variables
  ///////////////////////////////////////////////////////////////////////////////

  final case class State[S, A](run: S => (S, A))

  object State {

    object Instances {

      import Applicative.Instances.ApplicativeState

      trait FunctorState[S] extends Functor[State[S, ?]] {
        def map[A, B](fa: State[S, A])(f: A => B): State[S, B] =
          State(s => {
            val (sp, a) = fa.run(s) // unwrap
            (sp, f(a)) // apply
          }) // rewrap
      }

      trait MonadState[S] extends Monad[State[S, ?]] with ApplicativeState[S] {
        override def point[A](a: A): State[S, A] =
          State(s => (s, a))

        override def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] =
          State(s => {
            val (sp, a) = fa.run(s) // unwrap
            val fb: State[S, B] = f(a) // apply
            fb.run(sp) // unwrap
          }) // rewrap
      }

      implicit def stateFunctorInstance[S]: Functor[State[S, ?]] = new FunctorState[S] {}

      implicit def stateMonadInstance[S]: Monad[State[S, ?]] = new MonadState[S] {}
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
  // log(computation.run(0)) // What's the final value?

  //val computationTake2 = State.put(5)
  //      .flatMap(_ => State.get())
  //      .map(x => x + 1)

  // log(computationTake2.run(0)) // What's the final value?

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
  ///////////////////////////////////////////////////////////////////////////////
  ///////////////////////////////////////////////////////////////////////////////

  ///////////////////////////////////////////////////////////////////////////////
  // Kleisli - I'm really sorry
  ///////////////////////////////////////////////////////////////////////////////

  final case class Kleisli[F[_] : Monad, A, B](run: A => F[B]) {
    def andThen[C](k: Kleisli[F, B, C]): Kleisli[F, A, C] =
      Kleisli(a => run(a).flatMap(k.run))
  }

  object Kleisli {

    object Instances {

      import Applicative.Syntax._
      import Monad.Syntax._

      trait FunctorKleisli[F[_], R] extends Functor[Kleisli[F, R, ?]] {
        implicit def F: Monad[F]

        def map[A, B](rfa: Kleisli[F, R, A])(f: A => B): Kleisli[F, R, B] = {
          Kleisli(rfa.run.map(fa => fa.map(f)))
        }
      }

      trait ApplicativeKleisli[F[_], R] extends Applicative[Kleisli[F, R, ?]] with FunctorKleisli[F, R] {
        implicit def F: Monad[F]

        override def pure[A](a: A): Kleisli[F, R, A] =
          Kleisli(a.pure[F].pure[R => ?])

        override def ap[A, B](rff: => Kleisli[F, R, A => B])(rfa: => Kleisli[F, R, A]): Kleisli[F, R, B] = {
          Kleisli(r => implicitly[Applicative[F]].ap(rff.run(r))(rfa.run(r)))
        }
      }


      trait MonadKleisli[F[_], R] extends Monad[Kleisli[F, R, ?]] with ApplicativeKleisli[F, R] {
        implicit def F: Monad[F]

        override def point[A](a: A): Kleisli[F, R, A] =
          Kleisli(a.point[F].point[R => ?])

        override def flatten[A](ffa: Kleisli[F, R, Kleisli[F, R, A]]): Kleisli[F, R, A] =
        //              unwrap                unwrap
        // rewrap
          Kleisli(r => ffa.run(r).flatMap(fa => fa.run(r)))
      }

      implicit def kleisliFunctorInstance[F[_] : Monad, R]: Functor[Kleisli[F, R, ?]] = new FunctorKleisli[F, R] {
        override def F: Monad[F] = implicitly[Monad[F]]
      }

      implicit def kleisliApplicativeInstance[F[_] : Monad, R]: Applicative[Kleisli[F, R, ?]] = new ApplicativeKleisli[F, R] with FunctorKleisli[F, R] {
        override def F: Monad[F] = implicitly[Monad[F]]
      }

      implicit def kleisliMonadInstance[F[_] : Monad, R]: Monad[Kleisli[F, R, ?]] = new MonadKleisli[F, R] {
        override def F: Monad[F] = implicitly[Monad[F]]
      }
    }

  }

  import Kleisli.Instances._

  def getMotherK: Kleisli[Option, FamilyMember, FamilyMember] =
    Kleisli(getMother)

  val getGreatGrandmotherAgeR: FamilyMember => Option[Int] =
    getMotherK.andThen(getMotherK).andThen(getMotherK).map(_.age).run

  //  log(getGreatGrandmotherAgeR(son)) // Some(103)

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

  //  log(postHandler.run(
  //    HttpRequest(
  //      "POST",
  //      Map("Authorisation" -> "Admin"),
  //      Some("Hello world")
  //  ))) // Some(HttpResponse("Hello world"))

  //  log(postHandler.run(
  //    HttpRequest(
  //      "POST",
  //      Map("Authorisation" -> "Not Admin"),
  //      Some("Hello world")
  //  ))) // None

  //  log(postHandler.run(
  //    HttpRequest(
  //      "GET",
  //      Map("Authorisation" -> "Admin"),
  //      Some("Hello world")
  //  ))) // None

  type ReaderT[F[_], A, B] = Kleisli[F, A, B]

  ///////////////////////////////////////////////////////////////////////////////

  val friendlyEcho: Kleisli[Sync, String, Unit] = for {
    _ <- Kleisli((name: String) => IO.Safe.putStrLn(s"Hello $name! Please enter something to be echoed:"))
    str <- Kleisli((_: String) => IO.Safe.getStrLn())
    _ <- Kleisli((name: String) => IO.Safe.putStrLn("Echoing: " + str + s". Have a nice day $name!"))
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

      implicit class SemigroupIdSyntax[M](val here: M) extends AnyVal {
        def <>(there: M)(implicit instance: Semigroup[M]): M = {
          instance.<>(here)(there)
        }
      }

    }

    object Instances {

      trait SemigroupList[A] extends Semigroup[List[A]] {
        override def <>(here: List[A])(there: List[A]): List[A] =
          here ++ there
      }

      trait SemigroupSum extends Semigroup[Int] {
        override def <>(here: Int)(there: Int): Int =
          here + there
      }

      trait SemigroupRightBiasMap[K, V] extends Semigroup[Map[K, V]] {
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

      implicit def listSemigroupInstance[A]: Semigroup[List[A]] = new SemigroupList[A] {}

      implicit def sumSemigroupInstance[A]: Semigroup[Int] = new SemigroupSum {}

      implicit def mapRightBiasSemigroupInstance[K, V]: Semigroup[Map[K, V]] = new SemigroupRightBiasMap[K, V] {}
    }

  }

  trait Monoid[A] extends Semigroup[A] {
    def mempty: A
  }

  object Monoid {

    object Instances {

      import Semigroup.Instances._

      trait MonoidList[A] extends Monoid[List[A]] with SemigroupList[A] {
        override def mempty: List[A] = List.empty
      }

      trait MonoidSum extends Monoid[Int] with SemigroupSum {
        override def mempty: Int = 0
      }

      trait MonoidRightBiasMap[K, V] extends Monoid[Map[K, V]] with SemigroupRightBiasMap[K, V] {
        override def mempty: Map[K, V] = Map.empty
      }

      implicit def listMonoidInstance[A]: Monoid[List[A]] = new MonoidList[A] {}

      implicit def sumMonoidInstance: Monoid[Int] = new MonoidSum {}

      implicit def mapRightBiasMonoidInstance[K, V]: Monoid[Map[K, V]] = new MonoidRightBiasMap[K, V] {}

    }

    import Semigroup.Syntax._

    def mempty[M](implicit instance: Monoid[M]): M = instance.mempty

    def mconcat[M: Monoid](xs: List[M]): M = {
      xs.foldRight(mempty)(_ <> _)
    }

    def mconcatMap[M: Monoid, G](xs: List[G])(f: G => M): M = {
      mconcat(xs.map(f))
    }
  }

  import Monoid.Instances._
  import Semigroup.Syntax._

  //  log(1 <> Monoid.mempty[Int]) // 1
  //  log(Monoid.mempty[Int] <> 1) // 1
  //  log(1 <> 2) // 3
  //
  //  log(Monoid.mconcat(List(1, 2, 3))) // 6
  //
  //  log(Map("hello" -> "world") <> Map("my name" -> "is Max")) // // Map(hello -> world, my name -> is Max)
  //
  //  log(Monoid.mconcat(
  //    List(
  //      Map("hello" -> "world"),
  //      Map("my name" -> "is Max"),
  //    ))
  //  ) // Map(hello -> world, my name -> is Max)

  ///////////////////////////////////////////////////////////////////////////////
  // The Free Monoid
  ///////////////////////////////////////////////////////////////////////////////

  sealed trait FileOp

  case class WriteFile(filename: String, contents: String) extends FileOp

  case class ReadFile(filename: String) extends FileOp

  case class DeleteFile(filename: String) extends FileOp

  object FileOp {
    def writeFile(filename: String, contents: String): List[FileOp] =
      List(WriteFile(filename, contents))

    def readFile(name: String): List[FileOp] =
      List(ReadFile(name))

    def deleteFile(filename: String): List[FileOp] =
      List(DeleteFile(filename))
  }


  import Semigroup.Instances._
  import Semigroup.Syntax._
  import Monoid.Instances._

  val program: List[FileOp] = {
    FileOp.writeFile("a.txt", "a") <>
      FileOp.deleteFile("b.txt") <>
      FileOp.writeFile("b.txt", "b")
  }

//  log(program)

  import java.io.PrintWriter

  def prodInterpreter(op: FileOp): List[Unit] = {
    op match {
      case WriteFile(filename, contents) =>
        IO.Unsafe.writeFile(filename, contents)
      case ReadFile(filename) =>
        log(IO.Unsafe.readFile(filename))
      case DeleteFile(filename) =>
        IO.Unsafe.deleteFile(filename)
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

  //  log(Monoid.mconcatMap(program)(prodInterpreter))
  //  log(Monoid.mconcatMap(program)(testInterpreter))

  // 🤔
  def appendFile(filename: String, contents: String): List[FileOp] = {
    val oldContents = FileOp.readFile(filename)
    FileOp.deleteFile(filename) <> FileOp.writeFile(filename, oldContents + contents)
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

  //  log(ll.map(_ + 1))

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
        case Free.Point(a) => a.point[F]
        case Free.Suspend(ga) => nt(ga)
        case Free.FlatMapped(fa, f) =>
          // HERE BE DRAGONS: This is not stack safe - you have to use a
          // "Trampoline" because we can't do TCO polymorphically.
          // DO NOT USE THIS IN PROD
          fa.foldMap[F](nt).flatMap(a => f(a).foldMap[F](nt))
      }
    }
  }

  object Free {

    final case class Point[G[_], A](a: A) extends Free[G, A]

    final case class Suspend[G[_], A](ga: G[A]) extends Free[G, A]

    final case class FlatMapped[G[_], A, B](fa: Free[G, A], f: A => Free[G, B]) extends Free[G, B]

    def suspend[G[_], A](ga: G[A]): Free[G, A] =
      Suspend(ga)

    object Instances {

      trait FunctorFree[G[_]] extends Functor[Free[G, ?]] {
        def map[A, B](fa: Free[G, A])(f: A => B): Free[G, B] =
          FlatMapped(fa, (a: A) => Point(f(a)))
      }

      import Applicative.Instances._

      trait MonadFree[G[_]] extends Monad[Free[G, ?]] with ApplicativeFree[G] {
        override def point[A](a: A): Free[G, A] =
          Point(a)

        override def flatMap[A, B](fa: Free[G, A])(f: A => Free[G, B]): Free[G, B] =
          FlatMapped(fa, f)
      }

      implicit def freeFunctorInstance[G[_]]: Functor[Free[G, ?]] = new FunctorFree[G] {}

      implicit def freeMonadInstance[G[_]]: Monad[Free[G, ?]] = new MonadFree[G] {}
    }

  }

  ///////////////////////////////////////////////////////////////////////////////
  // The Free Monad Usage
  ///////////////////////////////////////////////////////////////////////////////

  sealed trait FileOpM[A]

  object FileOpM {

    case class WriteFileM(filename: String, contents: String) extends FileOpM[Unit]

    case class ReadFileM(filename: String) extends FileOpM[String]

    case class DeleteFileM(filename: String) extends FileOpM[Unit]

    def writeFileM(filename: String, contents: String): Free[FileOpM, Unit] =
      Free.suspend(WriteFileM(filename, contents))

    def readFileM(filename: String): Free[FileOpM, String] =
      Free.suspend(ReadFileM(filename))

    def deleteFileM(filename: String): Free[FileOpM, Unit] =
      Free.suspend(DeleteFileM(filename))
  }


  import Free.Instances._

  def appendFileM(filename: String, contents: String): Free[FileOpM, Unit] = for {
    oldContents <- FileOpM.readFileM(filename)
    _ <- FileOpM.deleteFileM(filename)
    _ <- FileOpM.writeFileM(filename, oldContents + contents)
  } yield ()

  val programM = for {
    _ <- FileOpM.writeFileM("aM.txt", "aM")
    _ <- FileOpM.deleteFileM("aM.txt")
    _ <- FileOpM.writeFileM("bM.txt", "bM")
    _ <- appendFileM("bM.txt", "cM")
  } yield ()

//  log(programM)

  import FileOpM._

  def prodInterpreterM: FileOpM ~> Sync = new (FileOpM ~> Sync) {
    override def apply[A](fa: FileOpM[A]): Sync[A] =
      fa match {
        case WriteFileM(filename, contents) =>
          IO.Safe.writeFile(filename, contents)
        case ReadFileM(filename) =>
          IO.Safe.readFile(filename)
        case DeleteFileM(filename) =>
          IO.Safe.deleteFile(filename)
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

  val sync = programM.foldMap(prodInterpreterM).unsafeInterpret()
  val map = programM.foldMap(testInterpreterM).run(Map.empty)

//  log(map)

  ///////////////////////////////////////////////////////////////////////////////
  // Why is it called the Free Monad
  ///////////////////////////////////////////////////////////////////////////////

  // def mconcatMap[M: Monoid, G]  (xs: List[G])    (f: G => M):   M = {
  // def foldMap[F[_]: Monad, G, A](fa: Free[G, A])(nt: G ~> F): F[A] = {

  ///////////////////////////////////////////////////////////////////////////////
  ///////////////////////////////////////////////////////////////////////////////
  ///////////////////////////////////////////////////////////////////////////////

  // PRESENTER NOTE: Scroll to the Validated section

  ///////////////////////////////////////////////////////////////////////////////

  def catM(filenames: List[String]): Sync[String] = {
    ???
  }

  def catT(filenames: (String, String)): Sync[String] = {
    for {
      one <- IO.Safe.readFile(filenames._1)
      two <- IO.Safe.readFile(filenames._2)
    } yield one + two
  }

  // see sumAgeOfPersonAndParentsC

  ///////////////////////////////////////////////////////////////////////////////

  object MoreSyncInstances {

    import scala.concurrent._
    import scala.concurrent.duration._

    trait ApplicativeSync extends Applicative[Sync] with FunctorSync {
      override def pure[A](a: A): Sync[A] = Sync.suspend(a)

      // This is obviously an incredibly dumb way of implementing this,
      // but I hope you get the gist. We defer to the stdlib to give us a way of running two
      // computations at once, and once both have resolved, finally running our function
      override def liftA2[A, B, C](f: A => B => C): Sync[A] => Sync[B] => Sync[C] =
        (fa: Sync[A]) => (fb: Sync[B]) => Sync.suspend {
          implicit val context = scala.concurrent.ExecutionContext.global

          val pa: Future[A] = Future {
            fa.unsafeInterpret()
          }

          val pb: Future[B] = Future {
            fb.unsafeInterpret()
          }

          val a = Await.result(pa, 5.seconds)
          val b = Await.result(pb, 5.seconds)

          f(a)(b)
        }
    }

    implicit def syncApplicativeInstance: Applicative[Sync] = new ApplicativeSync {}
  }

  ///////////////////////////////////////////////////////////////////////////////

  import MoreSyncInstances._

  def catA(filenames: (String, String)): Sync[String] = {
    val one: Sync[String] = IO.Safe.readFile(filenames._1)
    val two: Sync[String] = IO.Safe.readFile(filenames._2)

    val join = (x: String, y: String) => x + y

    Applicative.liftA2(join.curried)(one)(two)
  }

  ///////////////////////////////////////////////////////////////////////////////

  trait Applicative[F[_]] extends Functor[F] {
    self: Functor[F] =>
    def pure[A](a: A): F[A]

    // can define one or the other
    def ap[A, B](ff: => F[A => B])(fa: => F[A]): F[B] = {
      val apply =
        (f: A => B) => (a: A) => f(a)

      liftA2(apply)(ff)(fa)
    }

    // you might want to define `liftA2` because mapping over your `F` might
    // be an expensive operation
    def liftA2[A, B, C](f: A => B => C): F[A] => F[B] => F[C] =
      (fa: F[A]) => (fb: F[B]) => ap(fa.map(f)(self))(fb)
  }

  object Applicative {
    def liftA2[F[_], A, B, C](f: A => B => C)(fa: F[A])(fb: F[B])(implicit instance: Applicative[F]): F[C] =
      instance.liftA2(f)(fa)(fb)

    object Syntax {

      implicit class ApplicativeIdExtensions[A](private val self: A) extends AnyVal {
        def pure[F[_]](implicit instance: Applicative[F]): F[A] = {
          instance.pure(self)
        }
      }

      implicit class ApplicativeExtensions[F[_], A, B](private val self: F[A => B]) extends AnyVal {
        def ap(fa: => F[A])(implicit instance: Applicative[F]): F[B] = {
          instance.ap(self)(fa)
        }
      }

    }

    object Instances {

      trait ApplicativeOption extends Applicative[Option] with FunctorOption {
        override def pure[A](a: A): Option[A] =
          Some(a)

        override def ap[A, B](ff: => Option[A => B])(fa: => Option[A]): Option[B] =
          (ff, fa) match {
            case (Some(f), Some(a)) => Some(f(a))
            case _ => None
          }
      }

      trait ApplicativeList extends Applicative[List] with FunctorList {
        override def pure[A](a: A): List[A] =
          List(a)

        override def ap[A, B](ff: => List[A => B])(fa: => List[A]): List[B] =
          for {
            f <- ff
            a <- fa
          } yield f(a)
      }

      trait ApplicativeState[S] extends Applicative[State[S, ?]] with FunctorState[S] {
        override def pure[A](a: A): State[S, A] =
          State(s => (s, a))

        override def ap[A, B](ff: => State[S, A => B])(fa: => State[S, A]): State[S, B] =
          State(s => {
            val (sp, f) = ff.run(s) // unwrap
            val (spp, a) = fa.run(sp) // unwrap
            (spp, f(a)) // rewrap
          })
      }

      trait Applicative1Function1[R] extends Applicative[R => ?] with FunctorFunction1[R] {
        override def pure[A](a: A): R => A =
          (_: R) => a


        override def ap[A, B](ff: => R => A => B)(fa: => R => A): R => B = { r: R =>
          val f: A => B = ff(r) // unwrap
          val a: A = fa(r) // unwrap
          val b: B = f(a) // apply
          b
        }
      }

      trait ApplicativeFree[G[_]] extends Applicative[Free[G, ?]] with FunctorFree[G] {
        override def pure[A](a: A): Free[G, A] = Free.Point(a)

        // this is a lot of overhead just to Applicative operations on Free.
        // maybe there's some other sort of structure that can help us here...
        override def ap[A, B](ff: => Free[G, A => B])(fa: => Free[G, A]): Free[G, B] =
          Free.FlatMapped(ff, (f: A => B) =>
            Free.FlatMapped(fa, (a: A) =>
              Free.Point(f(a))
            )
          )
      }

      implicit val optionApplicativeInstance: Applicative[Option] = new ApplicativeOption {}
      implicit val listApplicativeInstance: Applicative[List] = new ApplicativeList {}

      implicit def stateApplicativeInstance[S]: Applicative[State[S, ?]] = new ApplicativeState[S] {}

      implicit def function1ApplicativeInstance[R]: Applicative[R => ?] = new Applicative1Function1[R] {}

      implicit def freeApplicativeInstance[G[_]]: Applicative[Free[G, ?]] = new ApplicativeFree[G] {}

    }

  }

  ///////////////////////////////////////////////////////////////////////////////

  sealed trait Validated[+E, +A]

  case class Valid[E, A](value: A) extends Validated[E, A]

  case class Invalid[E, A](error: E) extends Validated[E, A]

  // HIDE ME
  object Validated {

    object Instances {

      trait FunctorValidated[E] extends Functor[Validated[E, ?]] {
        override def map[A, B](fa: Validated[E, A])(f: A => B): Validated[E, B] = {
          fa match {
            case Valid(v) => Valid(f(v))
            case Invalid(e) => Invalid(e)
          }
        }
      }

      //      trait MonadValidated[E] extends Monad[Validated[E, ?]]  {
      //        override def point[A](a: A): Validated[E, A] =
      //          Valid(a)
      //
      //        override def flatMap[A, B](fa: Validated[E, A])(f: A => Validated[E, B]): Validated[E, B] = {
      //          fa match {
      //            case Valid(v) => f(v)
      //            case Invalid(e) => Invalid(e) // [sic]
      //          }
      //        }
      //      }

      implicit def functorValidated[E]: Functor[Validated[E, ?]] = new FunctorValidated[E] {}

      // HIDE ME
      implicit def applicativeValidated[E: Semigroup]: Applicative[Validated[E, ?]] = new Applicative[Validated[E, ?]] with FunctorValidated[E] {
        override def pure[A](a: A): Validated[E, A] = Valid(a)

        override def ap[A, B](ff: => Validated[E, A => B])(fa: => Validated[E, A]): Validated[E, B] =
          (ff, fa) match {
            case (Valid(f), Valid(v)) => Valid(f(v))
            case (Invalid(e), Valid(_)) => Invalid(e)
            case (Valid(_), Invalid(e)) => Invalid(e)
            case (Invalid(e), Invalid(ee)) => Invalid(e <> ee)
          }
      }
    }

  }

  object CredentialValidator {
    type Error = String

    import Validated.Instances._

    def validateEmail(email: String): Validated[List[Error], String] = {
      if (email.contains("@")) {
        Valid(email)
      } else {
        Invalid(List(s"Username must be an email"))
      }
    }

    def validatePassword(password: String): Validated[List[Error], String] = {
      if (password.length < 8) {
        Valid(password)
      } else {
        Invalid(List(s"Password was shorter than 8 characters"))
      }
    }

    sealed abstract case class UserCredentials(username: String, password: String) {
    }

    object UserCredentials {
      //      def apply(username: String, password: String): Validated[List[String], UserCredentials] =
      //        for {
      //          username <- validateEmail(username)
      //          password <- validatePassword(password)
      //        } yield new UserCredentials(username = username, password = password) {}

      // HIDE ME
      def apply(username: String, password: String): Validated[List[String], UserCredentials] = {
        val mkUserCreds = (username: String, password: String) =>
          new UserCredentials(username = username, password = password) {}

        Applicative.liftA2(mkUserCreds.curried)(validateEmail(username))(validatePassword(password))
      }
    }

    def signup[E](credentials: Valid[E, UserCredentials]): Unit = {
      log(credentials)
    }

    CredentialValidator.UserCredentials("Max Bo", "hunter2") match {
      case v@Valid(_) => signup(v)
      //    case i@Invalid(_) => signup(i)
      case Invalid(error) => log(error)
    }
  }

  ///////////////////////////////////////////////////////////////////////////////

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
    val motherAge = member.mother.map(_.age)
    val fatherAge = member.father.map(_.age)
    val add = (x: Int, y: Int) => {
      x + y
    }

    motherAge.map(add.curried).ap(fatherAge)
    // OR
    Applicative.liftA2(add.curried)(motherAge)(fatherAge)
  }

  //  log(sumAgeOfPersonAndParentsA(son)) // Some(133)
  //  log(sumAgeOfPersonAndParentsA(mum)) // None

  ///////////////////////////////////////////////////////////////////////////////
  // After that "brief" interlude, back to running things in parallel
  ///////////////////////////////////////////////////////////////////////////////

  import MoreSyncInstances._

  // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/all
  def sequenceSyncList[A](xs: List[Sync[A]]): Sync[List[A]] = {

    val collector: Sync[List[A]] = List.empty[A].pure[Sync]

    def prepend: A => List[A] => List[A] = (x: A) => (xs: List[A]) =>
      xs.prepended(x)

    def lifted: (Sync[A], Sync[List[A]]) => Sync[List[A]] =
      Function.uncurried(Applicative.liftA2(prepend))

    xs.foldRight[Sync[List[A]]](collector)(lifted)
  }

  sequenceSyncList(List(
    "README.md",
    "LICENSE"
  ).map(IO.Safe.readFile)).unsafeInterpret()

  ///////////////////////////////////////////////////////////////////////////////

  implicit class ListExtensions[F[_] : Applicative, A](xs: List[F[A]]) {
    def sequenceAL(): F[List[A]] = {
      val collector: F[List[A]] = List.empty[A].pure[F]

      def prepend: A => List[A] => List[A] = (x: A) => (xs: List[A]) =>
        xs.prepended(x)

      def lifted: (F[A], F[List[A]]) => F[List[A]] =
        Function.uncurried(Applicative.liftA2(prepend))

      xs.foldRight[F[List[A]]](collector)(lifted)
    }
  }

  ///////////////////////////////////////////////////////////////////////////////

  val metadataSequence: Sync[List[String]] = List(
    "README.md",
    "LICENSE"
  ).map(IO.Safe.readFile).sequenceAL()

  // transforms a List[Sync[String]] into Sync[List[String]]
//  metadataSequence.unsafeInterpret()

  ///////////////////////////////////////////////////////////////////////////////

  val stateSequence: State[Int, List[Unit]] = List(
    State.put[Int](0),
    State.modify[Int](_ + 1)
  ).sequenceAL()

  // transforms a List[State[Int, Unit]] into State[Int, List[Unit]]
//  log(stateSequence.run(0))

  ///////////////////////////////////////////////////////////////////////////////

  val someOptionSequence: Option[List[Int]] = List(
    Some(5).asInstanceOf[Option[Int]], // need to widen the type to make the syntax work
    Some(6),
    Some(7)
  ).sequenceAL()

  val noneOptionSequence: Option[List[Int]] = List(
    Some(5),
    None,
    Some(7)
  ).sequenceAL()

  ///////////////////////////////////////////////////////////////////////////////

  sealed trait FreeAp[G[_], A] {
    self: FreeAp[G, A] =>
    def foldMap[F[_] : Applicative](nt: G ~> F): F[A] = {
      self match {
        case FreeAp.Pure(a) => a.pure[F]
        case FreeAp.Suspend(ga) => nt(ga)
        case FreeAp.Ap(ff, fa) =>
          // HERE BE DRAGONS: This is not stack safe - you have to use a
          // "Trampoline" because we can't do TCO polymorphically.
          // DO NOT USE THIS IN PROD
          ff.foldMap[F](nt).ap(fa.foldMap[F](nt))
      }
    }
  }

  object FreeAp {

    final case class Pure[G[_], A](a: A) extends FreeAp[G, A]

    final case class Suspend[G[_], A](ga: G[A]) extends FreeAp[G, A]

    final case class Ap[G[_], A, B](ff: FreeAp[G, A => B], fa: FreeAp[G, A]) extends FreeAp[G, B]

    def suspend[G[_], A](ga: G[A]): FreeAp[G, A] =
      Suspend(ga)

    object Instances {

      trait FunctorFreeAp[G[_]] extends Functor[FreeAp[G, ?]] {
        def map[A, B](fa: FreeAp[G, A])(f: A => B): FreeAp[G, B] = {
          val ff = Pure[G, A => B](f)
          Ap[G, A, B](ff, fa)
        }
      }

      trait ApplicativeFreeAp[G[_]] extends Applicative[FreeAp[G, ?]] with FunctorFreeAp[G] {
        override def pure[A](a: A): FreeAp[G, A] =
          Pure(a)

        override def ap[A, B](ff: => FreeAp[G, A => B])(fa: => FreeAp[G, A]): FreeAp[G, B] =
          Ap(ff, fa)
      }

      implicit def freeApFunctorInstance[G[_]]: Functor[FreeAp[G, ?]] = new FunctorFreeAp[G] {}

      implicit def freeApApplicativeInstance[G[_]]: Applicative[FreeAp[G, ?]] = new ApplicativeFreeAp[G] {}
    }

  }



  ///////////////////////////////////////////////////////////////////////////////
  // WIP SECTION
  ///////////////////////////////////////////////////////////////////////////////

  object TransformerHelpers {

    import Applicative.Syntax._

    def map2[F[_]: Functor, G[_]: Functor, A, B](fga: F[G[A]])(f: A => B): F[G[B]] =
      fga.map(_.map(f))

    def pure2[F[_]: Applicative, G[_]: Applicative, A](a: A): F[G[A]] = {
      a.pure[G].pure[F]
    }

    def ap2[F[_]: Applicative, G[_]: Applicative, A, B](fgf: => F[G[A => B]])(fga: => F[G[A]]): F[G[B]] = {
      //      val ap: (=> G[A => B]) => (=> G[A]) => G[B] =
      ???
    }

    def point2[F[_]: Monad, G[_]: Monad, A](a: A): F[G[A]] = {
      a.point[G].point[F]
    }

    // YOU CAN'T WRITE THIS METHOD!
    //    def flatMap2[F[_]: Monad, G[_]: Monad, A, B](fga: => F[G[A]])(f: A => F[G[B]]): F[G[B]] = {
    //      fga.flatMap(_.flatMap(???))
    //    }
  }

  final case class OptionT[F[_] : Monad, A](run: F[Option[A]])

  object OptionT {
    object Instances {

      trait FunctorOptionT[F[_]] extends Functor[OptionT[F, ?]] {
        implicit def F: Monad[F]

        override def map[A, B](fa: OptionT[F, A])(f: A => B): OptionT[F, B] =
          OptionT(TransformerHelpers.map2(fa.run)(f))
      }

      trait ApplicativeOptionT[F[_]] extends Applicative[OptionT[F, ?]] with FunctorOptionT[F] {
        implicit def F: Monad[F]

        override def pure[A](a: A): OptionT[F, A] =
          OptionT(TransformerHelpers.pure2[F, Option, A](a))

        override def ap[A, B](ff: => OptionT[F, A => B])(fa: => OptionT[F, A]): OptionT[F, B] =
        // TODO: replace with ap2
          ???
      }

      trait MonadOptionT[F[_]] extends Monad[OptionT[F, ?]] with ApplicativeOptionT[F] {
        implicit def F: Monad[F]

        override def point[A](a: A): OptionT[F, A] =
          OptionT(TransformerHelpers.point2[F, Option, A](a))

        override def flatMap[A, B](fa: OptionT[F, A])(f: A => OptionT[F, B]): OptionT[F, B] =
          OptionT(fa.run.flatMap {
            case Some(a) => f(a).run
            case None => None.asInstanceOf[Option[B]].point[F]
          })
      }

      //      def optionTFunctorInstance[F[_]: Monad]: Functor[OptionT[F, ?]] = new FunctorOptionT[F] {
      //        override implicit def F: Monad[F] = implicitly[Monad[F]]
      //      }
      //
      //      def optionTApplicativeInstance[F[_]: Monad]: Applicative[OptionT[F, ?]] = new ApplicativeOptionT[F] {
      //        override implicit def F: Monad[F] = implicitly[Monad[F]]
      //      }
      //
      //      def optionTMonadInstance[F[_]: Monad]: Monad[OptionT[F, ?]] = new MonadOptionT[F] {
      //        override implicit def F: Monad[F] = implicitly[Monad[F]]
      //      }
    }
  }

  // Seen in a previous episode
  //  def sumAgeOfPersonAndParentsC(member: FamilyMember): Option[Int] = {
  //    for {
  //      mother <- member.mother
  //      father <- member.father
  //    } yield member.age + mother.age + father.age
  //  }

  type UUID = String

  case class RemoteFamilyMember(
    id: UUID,
    age: Int,
    mother: Option[UUID] = None,
    father: Option[UUID] = None
  )

  case class FamilyMemberServiceStub(db: Map[String, RemoteFamilyMember]) {
    def get(key: String): Sync[Option[RemoteFamilyMember]] =
      Sync.suspend(db.get(key))
  }

  val service = FamilyMemberServiceStub(
    Map(
      "0" -> RemoteFamilyMember("0", 22, mother = Some("1")),
      "1" -> RemoteFamilyMember("1", 51),
      "2" -> RemoteFamilyMember("2", 54)
    )
  )

  // Ain't gonna work!
  //    def sumAgeOfPersonAndParentsT(service: FamilyMemberServiceStub, id: UUID): Sync[Option[Int]] = {
  //      for {
  //        member <- service.get(id)
  //        mother <- service.get(member.mother)
  //        father <- service.get(member.father)
  //      } yield member.age + mother.age + father.age
  //    }

//    def sumAgeOfPersonAndParentsT(service: FamilyMemberServiceStub, id: UUID): Sync[Option[Int]] = {
//      for {
//        member <- OptionT(service.get(id))
//        mother <- OptionT(member.mother.traverse(service.get))
//        father <- OptionT(member.father.traverse(service.get))
//      } yield member.age + mother.age + father.age
//    }

  ///////////////////////////////////////////////////////////////////////////////
}
