import Step0.Functor
import Step0.Functor.Instances._
import Step0.Functor.Syntax._
import Step0.Function1FunctorInstances._
import Step0.Function1MonadInstances._
import pprint.log

object Step4 {
  // Now we can talk about the Profunctor part of "Profunctor Optics"
  // We all hopefully know what a functor is, it's something we can write a lawful implementation of map for

  /**
  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }
  **/
  log(implicitly[Functor[List]].map(List(1, 2, 3))(_ + 1))

  // A functor is something we can `map` over. We commonly use this for things like List, Option, and Either
  // One thing those things have in common is that they're containers

  // That's an accurate intuition, but not a complete one!
  // For spotting Functors in the wild, without learning about variance and how to calculate positive and
  //  negative positional arguments, just think about whether a type parameter is a source of values, or
  //  a sink of values



  trait Contravariant[F[_]] {
    def contramap[A, B](fa: F[A])(f: B => A): F[B]
  }

  // Note the difference here, it's in the direction of the arrow for parameter `f` to our {contra}map function.
  //       map[A, B](fa: F[A])(f: A => B): F[B]
  // contramap[A, B](fa: F[A])(f: B => A): F[B]



  object Contravariant {
    object Syntax {

      implicit class ContravariantExtensions[F[_], A](private val self: F[A]) extends AnyVal {
        def contramap[B](f: B => A)(implicit instance: Contravariant[F]): F[B] = {
          instance.contramap(self)(f)
        }
      }
    }

    object Instances {
      // compare this to FunctorFunction1
      trait ContravariantFunction1[R] extends Contravariant[? => R] {
        def contramap[A, B](fa: A => R)(f: B => A): B => R =
          f.andThen(fa)
      }

      implicit def function1ContravariantFunctorInstance[R]: Contravariant[? => R] = new ContravariantFunction1[R] {}
    }
  }

  import Contravariant.Instances._
  import Contravariant.Syntax._

  def uppercaseStep: String => String = _.toUpperCase

  log(s"Feed the pipeline a value: ${uppercaseStep("Mikey")}")

  // Proof we can map over a Function, modifying items on the way out
  val outputLength = uppercaseStep.map(_.length)

  implicitly[Functor[String => ?]]

  log(s"Map the output side of the pipeline: ${outputLength.apply("Mikey")}")

  implicitly[Contravariant[? => String]]

  // Proof we can contramap over a Function, modifying items on the way in
  val doubleInputs = uppercaseStep.contramap((s: String) => s ++ s)
  log(s"Map the input side of the pipeline: ${doubleInputs.apply("Mikey")}")

  // A profunctor is just any type that has BOTH a functor instance, and a contravariant instance
  // That's all there is to it!
  /**
  trait Profunctor[=>:[_, _]]  { self =>
  /** Contramap on `A`. */
  def mapfst[A, B, C](fab: (A =>: B))(f: C => A): (C =>: B)

  /** Functor map on `B`. */
  def mapsnd[A, B, C](fab: (A =>: B))(f: B => C): (A =>: C)

  /** Functor map on `A` and `B`. */
  def dimap[A, B, C, D](fab: (A =>: B))(f: C => A)(g: B => D): (C =>: D) = mapsnd(mapfst(fab)(f))(g)
  */

//  val doBoth = uppercaseStep.dimap((s: String) => s ++ s, _.reverse)
//  log(s"\n\nMap both sides at the same time using the Profunctor instance: ${doBoth.apply("Mikey")}")

  // This is one of the examples where the intuition of Functors being containers is incorrect.
}
