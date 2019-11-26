import Part1._
import Part1.Functor.Instances._
import Part1.Functor.Syntax._
import Part1.Applicative.Instances._
import Part1.Applicative.Syntax._
import Part1.Monad.Instances._
import Part1.Monad.Syntax._

import pprint.log

object Part2 extends App {

  /**
   * A monoid on applicative functors.
   */
  trait Alternative[F[_]] { // should extend Applicative, but we don't because of implicit clashes
    def empty[A]: F[A]
    def <|>[A](pa: => F[A])(qa: => F[A]): F[A]
  }

  object Alternative {
    import Syntax._

    def empty[F[_]: Alternative, A]: F[A] =
      implicitly[Alternative[F]].empty[A]

    /**
     * `many` takes a single function argument and repeatedly applies it until
     * the function fails and then yields the collected results up to that
     * point.
     */
    def many[F[_]: Alternative: Applicative, A](v: => F[A]): F[List[A]] = {
      some(v) <|> List.empty[A].pure[F]
    }

    /**
     * The `some` function behaves similar except that it will fail itself if
     */
    def some[F[_]: Alternative: Applicative, A](v: => F[A]): F[List[A]] = {
      def prepend: A => List[A] => List[A] = (x: A) => (xs: List[A]) =>
        xs.prepended(x)

      lazy val m: F[List[A]] = many(v)

      v.map(prepend).ap(m)
    }

    object Syntax {
      implicit class AlternativeIdSyntax[F[_], A](val here: F[A]) extends AnyVal {
        def <|>(there: => F[A])(implicit instance: Alternative[F]): F[A] = {
          instance.<|>(here)(there)
        }
      }
    }
  }

  import Alternative.Syntax._

  // Lifted from http://dev.stephendiehl.com/fun/002_parsers.html
  /**
   * Structurally a parser is a function which takes an input stream of
   * characters and yields a parse tree by applying the parser logic over
   * sections of the character stream (called lexemes) to build up a
   * composite data structure for the AST.
   */
  case class Parser[A](parse: String => List[(A, String)]) {

    /**
     * Running the function will result in traversing the stream of characters
     * yielding a value of type `A` that usually represents the AST for the
     * parsed expression, or failing with a parse error for malformed input,
     * or failing by not consuming the entire stream of input. A more
     * robust implementation would track the position information of failures
     * for error reporting.
     */
    def run(stream: String): Either[String, A] = {
      parse(stream) match {
        case List((res, "")) =>
          Right(res)
        case List((_, rs)) =>
          Left(s"Parser did not consume entire stream. Remaining: $rs")
        case d =>
          Left(s"Parser error. Conflict: $d")
      }
    }
  }

  ///////////////////////////////////////////////////////////////////////////////

  object Parser {

    /**
     * We advance the parser by extracting a single character from the parser
     * stream and returning in a tuple containing itself and the rest of the
     * stream. The parser logic will then scrutinize the character and either
     * transform it in some portion of the output or advance the stream and
     * proceed.
     */
    def item: Parser[Char] = Parser {
      case "" => List.empty
      case xs => List((xs.head, xs.tail))
    }

    object Instances {

      trait FunctorParser extends Functor[Parser] {
        override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] =
          Parser { s =>
            for {
              (a, rs) <- fa.parse(s)
            } yield (f(a), rs)
          }
      }

      trait ApplicativeParser extends Applicative[Parser] with FunctorParser {
        override def pure[A](a: A): Parser[A] =
          Parser { s => List((a, s)) }

        override def ap[A, B](ff: => Parser[A => B])(fa: => Parser[A]): Parser[B] =
          Parser { s =>
            for {
              (f, s1) <- ff.parse(s) // consume some of the stream
              (a, s2) <- fa.parse(s1) // consume some more of the stream
            } yield (f(a), s2)
          }
      }

      trait MonadParser extends Monad[Parser] with ApplicativeParser {
        /**
         * The unit operation injects a single pure value as the result,
         * without reading from the parse stream.
         */
        override def point[A](a: A): Parser[A] =
          Parser { s => List((a, s)) }

        /**
         * A bind operation for our parser type will take one parse operation
         * and compose it over the result of second parse function. Since the
         * parser operation yields a list of tuples, composing a second parser
         * function simply maps itself over the resulting list and concat's the
         * resulting nested list of lists into a single flat list in the usual
         * list monad fashion. The unit operation injects a single pure value as
         * the result, without reading from the parse stream.
         */
        override def flatMap[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B] =
          Parser { s =>
            fa.parse(s).flatMap { case (a: A, s1: String) =>
              val fb: List[(B, String)] = f(a).parse(s1)
              fb
            }
          }
      }

      implicit val parserFunctorInstance: Functor[Parser] = new FunctorParser {}
      implicit val parserApplicativeInstance: Applicative[Parser] = new ApplicativeParser {}
      implicit val parserMonadInstance: Monad[Parser] = new MonadParser {}
    }

    /**
     * Of particular importance is that this particular monad has
     * a zero value (`failure`), namely the function which halts reading the
     * stream and returns the empty stream.
     */
    def failure[A]: Parser[A] =
      Parser { cs => List.empty }

    /**
     * Together this forms a monoidal structure with a secondary operation
     * (`combine`) which applies two parser functions over the same stream and
     * concatenates the result.
     */
    def combine[A](pa: Parser[A], qa: Parser[A]): Parser[A] =
      Parser { s => pa.parse(s) ++ qa.parse(s) }

    def option[A](pa: Parser[A], qa: => Parser[A]): Parser[A] =
      Parser { s =>
        pa.parse(s) match {
          case Nil => qa.parse(s)
          case res => res
        }
      }

    object MoreInstances {
      import Instances._

      /**
       * This gives rise to the Alternative class instance which encodes the
       * logic for trying multiple parse functions over the same stream and
       * handling failure and rollover.
       */
      trait AlternativeParser extends Alternative[Parser] {
        override def empty[A]: Parser[A] =
          failure[A]

        override def <|>[A](pa: => Parser[A])(qa: => Parser[A]): Parser[A] =
          option(pa, qa)
      }

      implicit val parserAlternativeInstance: Alternative[Parser] = new AlternativeParser {}
    }

    import Instances._
    import MoreInstances._

    /**
     * On top of this we can add functionality for checking whether the current
     * character in the stream matches a given predicate ( i.e is it a digit,
     * is it a letter, a specific word, etc).
     */
    def satisfy(p: Char => Boolean): Parser[Char] =
      item.flatMap { c =>
        if (p(c)) {
          c.pure[Parser]
        } else {
          Parser.failure
        }
      }

    /**
     * Essentially this 50 lines code encodes the entire core of the parser
     * combinator machinery. All higher order behavior can be written on top of
     * just this logic. Now we can write down several higher level functions
     * which operate over sections of the stream.
     */
    def oneOf(s: List[Char]): Parser[Char] =
      satisfy(s.contains)

    def chainl[A](p: Parser[A])(op: Parser[A => A => A])(a: A): Parser[A] = {
      chainl1(p)(op) <|> a.pure[Parser]
    }

    /**
     * `chainl1` parses one or more occurrences of p, separated by op and
     * returns a value obtained by a recursing until failure on the left hand
     * side of the stream. This can be used to parse left-recursive grammar.
     */
    def chainl1[A](p: Parser[A])(op: Parser[A => A => A]): Parser[A] = {
      // If you think I understand how this works, you'd be sorely mistaken
      def rest(a: A): Parser[A] = (for {
        f <- op
        b <- p
        res <- rest(f(a)(b))
      } yield res) <|> a.pure[Parser]

      for {
        a <- p
        res <- rest(a)
      } yield res
    }

    /**
     * Using satisfy we can write down several combinators for detecting the
     * presence of specific common patterns of characters (numbers,
     * parenthesized expressions, whitespace, etc).
     */

    def char(c: Char): Parser[Char] =
      satisfy(_ == c)

    def spaces: Parser[String] =
      Alternative.many(oneOf(List('\n', '\r'))).map(_.mkString)

    def string(ccs: String): Parser[String] =
      ccs match {
        case "" => "".pure[Parser]
        case cs => for {
          _ <- char(cs.head)
          _ <- string(cs.tail)
        } yield cs
      }

    def token[A](p: Parser[A]): Parser[A] = for {
      a <- p
      _ <- spaces
    } yield a

    def reserved(s: String): Parser[String] =
      token(string(s))


    def digit: Parser[Char] =
      satisfy(_.isDigit)

    def natural: Parser[Int] =
      Alternative.some(digit).map(_.mkString.toInt)

    def number: Parser[Int] = for {
      s <- string("-") <|> string("")
      cs <- Alternative.some(digit)
    } yield (s + cs.mkString).toInt

    def parens[A](m: Parser[A]): Parser[A] = for {
      _ <- reserved("(")
      n <- m
      _ <- reserved(")")
    } yield n
  }

  /**
   * And that's about it! In a few hundred lines we have enough of a parser
   * library to write down a simple parser for a calculator grammar. In the
   * formal Backus–Naur Form our grammar would be written as:
   *
   * number = [ "-" ] digit { digit }.
   * digit  = "0" | "1" | ... | "8" | "9".
   * expr   = term { addop term }.
   * term   = factor { mulop factor }.
   * factor = "(" expr ")" | number.
   * addop  = "+" | "-".
   * mulop  = "*".
   */

  /**
   * The direct translation to Scala in terms of our newly constructed parser
   * combinator has the following form:
   */
  object Calculator {

    import Parser._
    import Parser.Instances._
    import Parser.MoreInstances._
    import Alternative.Syntax._

    sealed trait Expr

    case class Add(a: Expr, b: Expr) extends Expr

    case class Mul(a: Expr, b: Expr) extends Expr

    case class Sub(a: Expr, b: Expr) extends Expr

    case class Lit(n: Int) extends Expr

    def eval(ex: Expr): Int = ex match {
      case Add(a, b) => eval(a) + eval(b)
      case Mul(a, b) => eval(a) * eval(b)
      case Sub(a, b) => eval(a) - eval(b)
      case Lit(n) => n
    }

    def int: Parser[Expr] = for {
      n <- number
    } yield Lit(n)

    def expr: Parser[Expr] =
      chainl1(term)(addop)

    def term: Parser[Expr] =
      chainl1(factor)(mulop)

    def factor: Parser[Expr] =
      int <|> parens(expr)

    def infixOp[A](x: String, f: A => A => A): Parser[A => A => A] = for {
      _ <- reserved(x)
    } yield f

    def addop: Parser[Expr => Expr => Expr] = {
      val add: Expr => Expr => Expr = (Add(_, _)).curried
      val sub: Expr => Expr => Expr = (Sub(_, _)).curried

      infixOp("+", add) <|> infixOp("-", sub)
    }

    def mulop: Parser[Expr => Expr => Expr] =
      infixOp("*", (Mul(_, _)).curried)

    def apply(s: String): Either[String, Int] =
      expr.run(s).map(eval)
  }

  /**
   * Now we can try out our little parser.
   */
  log(Calculator("1+1")) // 2
  log(Calculator("(2*(1+2)*(3-(-4+5)))")) // 12
}
