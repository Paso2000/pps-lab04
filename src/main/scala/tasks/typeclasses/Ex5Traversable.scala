package u04lab
import u03.Sequences.*
import Sequence.*
import u03.extensionmethods.Optionals.Optional
import u03.extensionmethods.Optionals.Optional.{Just, None}
import u04.typeclasses.HigherKindedTypesWithExtensionMethods.Filterable

/*  Exercise 5: 
 *  - Generalise by ad-hoc polymorphism logAll, such that:
 *  -- it can be called on Sequences but also on Optional, or others... 
 *  -- it does not necessarily call log, but any function with analogous type
 *  - Hint: introduce a type class Traversable[T[_]]], capturing the ability of calling a
 *    "consumer function" on all elements (with type A) of a datastructure T[A] 
 *    Note Traversable is a 2-kinded trait (similar to Filterable, or Monad)
 *  - Write givens for Traversable[Optional] and Traversable[Sequence]
 *  - Show you can use the generalisation of logAll to:
 *  -- log all elements of an Optional, or of a Traversable
 *  -- println(_) all elements of an Optional, or of a Traversable
 */

object Ex5Traversable:

  trait Traversable[T[_]]:
    def log[A](t: T[A]): Unit
  
  private def logAll[T[_]: Traversable,A](traversable: T[A]): Unit =
    val loggable = summon[Traversable[T]]
    loggable.log(traversable)

  given Traversable[Sequence] with
    def log[A](sequence: Sequence[A]): Unit = sequence match
      case Nil() =>
      case Cons(h,t) => print(h); log(t)


  given Traversable[Optional] with
    def log[A](optional: Optional[A]):Unit = optional match
      case Optional.None() =>
      case Optional.Just(a) => print(a)


  @main def tryLoggable(): Unit =

    val si = Cons(10, Cons(20, Cons(30, Nil())))
    println:
      logAll(si) // 60

    logAll(Nil())

    val sd = Optional.Just(5)
    println:
      logAll(sd) // 60.0






  
