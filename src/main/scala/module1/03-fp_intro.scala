package module1

import module1.list.List.{Cons, Nil}

object opt {

  sealed trait Option[+T] {
    def isEmpty: Boolean = this match {
      case Option.Some(v) => false
      case Option.None => true
    }

    def get: T = this match {
      case Option.Some(v) => v
      case Option.None => throw new Exception("Get on empty Option")
    }

    def getOrElse[TT >: T](b: TT): TT = this match {
      case Option.Some(v) => v
      case Option.None => b
    }

    def map[B](f: T => B): Option[B] = this match {
      case Option.Some(v) => Option.Some(f(v))
      case Option.None => Option.None
    }

    def flatMap[B](f: T => Option[B]): Option[B] = this match {
      case Option.Some(v) => f(v)
      case Option.None => Option.None
    }

    /**
     * Реализовать метод printIfAny, который будет печатать значение, если оно есть
     */

    def printIfAny: Unit = this match {
      case Option.Some(v) => println(v)
      case _ => None
    }

    /**
     *
     * Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option
     */
    def zip[B](that: Option[B]): Option[(T, B)] = this match {
      case Option.None => Option.None
      case Option.Some(v) => that match {
        case Option.None => Option.None
        case Option.Some(v1) => Option.Some(v, v1)
      }
    }

    /*
final def zip[A1 >: A, B](that: Option[B]): Option[(A1, B)] =
if (isEmpty || that.isEmpty) None else Some((this.get, that.get))
*/

    /**
     *
     * Реализовать метод filter, который будет возвращать не пустой Option
     * в случае если исходный не пуст и предикат от значения = true
     */

    def filter(f: T => Boolean): Option[T] = this match {
      case Option.Some(v) if f(v) => Option.Some(v)
      case Option.None => Option.None
    }
  }

  object Option {

    case class Some[T](v: T) extends Option[T]

    case object None extends Option[Nothing]

  }

}


/**
 *
 * Реализовать односвязанный иммутабельный список List
 * Список имеет два случая:
 * Nil - пустой список
 * Cons - непустой, содердит первый элемент (голову) и хвост (оставшийся список)
 */

object list {

  sealed trait List[+T] {
    //  def :: [B >: A](elem: B): List[B] =  new ::(elem, this)
    def ::[B >: T](elem: B): List[B] = Cons(elem, this)

    def apply[T](arg: T*): List[T] = {
      var l: List[T] = List.Nil
      arg.foreach(el => l = el :: l)
      l
    }

    def mkString(str: String = ","): String = this match {
      case List.Cons(head, tail) => s"$head$str" ++ tail.mkString(str)
      case List.Cons(head, Nil) => s"$head"
      case List.Nil => ""
    }
    

    def map[T](f: T => T): List[T] = this match {
      case List.Nil => List.Nil
      case List.Cons(h, t) => f(h) :: t.map(f)
    }

    def filter(f: T => Boolean): List[T] = this match {
      case List.Cons(head, tail) => if (f(head)) head :: tail.filter(f) else tail.filter(f)
      case List.Nil => List.Nil
    }

    def incList(list: List[Int]): List[Int] = list.map(_ + 1)

    def shoutString(list: List[String]): List[String] = list.map(_ + "!")

  }

  object List {

    case class Cons[T](head: T, tail: List[T]) extends List[T]

    case object Nil extends List[Nothing]

  }

}