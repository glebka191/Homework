package module1

import module1.list.List

object Test {
  def main(args: Array[String]): Unit = {
    println(assert((5 :: List.Nil) == List.Cons(5, List.Nil)))

    println(assert((10 :: List.Cons(5, List.Nil)) ==
      List.Cons(10, List.Cons(5, List.Nil))))

  }
}
