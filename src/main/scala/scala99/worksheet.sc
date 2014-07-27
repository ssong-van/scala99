import scala.util.Random

val l = List(1,1,1,2,2,3,3,3,3,1,1)
l.collect{
  case e => e + 1
}
l.map{
  case e => e + 1
}

//List.fill(3)(n => n)
l flatMap {e => List.tabulate(3)(_ => e)}

val a = List()
a ++: List(1) ++: List('3')

List(1,2,3,4,5,6).foldLeft((1, List[Int]())){
  (acc, e) =>
    println(acc._1, acc._1 % 3)
    if (acc._1 % 3 != 0)
      (acc._1 + 1, acc._2 ++: List(e))
    else (acc._1+1, acc._2)
}._2

// Add it as is
List(1,2) :+ 3
List(1,2) :+ List(3,4)
List(1,2) :: List(3,4)
List(1,2) +: List(3,4)

// Insert each element
List(1,2) ::: List(3,4)
List(1,2) ++ List(3,4)

Random.shuffle(List(1,2,3))
