// Tansform vector into parallel vector and add two vectors
import scala.collection.parallel.immutable.ParVector
import scala.collection.parallel.CollectionConverters._

object parVectors extends App {
  def map(v: ParVector[Vector[Int]], f: Int => Int): ParVector[Vector[Int]] = {
    if (v.isEmpty) v
    else v.head.map(f) +: map(v.tail, f)
  }

  val vector = Vector(Vector(1,2,3), Vector(4,5,6), Vector(7,8,9))
  val parVector = vector.par
  val parVector2 = map(parVector, (x: Int) => x + 1)
  val vector2 = parVector2.seq
  println(vector2)
}
