
object parVectors extends App {
  // Transformar tablero paralelo en tablero secuencial
  val p = List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))
  val p1 = p.map(_.par).par
  println(p1)
  val p2 = p1.zipWithIndex.map { case (row, i) => row.zipWithIndex.map { case (e, j) => println("Fila:"+i+" Columna:"+j+ " Valor:"+e) } }
}