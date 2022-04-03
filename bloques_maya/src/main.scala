import scala.io.StdIn.readLine

object main extends App {
  def generar_tablero_aleatorio(filas: Int, columnas: Int): List[List[Int]] = {
    if (filas == 0) {
      List()
    } else {
      val fila = List.fill(columnas)(util.Random.nextInt(8) + 1)
      fila :: generar_tablero_aleatorio(filas - 1, columnas)
    }
  }

  def leer_estado(n: Int): Char = n match {
    case 0 => ' '
    case 1 => 'A'
    case 2 => 'R'
    case 3 => 'N'
    case 4 => 'V'
    case 5 => 'P'
    case 6 => 'M'
    case 7 => 'G'
    case 8 => 'B'
  }

  def pintar_tablero(tablero: List[List[Int]]): Unit = {
    def pintar_fila(fila: List[Int]): Unit = {
      if (fila.isEmpty) {
        println()
      } else {
        print(leer_estado(fila.head) + " ")
        pintar_fila(fila.tail)
      }
    }

    if (tablero.isEmpty) println()
    else {
      pintar_fila(tablero.head)
      pintar_tablero(tablero.tail)
    }
  }

  def get_bloque(tablero: List[List[Int]], fila: Int, columna: Int): Char = {
    if (fila < 0 || columna < 0 || fila >= tablero.length || columna >= tablero.head.length) 0
    else leer_estado(tablero(fila)(columna))
  }

  def contiene(fila: List[Int], estado:Int): Boolean = {
    if (fila.isEmpty) false
    else if (fila.head == estado) true
    else contiene(fila.tail, estado)
  }

  def cambiar_estado(tablero: List[List[Int]], fila: Int, columna: Int, estado: Int): List[List[Int]] = {
    def cambiar_estado_fila(fila: List[Int], columna:Int,  estado: Int): List[Int] = {
      if (columna == 0) estado::fila.tail
      else cambiar_estado_fila(fila.tail, columna - 1, estado)
    }

    if (fila == 0) cambiar_estado_fila(tablero.head, columna, estado) :: tablero.tail
    else tablero.head :: cambiar_estado(tablero.tail, fila - 1, columna, estado)
  }

  val tablero = generar_tablero_aleatorio(10, 10);
  pintar_tablero(tablero)
}
