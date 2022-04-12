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

  def generar_color(letra: Char): String = {
    letra match {
      case 'A' => "Azul"
      case 'R' => "Rojo"
      case 'N' => ""
    }
  }

  /*def obtener_posicion(tablero: List[List[Int]], fila: Int, columna: Int): Int = {
    def obtener_columna(fila: List[Int], columna:Int): Int = {
      if (columna == 0) {
        fila.head
      } else {
        obtener_columna(fila.tail, columna - 1)
      }
    }

    if (fila == 0) {
      obtener_columna(tablero.head, columna)
    } else {
      obtener_posicion(tablero.tail, fila - 1, columna)
    }
  }*/

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
      else fila.head::cambiar_estado_fila(fila.tail, columna - 1, estado)
    }

    if (fila == 0) cambiar_estado_fila(tablero.head, columna, estado) :: tablero.tail
    else tablero.head :: cambiar_estado(tablero.tail, fila - 1, columna, estado)
  }

  def pulsar_bloque(tablero:List[List[Int]], fila:Int, columna:Int): List[List[Int]] ={
    val bloque = get_bloque(tablero, fila, columna)
    if (bloque != 0) {
      // val nuevo_tablero = cambiar_estado(tablero, fila, columna, 0)
      bloque match {
        case 'B' => println("Has pulsado una bomba")
        case _ => println("Has pulsado un bloque " + bloque + " y tiene " + contar_iguales(tablero, fila, columna, Nil).length + " bloques iguales alrededor")
      }
      tablero
    } else {
      println("No hay bloque en esa posicion")
      tablero
    }

  }

  def tablero_vacio(tablero: List[List[Int]]): Boolean = {
    def tablero_vacio_fila(fila: List[Int]): Boolean = {
      if (fila.isEmpty) true
      else if (fila.head == 0) tablero_vacio_fila(fila.tail)
      else false
    }

    if (tablero.isEmpty) true
    else tablero_vacio_fila(tablero.head) && tablero_vacio(tablero.tail)
  }

  def actualizar_puntuacion(t1: List[List[Int]], t2: List[List[Int]], puntuacion:Int): Int = {
    def diferencia_tableros(t1: List[List[Int]], t2: List[List[Int]]): Int = {
      if (t1.isEmpty) 0
      else if (t1.head == t2.head) diferencia_tableros(t1.tail, t2.tail)
      else 1 + diferencia_tableros(t1.tail, t2.tail)
    }

    val diferentes = diferencia_tableros(t1, t2)
    if (diferentes > 2) puntuacion + diferentes*10
    else 0
  }

  // Función que desplaza los bloques una fila hacia abajo, si el elemento de debajo tiene estado 0
  def desplazar_bloques(tablero: List[List[Int]]):List[List[Int]] = {
    def desplazar_columna(tablero: List[List[Int]], fila:Int, columna:Int):List[List[Int]] = {
      if (fila == tablero.length-1) tablero
      else {
        val nuevo_tablero = desplazar_columna(tablero, fila + 1, columna)
        val bloque_actual = tablero(fila)(columna)
        if (nuevo_tablero(fila+1)(columna) == 0)
        {
          val nuevo_tablero2 = cambiar_estado(nuevo_tablero, fila+1, columna, bloque_actual)
          cambiar_estado(nuevo_tablero2, fila, columna, 0)
        }
        else nuevo_tablero
      }
    }

    def desplazar_fila(tablero:List[List[Int]], fila:Int, columna:Int):List[List[Int]] = {
      if (columna == tablero.head.length - 1) tablero
      else if (fila!=tablero.length && es_columna_vacia(tablero, columna)) {
        val nuevo_tablero = cambiar_estado(tablero, fila, columna, tablero(fila)(columna + 1))
        val nuevo_tablero2 = cambiar_estado(nuevo_tablero, fila, columna + 1, tablero(fila)(columna))
        desplazar_fila(nuevo_tablero2, fila + 1, columna)
      }
      else desplazar_fila(tablero,0, columna+1)
    }

    def es_columna_vacia(tablero: List[List[Int]], columna:Int):Boolean = {
      if (tablero.tail == Nil) tablero.head(columna) == 0
      else es_columna_vacia(tablero.tail, columna)
    }

    def desplazar_bloques_aux(tablero: List[List[Int]], fila: Int, columna: Int):List[List[Int]] = {
      if (columna == tablero(0).length) tablero
      else {
        val tablero_modificado = desplazar_bloques_aux(tablero, fila, columna + 1)
        val tablero_modificado2 = desplazar_columna(tablero_modificado, fila, columna)
        if (es_columna_vacia(tablero_modificado2, columna)) desplazar_fila(tablero_modificado2, 0 ,columna)
        else tablero_modificado2
      }
    }
    desplazar_bloques_aux(tablero,  0, 0)
  }

  def contiene(matriz:List[List[Int]], lista:List[Int]): Boolean ={
    if (matriz == Nil) false
    else if (matriz.tail == Nil) listas_iguales(matriz.head, lista)
    else contiene(matriz.tail, lista)
  }

  def listas_iguales(l1: List[Int], l2: List[Int]): Boolean = {
    if (l1.isEmpty) l2.isEmpty
    else if (l1.head == l2.head) listas_iguales(l1.tail, l2.tail)
    else false
  }

  def contar_iguales(tablero: List[List[Int]], fila: Int, columna: Int, elementos: List[List[Int]]): List[List[Int]] = {
    def contar_iguales_aux(tablero: List[List[Int]], fila:Int, columna:Int, inc_x: Int, inc_y: Int, elementos:List[List[Int]]): List[List[Int]] ={
      if (fila + inc_x < 0 || fila + inc_x == tablero.length || columna+inc_y < 0 || columna+inc_y == tablero.head.length) elementos
      else if (tablero(fila+inc_x)(columna+inc_y) == tablero(fila)(columna)) contar_iguales(tablero, fila+inc_x, columna+inc_y, elementos)
      else elementos
    }

    // 3,2 --> (3,2), (3,3), (3,4)
    print(elementos)

    if (!contiene(elementos, List(fila, columna))){
      val elem2 = List(fila, columna) :: elementos
      val elem3 = contar_iguales_aux(tablero, fila, columna, 0, 1, elem2) // Hacia derecha
      val elem4 = contar_iguales_aux(tablero, fila, columna, 1, 0, elem3) // Hacia abajo
      val elem5 = contar_iguales_aux(tablero, fila, columna, 0, -1, elem4) // Hacia izquierda
      contar_iguales_aux(tablero, fila, columna, -1, 0, elem5) // Hacia arriba
    }
    else elementos
  }

  def jugar(tablero: List[List[Int]], puntuacion: Int, vidas: Int):Unit = {
    if (!tablero_vacio(tablero)) {
      println("Nuevo turno: ")
      pintar_tablero(tablero)
      println("Introduce la posición que quieres pulsar: x,y")
      val coord = readLine()
      val nuevo_tablero = pulsar_bloque(tablero, Integer.parseInt(coord.split(",")(0)), Integer.parseInt(coord.split(",")(1)))
      val puntuacion_nueva = actualizar_puntuacion(tablero, nuevo_tablero, puntuacion)
      val tablero_desplazado = desplazar_bloques(nuevo_tablero)
      if (puntuacion_nueva==0) jugar(tablero_desplazado, puntuacion_nueva, vidas-1)
      else jugar(tablero_desplazado, puntuacion_nueva, vidas)
    }
  }

  val tablero = generar_tablero_aleatorio(10, 10)
  jugar(tablero,0, 8)
}
