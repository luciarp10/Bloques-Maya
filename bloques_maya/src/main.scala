import scala.io.StdIn.readLine
import scala.util.Random

import java.lang.Integer;

object main extends App {
  // TODO: Repensar la generación del tablero con las restricciones de la práctica
  def generar_tablero_aleatorio(filas: Int, columnas: Int, colores:Set[Int]): List[List[Int]] = {
    if (filas == 0) {
      List()
    } else {
      val fila = List.fill(columnas)(Random.shuffle(colores).head)
      fila :: generar_tablero_aleatorio(filas - 1, columnas, colores)
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

    def pintar_tablero_aux(tablero:List[List[Int]], fila:Int):Unit ={
      if (tablero.isEmpty) println()
      else {
        print(fila+" | ")
        pintar_fila(tablero.head)
        pintar_tablero_aux(tablero.tail, fila+1)
      }
    }

    def pintar_indices(i: Int, f: Int): Unit = {
      if (i == f) {
        println()
      } else {
        print(i + " ")
        pintar_indices(i + 1, f)
      }
    }

    def pintar_char
    (n:Int, c:Char):Unit = {
      if (n == 0) {
        println()
      } else {
        print(c+" ")
        pintar_char(n - 1, c)
      }
    }

    print("    ")
    pintar_indices(0, tablero.head.length)
    print("    ")
    pintar_char(tablero.head.length, '_')
    pintar_tablero_aux(tablero, 0)

  }

  def obtener_columna(fila: List[Int], columna:Int): Int = {
    if (columna == 0) {
      fila.head
    } else if (columna < 0) -1
    else {
      obtener_columna(fila.tail, columna - 1)
    }
  }

  def obtener_posicion(tablero: List[List[Int]], fila: Int, columna: Int): Int = {
    if (fila == 0) {
      obtener_columna(tablero.head, columna)
    } else if (fila < 0) -1
    else {
      obtener_posicion(tablero.tail, fila - 1, columna)
    }
  }

  def get_bloque(tablero: List[List[Int]], fila: Int, columna: Int): Char = {
    if (fila < 0 || columna < 0 || fila >= tablero.length || columna >= tablero.head.length) 0
    else leer_estado(tablero(fila)(columna))
  }

  def cambiar_estado_multipos(tablero: List[List[Int]], lista_pos: List[List[Int]], estado: Int): List[List[Int]] = {
    if(lista_pos.isEmpty) tablero
    else {
      val tablero_nuevo = cambiar_estado(tablero, lista_pos.head(0), lista_pos.head(1), estado)
      cambiar_estado_multipos(tablero_nuevo, lista_pos.tail, estado)
    }
  }

  def cambiar_estado(tablero: List[List[Int]], fila: Int, columna: Int, estado: Int): List[List[Int]] = {
    def cambiar_estado_fila(fila: List[Int], columna:Int,  estado: Int): List[Int] = {
      if (fila.isEmpty) fila
      else if (columna == 0) estado::fila.tail
      else fila.head::cambiar_estado_fila(fila.tail, columna - 1, estado)
    }

    if (tablero.isEmpty) tablero
    else if (fila == 0) cambiar_estado_fila(tablero.head, columna, estado) :: tablero.tail
    else tablero.head :: cambiar_estado(tablero.tail, fila - 1, columna, estado)
  }

  def pulsar_bloque(tablero:List[List[Int]], fila:Int, columna:Int): List[List[Int]] ={
    val bloque = get_bloque(tablero, fila, columna)
    if (bloque != 0) {
      // val nuevo_tablero = cambiar_estado(tablero, fila, columna, 0)
      bloque match {
        case 'B' =>
          explotar_bomba(tablero, fila, columna)
        case _ =>
          val iguales = contar_iguales(tablero, fila, columna, Nil)
          if (iguales.length > 2) {
            cambiar_estado_multipos(tablero, iguales, 0)
          }
          else {
            println("Has pulsado un grupo de " + iguales.length + " bloques, has perdido una vida.")
            cambiar_estado(tablero, fila, columna, 0)
          }
      }
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

  def actualizar_puntuacion(t1: List[List[Int]], t2: List[List[Int]], puntuacion:Int, bloque_pulsado: List[Int]): Int = {
    def diferencia_tableros(t1: List[List[Int]], t2: List[List[Int]]): Int = {
      if (t1.isEmpty && t2.isEmpty) 0
      else if (t1.isEmpty) t2.head.length + diferencia_tableros(t1, t2.tail)
      else if (t2.isEmpty) t1.head.length + diferencia_tableros(t1.tail, t2)
      else diferencia_listas(t1.head, t2.head) + diferencia_tableros(t1.tail, t2.tail)
    }

    def diferencia_listas(l1: List[Int], l2: List[Int]): Int = {
      if (l1.isEmpty && l2.isEmpty) 0
      else if (l1.isEmpty) l2.length
      else if (l2.isEmpty) l1.length
      else if (l1.head != l2.head) 1 + diferencia_listas(l1.tail, l2.tail)
      else diferencia_listas(l1.tail, l2.tail)
    }

    val diferentes = diferencia_tableros(t1, t2)
    if (diferentes > 2 || obtener_posicion(t1, obtener_columna(bloque_pulsado, 0), obtener_columna(bloque_pulsado,1)) == 8) puntuacion + diferentes*10
    else 0
  }

  // Función que desplaza los bloques una fila hacia abajo, si el elemento de debajo tiene estado 0
  def desplazar_bloques(tablero: List[List[Int]]):List[List[Int]] = {
    def desplazar_columna_aux(tablero: List[List[Int]], fila:Int, columna:Int, tablero_antiguo:List[List[Int]]):List[List[Int]] = {
      if (columna == tablero.head.length) desplazar_columna(tablero, tablero_antiguo)
      else if (fila == tablero.length-1) desplazar_columna_aux(tablero, 0, columna+1, tablero_antiguo)
      else {
        val bloque_actual = tablero(fila)(columna)
        if (tablero(fila+1)(columna) == 0)
        {
          val nuevo_tablero = cambiar_estado(tablero, fila+1, columna, bloque_actual)
          val nuevo_tablero2 = cambiar_estado(nuevo_tablero, fila, columna, 0)
          desplazar_columna_aux(nuevo_tablero2, fila+1, columna, nuevo_tablero)
        }
        else desplazar_columna_aux(tablero, fila + 1, columna, tablero_antiguo)
      }
    }

    def desplazar_columna(tablero: List[List[Int]], nuevo_tablero: List[List[Int]]):List[List[Int]] = {
      if (matrices_iguales(tablero, nuevo_tablero)) nuevo_tablero
      else desplazar_columna_aux(tablero, 0, 0, tablero)
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
        val tablero_modificado2 = desplazar_columna(tablero_modificado, Nil)
        if (es_columna_vacia(tablero_modificado2, columna)) desplazar_fila(tablero_modificado2, 0 ,columna)
        else tablero_modificado2
      }
    }
    desplazar_bloques_aux(tablero,  0, 0)
  }

  def contiene(matriz:List[List[Int]], lista:List[Int]): Boolean ={
    if (matriz == Nil) false
    else if (matriz.tail == Nil) listas_iguales(matriz.head, lista)
    else listas_iguales(matriz.head, lista) || contiene(matriz.tail, lista)
  }

  def listas_iguales(l1: List[Int], l2: List[Int]): Boolean = {
    if (l1.isEmpty) l2.isEmpty
    else if (l1.head == l2.head) listas_iguales(l1.tail, l2.tail)
    else false
  }

  def matrices_iguales(m1:List[List[Int]], m2:List[List[Int]]): Boolean = {
    if (m1.isEmpty) m2.isEmpty
    else if (m2.isEmpty) false
    else if (m1.tail == Nil) listas_iguales(m1.head, m2.head)
    else listas_iguales(m1.head, m2.head) && matrices_iguales(m1.tail, m2.tail)
  }

  def contar_iguales(tablero: List[List[Int]], fila: Int, columna: Int, elementos: List[List[Int]]): List[List[Int]] = {
    def contar_iguales_aux(tablero: List[List[Int]], fila:Int, columna:Int, inc_x: Int, inc_y: Int, elementos:List[List[Int]]): List[List[Int]] ={
      if (fila + inc_x < 0 || fila + inc_x == tablero.length || columna+inc_y < 0 || columna+inc_y == tablero.head.length) elementos
      else if (tablero(fila+inc_x)(columna+inc_y) == tablero(fila)(columna)) contar_iguales(tablero, fila+inc_x, columna+inc_y, elementos)
      else elementos
    }

    if (!contiene(elementos, List(fila, columna))){
      val elem2 = List(fila, columna) :: elementos
      val elem3 = contar_iguales_aux(tablero, fila, columna, 0, 1, elem2) // Hacia derecha
      val elem4 = contar_iguales_aux(tablero, fila, columna, 1, 0, elem3) // Hacia abajo
      val elem5 = contar_iguales_aux(tablero, fila, columna, 0, -1, elem4) // Hacia izquierda
      contar_iguales_aux(tablero, fila, columna, -1, 0, elem5) // Hacia arriba
    }
    else elementos
  }

  def explotar_bomba(tablero: List[List[Int]], fila: Int, columna: Int): List[List[Int]] = {
    val nuevo_tablero = cambiar_estado(tablero, fila-1, columna-1, 0) // Arriba izquierda
    val nuevo_tablero_1 = cambiar_estado(nuevo_tablero, fila-1, columna, 0) // Arriba
    val nuevo_tablero_2 = cambiar_estado(nuevo_tablero_1, fila-1, columna+1, 0) // Arriba derecha

    val nuevo_tablero_3 = cambiar_estado(nuevo_tablero_2, fila, columna-1, 0) // Izquierda
    val nuevo_tablero_4 = cambiar_estado(nuevo_tablero_3, fila, columna, 0) // Centro
    val nuevo_tablero_5 = cambiar_estado(nuevo_tablero_4, fila, columna+1, 0) // Derecha

    val nuevo_tablero_6 = cambiar_estado(nuevo_tablero_5, fila+1, columna-1, 0) //Abajo izquierda
    val nuevo_tablero_7 = cambiar_estado(nuevo_tablero_6, fila+1, columna, 0) //Abajo centro
    cambiar_estado(nuevo_tablero_7, fila+1, columna+1, 0) //Abajo derecha
  }

  def map_to_int(list: List[String]): List[Int] = {
    if (list == Nil) Nil
    else list.head.toInt :: map_to_int(list.tail)
  }

  def jugar(tablero: List[List[Int]], puntuacion: Int, vidas: Int):List[Int] = {
    if (!tablero_vacio(tablero) && vidas > 0) {
      println("Nuevo turno: Vidas: " + vidas + " Puntuacion: " + puntuacion)
      pintar_tablero(tablero)
      println("Introduce la posición que quieres pulsar: x,y")
      val coord = map_to_int(readLine().split(",").toList)
      val nuevo_tablero = pulsar_bloque(tablero, obtener_columna(coord,0), obtener_columna(coord,1))
      val puntuacion_nueva = actualizar_puntuacion(tablero, nuevo_tablero, puntuacion, coord)
      val tablero_desplazado = desplazar_bloques(nuevo_tablero)
      if (puntuacion_nueva==0) jugar(tablero_desplazado, puntuacion_nueva, vidas-1)
      else jugar(tablero_desplazado, puntuacion_nueva, vidas)
    }
    else if (vidas > 0) {
      println("Has finalizado el tablero con una puntuacion de " + puntuacion + ". Se va a generar un nuevo tablero.")
      List(puntuacion, vidas)
    } else {
      println("Has perdido la partida")
      List(puntuacion, vidas)
    }
  }

  def create_list(x: Integer, y:Integer):List[Int] = {
    List(x.intValue(), y.intValue())
  }

  def colores_tablero(n_colores:Int):Set[Int] = {
    if (n_colores == 0) Set()
    else Set(1 + Random.nextInt(7)).++(colores_tablero(n_colores-1))
  }

  def colocar_bombas(tablero:List[List[Int]], n_bombas:Int):List[List[Int]] = {
    if (n_bombas == 0) tablero
    else {
      val fila = Random.nextInt(tablero.length)
      val columna = Random.nextInt(tablero.head.length)
      if (tablero(fila)(columna) != 8) colocar_bombas(cambiar_estado(tablero, fila, columna, 8), n_bombas-1)
      else colocar_bombas(tablero, n_bombas)
    }
  }

  def generar_tablero(nivel: Int):List[List[Int]] = nivel match {
    case 1 =>
      val colores = colores_tablero(3)
      val tablero = generar_tablero_aleatorio(9,11,colores)
      colocar_bombas(tablero, 2)
    case 2 =>
      val colores = colores_tablero(5)
      val tablero = generar_tablero_aleatorio(12,16,colores)
      colocar_bombas(tablero, 3)
    case 3 =>
      val colores = colores_tablero(7)
      val tablero = generar_tablero_aleatorio(25,15,colores)
      colocar_bombas(tablero, 5)
  }

  def create_empty_matrix():List[List[Int]] = {
    Nil
  }


  def bucle_juego(nivel: Int, puntuacion:Int, vidas: Int, partidas:Int):Unit = {
    vidas match {
      case 0 =>
        println("Has perdido la partida. Tu puntuación ha sido: " + puntuacion +" en " + partidas + " partidas.")
        menu()
      case _ =>
        val tablero = generar_tablero(nivel)
        val datos = jugar(tablero, puntuacion, vidas)

        if (obtener_columna(datos, 1) == 0) bucle_juego(nivel, puntuacion, 0, partidas+1)
        else bucle_juego(nivel, puntuacion+obtener_columna(datos, 0), obtener_columna(datos, 1), partidas+1)
    }
  }

  // Hacer menú que almacene los datos de las partidas y el tiempo jugado
  def menu():Unit = {
    println("Elige el nivel que quieres jugar: \n\t1 - Facil, \n\t2 - Medio, \n\t3 - Dificil, \n\t4 - Salir \nNivel:")
    val nivel_elegido = readLine().toInt

    nivel_elegido match {
      case 1 => bucle_juego(1, 0, 8, 0)
      case 2 => bucle_juego(2, 0, 10,0)
      case 3 => bucle_juego(3, 0, 15,0)
      case 4 => println("Adios, no vuelvas.")
      case _ =>
        println("Elige un nivel válido")
        menu()
    }
  }

  menu()
}
