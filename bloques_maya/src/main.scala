import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.util.Random
import scala.collection.parallel.immutable._

object main extends App {
  def list_to_par(list: List[List[Int]]): ParVector[ParSeq[Int]] = {
    if (list.isEmpty) {
      ParVector()
    } else
      ParVector(list.head.par) ++ list_to_par(list.tail)
  }

  def par_to_list(par: ParVector[ParSeq[Int]]): List[List[Int]] = {
    if (par.isEmpty) {
      List()
    } else
      par.head.seq.toList :: par_to_list(par.tail)
  }

  def generar_tablero_aleatorio(tablero_vacio: ParVector[ParSeq[Int]], colores:Set[Int]): ParVector[ParSeq[Int]] = {
    tablero_vacio.map(fila => fila.map(columna => Random.shuffle(colores).head))
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
    @tailrec
    def pintar_fila(fila: List[Int]): Unit = {
      if (fila.isEmpty) {
        println()
      } else {
        print(leer_estado(fila.head) + " ")
        pintar_fila(fila.tail)
      }
    }

    @tailrec
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

  @tailrec
  def obtener_columna(fila: List[Int], columna:Int): Int = {
    if (columna == 0) {
      fila.head
    } else if (columna < 0) -1
    else {
      obtener_columna(fila.tail, columna - 1)
    }
  }

  @tailrec
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
    else leer_estado(obtener_posicion(tablero, fila, columna))
  }

  def cambiar_estado_multipos(tablero: ParVector[ParSeq[Int]], lista_pos: List[List[Int]], estado: Int): ParVector[ParSeq[Int]] = {
    tablero.zipWithIndex.map {case (fila, i) => fila.zipWithIndex.map {case (columna, j) => if (lista_pos.contains(List(i, j))) estado else columna}}
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
      bloque match {
        case 'B' =>
          explotar_bomba(tablero, fila, columna)
        case _ =>
          val iguales = contar_iguales(tablero, fila, columna, Nil)
          if (iguales.length > 2) {
            par_to_list(cambiar_estado_multipos(list_to_par(tablero), iguales, 0))
          }
          else
            cambiar_estado(tablero, fila, columna, 0)
      }
    } else {
      println("No hay bloque en esa posicion")
      tablero
    }

  }

  def tablero_vacio(tablero: ParVector[ParSeq[Int]]): Boolean = {
    tablero.filter(fila => fila.filter(columna => columna != 0).isEmpty).length == tablero.length
  }

  def actualizar_puntuacion(t1: List[List[Int]], t2: List[List[Int]], puntuacion:Int, bloque_pulsado: List[Int]): Int = {
    def diferencia_tableros(t1: ParVector[ParSeq[Int]], t2: List[List[Int]]): Int = {
      val cont = t1.zipWithIndex.map {case (fila, i) => fila.zipWithIndex.map{case (columna, j) => if (obtener_posicion(t2, i, j) != columna) 1 else 0}}
      cont.flatten.sum // TODO Cambiar por método personalizado
    }

    val diferentes = diferencia_tableros(list_to_par(t1), t2)
    if (diferentes > 2 || obtener_posicion(t1, obtener_columna(bloque_pulsado, 0), obtener_columna(bloque_pulsado,1)) == 8) puntuacion + diferentes*10
    else 0
  }

  // Función que desplaza los bloques una fila hacia abajo, si el elemento de debajo tiene estado 0
  def desplazar_bloques(tablero: List[List[Int]]):List[List[Int]] = {
    @tailrec
    def desplazar_columna_aux(tablero: List[List[Int]], fila:Int, columna:Int, tablero_antiguo:List[List[Int]]):List[List[Int]] = {
      if (columna == tablero.head.length) desplazar_columna(tablero, tablero_antiguo)
      else if (fila == tablero.length-1) desplazar_columna_aux(tablero, 0, columna+1, tablero_antiguo)
      else {
        val bloque_actual =  obtener_posicion(tablero, fila, columna)
        if (obtener_posicion(tablero, fila+1, columna) == 0)
        {
          val tablero_bloque_movido = cambiar_estado(tablero, fila+1, columna, bloque_actual)
          val tablero_hueco_movido = cambiar_estado(tablero_bloque_movido, fila, columna, 0)
          desplazar_columna_aux(tablero_hueco_movido, fila+1, columna, tablero_bloque_movido)
        }
        else desplazar_columna_aux(tablero, fila + 1, columna, tablero_antiguo)
      }
    }

    def desplazar_columna(tablero: List[List[Int]], nuevo_tablero: List[List[Int]]):List[List[Int]] = {
      if (matrices_iguales(tablero, nuevo_tablero)) nuevo_tablero
      else desplazar_columna_aux(tablero, 0, 0, tablero)
    }

    @tailrec
    def desplazar_fila(tablero:List[List[Int]], fila:Int, columna:Int):List[List[Int]] = {
      if (columna == tablero.head.length - 1) tablero
      else if (fila!=tablero.length && es_columna_vacia(tablero, columna)) {
        val tablero_bloque_movido = cambiar_estado(tablero, fila, columna, obtener_posicion(tablero, fila, columna+1))
        val tablero_hueco_movido = cambiar_estado(tablero_bloque_movido, fila, columna + 1, obtener_posicion(tablero, fila, columna))
        desplazar_fila(tablero_hueco_movido, fila + 1, columna)
      }
      else desplazar_fila(tablero,0, columna+1)
    }

    @tailrec
    def es_columna_vacia(tablero: List[List[Int]], columna:Int):Boolean = {
      if (tablero.tail == Nil) tablero.head(columna) == 0
      else es_columna_vacia(tablero.tail, columna)
    }

    def desplazar_bloques_aux(tablero: List[List[Int]], fila: Int, columna: Int):List[List[Int]] = {
      if (columna == tablero(0).length) tablero
      else {
        val tablero_bajado = desplazar_bloques_aux(tablero, fila, columna + 1)
        val tablero_desplazado = desplazar_columna(tablero_bajado, Nil)
        if (es_columna_vacia(tablero_desplazado, columna)) desplazar_fila(tablero_desplazado, 0 ,columna)
        else tablero_desplazado
      }
    }
    desplazar_bloques_aux(tablero,  0, 0)
  }

  @tailrec
  def contiene(matriz:List[List[Int]], lista:List[Int]): Boolean ={
    if (matriz == Nil) false
    else if (matriz.tail == Nil) listas_iguales(matriz.head, lista)
    else listas_iguales(matriz.head, lista) || contiene(matriz.tail, lista)
  }

  @tailrec
  def listas_iguales(l1: List[Int], l2: List[Int]): Boolean = {
    if (l1.isEmpty) l2.isEmpty
    else if (l1.head == l2.head) listas_iguales(l1.tail, l2.tail)
    else false
  }

  @tailrec
  def matrices_iguales(m1:List[List[Int]], m2:List[List[Int]]): Boolean = {
    if (m1.isEmpty) m2.isEmpty
    else if (m2.isEmpty) false
    else if (m1.tail == Nil) listas_iguales(m1.head, m2.head)
    else listas_iguales(m1.head, m2.head) && matrices_iguales(m1.tail, m2.tail)
  }

  def contar_iguales(tablero: List[List[Int]], fila: Int, columna: Int, elementos: List[List[Int]]): List[List[Int]] = {
    def contar_iguales_aux(tablero: List[List[Int]], fila:Int, columna:Int, inc_x: Int, inc_y: Int, elementos:List[List[Int]]): List[List[Int]] ={
      if (fila + inc_x < 0 || fila + inc_x == tablero.length || columna+inc_y < 0 || columna+inc_y == tablero.head.length) elementos
      else if (obtener_posicion(tablero, fila+inc_x, columna+inc_y) == obtener_posicion(tablero, fila, columna) && obtener_posicion(tablero, fila, columna)!=0) contar_iguales(tablero, fila+inc_x, columna+inc_y, elementos)
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
    val posiciones = List(
      List(fila-1, columna-1), List(fila-1, columna), List(fila-1, columna+1),
      List(fila, columna-1), List(fila,columna), List(fila, columna+1),
      List(fila+1, columna-1), List(fila+1, columna), List(fila+1, columna+1)
    )

    par_to_list(cambiar_estado_multipos(list_to_par(tablero), posiciones, 0))
  }

  def map_to_int(list: List[String]): List[Int] = {
    if (list == Nil) Nil
    else list.head.toInt :: map_to_int(list.tail)
  }

  def jugar(tablero: List[List[Int]], puntuacion: Int, vidas: Int):List[Int] = {
    if (!tablero_vacio(list_to_par(tablero)) && vidas > 0) {
      println("Nuevo turno: Vidas: " + vidas + " Puntuacion: " + puntuacion)
      pintar_tablero(tablero)
      println("Introduce la posición que quieres pulsar: x,y")
      val coord = map_to_int(readLine().split(",").toList)
      val tablero_tras_pulsar = pulsar_bloque(tablero, obtener_columna(coord,0), obtener_columna(coord,1))
      val puntuacion_nueva = actualizar_puntuacion(tablero,  tablero_tras_pulsar, puntuacion, coord)
      val tablero_desplazado = desplazar_bloques( tablero_tras_pulsar)
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

  @tailrec
  def colocar_bombas(tablero:List[List[Int]], n_bombas:Int):List[List[Int]] = {
    if (n_bombas == 0) tablero
    else {
      val fila = Random.nextInt(tablero.length)
      val columna = Random.nextInt(tablero.head.length)
      if (obtener_posicion(tablero, fila, columna) != 8) colocar_bombas(cambiar_estado(tablero, fila, columna, 8), n_bombas-1)
      else colocar_bombas(tablero, n_bombas)
    }
  }

  def generar_tablero(nivel: Int):List[List[Int]] = nivel match {
    case 1 =>
      val colores = colores_tablero(3)
      val t_vacio = List.fill(9)(List.fill(11)(0))
      val tablero = par_to_list(generar_tablero_aleatorio(list_to_par(t_vacio),colores))
      colocar_bombas(tablero, 2)
    case 2 =>
      val colores = colores_tablero(5)
      val t_vacio = List.fill(12)(List.fill(16)(0))
      val tablero = par_to_list(generar_tablero_aleatorio(list_to_par(t_vacio),colores))
      colocar_bombas(tablero, 3)
    case 3 =>
      val colores = colores_tablero(7)
      val t_vacio = List.fill(25)(List.fill(15)(0))
      val tablero = par_to_list(generar_tablero_aleatorio(list_to_par(t_vacio),colores))
      colocar_bombas(tablero, 5)
  }

  def create_empty_matrix():List[List[Int]] = {
    Nil
  }

  @tailrec
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

  @tailrec
  def obtener_coordenadas_bloque_IA(tablero:List[List[Int]], mejor_grupo:Int, mejor_pos:List[Int], fila:Int, columna:Int):List[Int] = {
    if(fila == tablero.length) mejor_pos
    else if (columna == tablero.head.length) obtener_coordenadas_bloque_IA(tablero, mejor_grupo, mejor_pos, fila+1, 0)
    else {
      val vecinos = contar_iguales(tablero, fila, columna, Nil)
      if (obtener_posicion(tablero, fila, columna) != 0 && vecinos.length < mejor_grupo && vecinos.length>2){
        obtener_coordenadas_bloque_IA(tablero, vecinos.length, List(fila, columna), fila, columna+1)
      }
      else obtener_coordenadas_bloque_IA(tablero, mejor_grupo, mejor_pos, fila, columna+1)
    }
  }

  @tailrec
  def buscar_bomba(tablero: List[List[Int]], fila:Int, columna: Int, ultima_coord: List[Int]): List[Int] = {
    if (fila == tablero.length) {
      println("No hay bombas")
      coordenada_aleatoria(tablero)
    }
    else if (columna == tablero.head.length) buscar_bomba(tablero, fila+1, 0, ultima_coord)
    else {
      if (obtener_posicion(tablero, fila, columna) == 8) List(fila, columna)
      else buscar_bomba(tablero, fila, columna+1, ultima_coord)
    }
  }

  @tailrec
  def coordenada_aleatoria(tablero:List[List[Int]]): List[Int] = {
    val fila = Random.nextInt(tablero.length)
    val columna = Random.nextInt(tablero.head.length)
    if (obtener_posicion(tablero, fila, columna) == 0) coordenada_aleatoria(tablero)
    else List(fila, columna)
  }

  def mejor_coordenada_actual(tablero: List[List[Int]], coords_padre:List[Int], punt_padre:Int):List[Int] = {
    if (tablero_vacio(list_to_par(tablero))) coords_padre ::: List(punt_padre)
    else{
      val coord = obtener_coordenadas_bloque_IA(tablero, tablero.length*tablero.head.length, List(0,0), 0 ,0)
      val punt = contar_iguales(tablero, obtener_columna(coord, 0), obtener_columna(coord, 1), Nil).length
      if (punt <= 2) {
        val coord2 = buscar_bomba(tablero, 0, 0, coord)
        val punt2 = contar_iguales(tablero, obtener_columna(coord2, 0), obtener_columna(coord2, 1), Nil).length
        if (punt2 <= 2) coord2 ::: List(0)
        else coord2 ::: List(punt2*10+ punt_padre)
      }
      else coord ::: List(punt*10 + punt_padre)
    }
  }

  def mejor_coordenada(tablero:List[List[Int]]): List[Int] = {
    def mejor_coordenada_aux(tablero:List[List[Int]], fila: Int, columna:Int, mejor_grupo:Int, mejor_pos:List[Int]):List[Int] = {
      if (fila == tablero.length) mejor_pos
      else if (columna == tablero.head.length) mejor_coordenada_aux(tablero, fila+1, 0, mejor_grupo, mejor_pos)
      else if (obtener_posicion(tablero, fila, columna) != 0) {
        val tablero_tras_pulsar = pulsar_bloque(tablero, fila, columna)
        val tablero_desplazado = desplazar_bloques(tablero_tras_pulsar )
        val puntuacion = actualizar_puntuacion(tablero, tablero_tras_pulsar , 0, List(fila,columna))
        val coords = mejor_coordenada_actual(tablero_desplazado, List(fila, columna), puntuacion)
        if (obtener_columna(coords,2) >= mejor_grupo) mejor_coordenada_aux(tablero, fila, columna+1, obtener_columna(coords,2),
          List(fila, columna))
        else mejor_coordenada_aux(tablero, fila, columna+1, mejor_grupo, mejor_pos)
      } else mejor_coordenada_aux(tablero, fila, columna+1, mejor_grupo, mejor_pos)
    }

    pintar_tablero(tablero)
    val coords = mejor_coordenada_aux(tablero, 0, 0, 0, Nil)
    coords
  }

  @tailrec
  def lanzarIA(tablero: List[List[Int]], puntuacion: Int, vidas: Int):List[List[Int]] ={
    val coords = mejor_coordenada(tablero)
    val tablero_tras_pulsar = pulsar_bloque(tablero, obtener_columna(coords, 0), obtener_columna(coords, 1))
    val tablero_desplazado = desplazar_bloques(tablero_tras_pulsar)
    val puntuacion_nueva = actualizar_puntuacion(tablero, tablero_tras_pulsar, puntuacion, List(obtener_columna(coords, 0), obtener_columna(coords, 1)))
    if(puntuacion_nueva == 0) lanzarIA(tablero_desplazado, puntuacion_nueva, vidas-1) // TODO: Arreglar en segunda parte
    else lanzarIA(tablero_desplazado, puntuacion_nueva, vidas)
  }

  // Hacer menú que almacene los datos de las partidas y el tiempo jugado
  def menu():Unit = {
    println("Elige el nivel que quieres jugar: \n\t1 - Facil, \n\t2 - Medio, \n\t3 - Dificil, \n\t4 - IA, \n\t5 - Salir \nNivel:")
    val nivel_elegido = readLine().toInt

    nivel_elegido match {
      case 1 => bucle_juego(1, 0, 8, 0)
      case 2 => bucle_juego(2, 0, 10,0)
      case 3 => bucle_juego(3, 0, 15,0)
      case 4 => lanzarIA(generar_tablero(1), 0, 8)
      case 5 => println("Adios, no vuelvas.")
      case _ =>
        println("Elige un nivel válido")
        menu()
    }
  }

  menu()
}
