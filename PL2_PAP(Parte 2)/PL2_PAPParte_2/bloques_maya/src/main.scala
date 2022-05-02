import scala.annotation.tailrec
import scala.collection.GenSeq
import scala.io.StdIn.readLine
import scala.util.Random
import scala.collection.parallel.immutable._

object main extends App {
  /* *************************************************************************************
   *                               LÓGICA OBLIGATORIA                                    *
   ***************************************************************************************/

  /**
   * Rellena un tablero vacío con estados aleatorios contenidos en <i>colores</i>.
   * @param tablero_vacio Tablero vacío a rellenar
   * @param colores Set de colores a usar
   * @return Tablero rellenado con estados aleatorios
   */
  def generar_tablero_aleatorio(filas: Int, columnas: Int, colores:Set[Int]): List[List[Int]] = {
    if (filas == 0) List()
    else {
      val fila = List.fill(columnas)(Random.shuffle(colores).head)
      fila :: generar_tablero_aleatorio(filas - 1, columnas, colores)
    }
  }

  /**
   * Método match que traduce un estado numérico a la inicial del color correspondiente.
   * @param n Estado de una celda
   * @return Color correspondiente al estado
   */
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

  /**
   * Pinta el tablero en la salida estándar.
   * @param tablero Tablero con estados numéricos.
   */
  def pintar_tablero(tablero: List[List[Int]]): Unit = {
    /**
     * Función auxiliar para pintar el tablero. Imprime todos los elementos de una fila de forma recursiva.
     * @param fila Lista que contiene estados numéricos
     */
    @tailrec
    def pintar_fila(fila: List[Int]): Unit = {
      if (fila.isEmpty) println()
      else {
        print(leer_estado(fila.head) + " ")
        pintar_fila(fila.tail)
      }
    }

    /**
     * Funcion auxiliar para pintar el tablero. Imprime todas las filas de la matriz de forma recursiva.
     * @param tablero Matriz de estados numéricos
     * @param fila Indice de la fila actual
     */
    @tailrec
    def pintar_tablero_aux(tablero:List[List[Int]], fila:Int):Unit ={
      if (tablero.isEmpty) println()
      else {
        print(fila+" | ")
        pintar_fila(tablero.head)
        pintar_tablero_aux(tablero.tail, fila+1)
      }
    }

    /**
     * Función auxiliar para pintar el tablero. Imprime una secuencia de numeros desde i hasta f (incluidos).
     * @param i Primer numero a imprimir
     * @param f Ultimo numero a imprimir
     */
    @tailrec
    def pintar_indices(i: Int, f: Int): Unit = i match {
      case `f` => print(f + " ")
      case _ => {
        print(i + " ")
        pintar_indices(i + 1, f)
      }
    }

    /**
     * Función auxiliar para pintar el tablero. Imprime una secuencia de n caracteres c separados por espacios.
     * @param n Número de caracteres a imprimir.
     * @param c Caracter a imprimir.
     */
    @tailrec
    def pintar_char(n:Int, c:Char):Unit = n match {
      case 0 => println()
      case _ => {
        print(c + " ")
        pintar_char(n-1, c)
      }
    }

    // Imprimimos el tablero por pantalla
    print("    ") // Tabulación para que el tablero quede centrado
    pintar_indices(0, tablero.head.length) // Imprimimos los indices de las columnas
    print("    ") // Tabulación para que el tablero quede centrado
    pintar_char(tablero.head.length, '_') // Imprimimos una linea de '_'
    pintar_tablero_aux(tablero, 0) // Imprimimos el tablero
  }


  /**
   * Obtiene el elemento columna de un lista fila.
   * @param lista Lista de enteros de la que queremos obtener la columna
   * @param columna Posición de la lista que queremos obtener
   * @return Elemento contenido en la posicion pedida.
   */
  @tailrec
  def obtener_columna(lista: List[Int], columna:Int): Int = {
    if (columna == 0) lista.head
    else {
      obtener_columna(lista.tail, columna - 1)
    }
  }

  /**
   * Obtiene el elemento contenido en la posicion (fila, columna) de tablero.
   * @param tablero Matriz de enteros de la que queremos obtener el elemento
   * @param fila Fila de la matriz que queremos obtener
   * @param columna Columna de la matriz que queremos obtener
   * @return Entero contenido en la posicion (fila, columna)
   */
  @tailrec
  def obtener_posicion(tablero: List[List[Int]], fila: Int, columna: Int): Int = {
    if (fila == 0) obtener_columna(tablero.head, columna)
    else {
      obtener_posicion(tablero.tail, fila - 1, columna)
    }
  }

  /**
   * Obtiene el elemento contenido en la posicion (fila, columna) de tablero en forma de caracter.
   * @param tablero Matriz de enteros de la que queremos obtener el elemento
   * @param fila Fila de la matriz que queremos obtener
   * @param columna Columna de la matriz que queremos obtener
   * @return Letra correspondiente al estado de la posicion (fila, columna) de tablero
   */
  def get_bloque(tablero: List[List[Int]], fila: Int, columna: Int): Char = {
    if (fila < 0 || columna < 0 || fila >= tablero.length || columna >= tablero.head.length) 0 // Si la posición está fuera del tablero, devolvemos un 0
    else leer_estado(obtener_posicion(tablero, fila, columna))
  }


  def cambiar_estado_multipos(tablero: List[List[Int]], lista_pos: List[List[Int]], estado: Int): List[List[Int]] = {
    if(lista_pos.isEmpty) tablero
    else {
      val tablero_nuevo = cambiar_estado(tablero, lista_pos.head(0), lista_pos.head(1), estado)
      cambiar_estado_multipos(tablero_nuevo, lista_pos.tail, estado)
    }
  }

  /**
   * Cambia el estado de una posición del tablero de forma recursiva.
   * @param tablero Matriz de enteros a modificar.
   * @param fila Posición de la fila a modificar.
   * @param columna Posición de la columna a modificar.
   * @param estado Nuevo estado para la posición (fila, columna).
   * @return Tablero con la posición (fila, columna) modificada.
   */
  def cambiar_estado(tablero: List[List[Int]], fila: Int, columna: Int, estado: Int): List[List[Int]] = {
    /**
     * Cambia el estado de una posición de una lista de enteros.
     * @param fila Lista con estados numéricos.
     * @param columna Posición de la columna a modificar.
     * @param estado Nuevo estado para la posición (fila, columna).
     * @return Lista con la posición (fila, columna) modificada.
     */
    def cambiar_estado_fila(fila: List[Int], columna:Int,  estado: Int): List[Int] = {
      if (fila.isEmpty) fila // Si la lista está vacía, no se aplica ningún cambio
      else if (columna == 0) estado::fila.tail // Si hemos llegado a la posición, cambiamos el estado actual
      else fila.head::cambiar_estado_fila(fila.tail, columna - 1, estado) // Si no, seguimos recorriendo la lista
    }

    if (tablero.isEmpty) tablero // Si el tablero está vacío, no se aplica ningún cambio
    else if (fila == 0) cambiar_estado_fila(tablero.head, columna, estado) :: tablero.tail // Si hemos llegado a la posición, cambiamos el estado actual
    else tablero.head :: cambiar_estado(tablero.tail, fila - 1, columna, estado) // Si no, seguimos recorriendo el tablero por filas
  }

  /**
   * Comprueba que no se ha pulsado un bloque vacío. Comprueba si el bloque pulsado es una bomba o no.
   * Si es una bomba, la explota sin condición, si no, comprueba que dicho bloque forme un grupo de 3 o más iguales.
   * Si se forman grupos de 3 o más, elimina todas las casillas del grupo, si no, elimina solo el bloque pulsado.
   * @param tablero Tablero a modificar.
   * @param fila Posición de la fila del bloque pulsado.
   * @param columna Posición de la columna del bloque pulsado.
   * @return Nuevo tablero con bloques eliminados.
   */
  def pulsar_bloque(tablero:List[List[Int]], fila:Int, columna:Int): List[List[Int]] ={
    val bloque = get_bloque(tablero, fila, columna) // Obtenemos el estado del bloque pulsado
    if (bloque != 0) { // Si el bloque pulsado no es un hueco
      bloque match { // Comprobamos si es una bomba o no
        case 'B' =>
          explotar_bomba(tablero, fila, columna) // Si es una bomba, explotamos la bomba
        case _ => // Si no
          val iguales = contar_iguales(tablero, fila, columna, Nil) // Contamos sus bloques vecinos iguales a el
          if (iguales.length > 2) { // Si se forma un grupo de 3 o más bloques iguales
            cambiar_estado_multipos(tablero, iguales, 0) // Eliminamos todos los bloques del grupo
          }
          else cambiar_estado(tablero, fila, columna, 0) // Eliminamos el bloque pulsado
      }
    } else {
      println("No hay bloque en esa posicion") // Mostramos un mensaje por pantalla para avisar al usuario
      tablero // No se producen modificaciones en el tablero
    }

  }

  /**
   * Comprueba si un tablero está vacío recursivamente.
   * @param tablero Matriz de enteros a comprobar.
   * @return True si todas las posiciones son 0, false en caso contrario.
   */
  def tablero_vacio(tablero: List[List[Int]]): Boolean = {
    def tablero_vacio_fila(fila: List[Int]): Boolean = {
      if (fila.isEmpty) true
      else if (fila.head == 0) tablero_vacio_fila(fila.tail)
      else false
    }

    if (tablero.isEmpty) true
    else tablero_vacio_fila(tablero.head) && tablero_vacio(tablero.tail)
  }

  /**
   * Actualiza la puntuación contando las diferencias en el tablero original y el tablero modificado.
   * @param t1 Tablero original.
   * @param t2 Tablero modificado.
   * @param puntuacion Puntuación original.
   * @param bloque_pulsado Posición (i,j) del bloque pulsado.
   * @return Puntuación original más 10 puntos por cada diferencia entre tableros.
   */
  def actualizar_puntuacion(t1: List[List[Int]], t2: List[List[Int]], puntuacion:Int, bloque_pulsado: List[Int]): Int = {
    /**
     * Función auxiliar para actualizar_puntuacion. Cuenta las diferencias entre el tablero original y el modificado.
     * @param t1 Tablero original.
     * @param t2 Tablero modificado.
     * @return Número de diferencias entre los tableros.
     */
    def diferencia_tableros(t1: List[List[Int]], t2: List[List[Int]]): Int = {
      if (t1.isEmpty && t2.isEmpty) 0
      else if (t1.isEmpty) t2.head.length + diferencia_tableros(t1, t2.tail)
      else if (t2.isEmpty) t1.head.length + diferencia_tableros(t1.tail, t2)
      else diferencia_listas(t1.head, t2.head) + diferencia_tableros(t1.tail, t2.tail)
    }

    /**
     * Función auxiliar para actualizar_puntuacion. Cuenta las diferencias entre dos listas.
     * @param l1 Lista 1.
     * @param l2 Lista 2.
     * @return Número de diferencias entre las listas.
     */
    def diferencia_listas(l1: List[Int], l2: List[Int]): Int = {
      if (l1.isEmpty && l2.isEmpty) 0
      else if (l1.isEmpty) l2.length
      else if (l2.isEmpty) l1.length
      else if (l1.head != l2.head) 1 + diferencia_listas(l1.tail, l2.tail)
      else diferencia_listas(l1.tail, l2.tail)
    }

    val diferentes = diferencia_tableros(t1, t2) // Obtenemos las diferencias entre tableros
    // Si hay al menos 3 diferentes o el bloque pulsado es una bomba, se actualiza la puntuación
    if (diferentes > 2 || obtener_posicion(t1, obtener_columna(bloque_pulsado, 0), obtener_columna(bloque_pulsado,1)) == 8) puntuacion + diferentes*10
    else 0 // Si no, se devuelve 0
  }

  /**
   * Desplaza los bloques que se encuentren "flotando" en el tablero hacia abajo y hacia la derecha cuando sea necesario.
   * @param tablero Tablero a recolocar.
   * @return Tablero recolocado.
   */
  def desplazar_bloques(tablero: List[List[Int]]):List[List[Int]] = {
    /**
     * Función auxiliar que desplaza las posiciones de una columna vacía del tablero a la derecha del todo.
     * @param tablero Tablero a modificar.
     * @param fila Posicion del siguiente bloque a mover.
     * @param columna Columna del tablero a modificar.
     * @param tablero_antiguo Tablero original.
     * @return Tablero modificado.
     */
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

    /**
     * Desplaza las columnas vacías hacia la derecha del tablero hasta que aplicar esta función no tenga ningún efecto.
     * @param tablero Tablero a modificar.
     * @param nuevo_tablero Tablero modificado.
     * @return Tablero modificado.
     */
    def desplazar_columna(tablero: List[List[Int]], nuevo_tablero: List[List[Int]]):List[List[Int]] = {
      if (matrices_iguales(tablero, nuevo_tablero)) nuevo_tablero // Cuando ya no pueda hacer ningún cambio, he terminado.
      else desplazar_columna_aux(tablero, 0, 0, tablero) // Desplaza la columna vacía hacia la derecha
    }

    /**
     * Función que desplaza los bloques que se encuentren "flotando" en el tablero hacia abajo.
     * @param tablero Tablero a recolocar.
     * @param fila Posicion de la fila del bloque a mover.
     * @param columna Posicion de la columna del bloque a mover
     * @return Tablero con los huecos en la parte superior de las columnas.
     */
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

    /**
     * Comprueba si una columna está vacía. Comprueba si la posición de la columna de la última fila es 0, entonces asumimos toda la columna vacío.
     * @param tablero Tablero a comprobar.
     * @param columna Posicion de la columna a comprobar.
     * @return True si la columna está vacía. False en caso contrario.
     */
    @tailrec
    def es_columna_vacia(tablero: List[List[Int]], columna:Int):Boolean = {
      if (tablero.tail == Nil) tablero.head(columna) == 0
      else es_columna_vacia(tablero.tail, columna)
    }

    /**
     * Funcion auxiliar para desplazar las filas y columnas de un tablero hacia abajo y hacia la izquierda
     * @param tablero tablero inicial
     * @param fila posicion fila a desplazar
     * @param columna posicion columna a desplazar
     * @return tablero resultante tras desplazar
     */
    def desplazar_bloques_aux(tablero: List[List[Int]], fila: Int, columna: Int):List[List[Int]] = {
      if (columna == tablero.head.length) tablero
      else {
        val tablero_bajado = desplazar_bloques_aux(tablero, fila, columna + 1)
        val tablero_desplazado = desplazar_columna(tablero_bajado, Nil)
        if (es_columna_vacia(tablero_desplazado, columna)) desplazar_fila(tablero_desplazado, 0 ,columna)
        else tablero_desplazado
      }
    }
    desplazar_bloques_aux(tablero,  0, 0)
  }

  /**
   * Comprueba si una matriz contiene una posicion concreta
   * @param matriz matriz para buscar posicion
   * @param lista lista con fila y columna para ver si existe la posicion en la matriz
   * @return devuelve true si esta la posicion y false en caso contrario
   */
  @tailrec
  def contiene(matriz:List[List[Int]], lista:List[Int]): Boolean ={
    if (matriz == Nil) false
    else if (matriz.tail == Nil) listas_iguales(matriz.head, lista)
    else listas_iguales(matriz.head, lista) || contiene(matriz.tail, lista)
  }

  /**
   * Comprueba si dos listas son iguales
   * @param l1 lista 1
   * @param l2 lista 2
   * @return booleano true si son iguales y false en caso contrario
   */
  @tailrec
  def listas_iguales(l1: List[Int], l2: List[Int]): Boolean = {
    if (l1.isEmpty) l2.isEmpty
    else if (l1.head == l2.head) listas_iguales(l1.tail, l2.tail)
    else false
  }

  /**
   * Comprueba que dos matrices (lista de listas) sean iguales.
   * @param m1 matriz 1
   * @param m2 matriz 2
   * @return booleano true si son iguales y false en caso contrario
   */
  @tailrec
  def matrices_iguales(m1:List[List[Int]], m2:List[List[Int]]): Boolean = {
    if (m1.isEmpty) m2.isEmpty
    else if (m2.isEmpty) false
    else if (m1.tail == Nil) listas_iguales(m1.head, m2.head)
    else listas_iguales(m1.head, m2.head) && matrices_iguales(m1.tail, m2.tail)
  }

  /**
   * Dada una posicion y un tablero, cuenta el numero de bloques seguidos que hay a su alrededor conectados a el sin diagonales y devuelve las coordenadas de los que sean iguales
   * @param tablero tablero actual
   * @param fila fila inicio
   * @param columna columna inicio
   * @param elementos coordenadas de los elementos iguales al bloque inicial
   * @return coordenadas de los bloques iguales encontrados
   */
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

  /**
   * Cuando se pulsa una casilla que contiene una bomba, se eliminan los bloques de las 9 casillas que la rodean
   * @param tablero tablero actual
   * @param fila fila de la bomba
   * @param columna columna de la bomba
   * @return tablero tras explotar la bomba (sin desplazar)
   */
  def explotar_bomba(tablero: List[List[Int]], fila: Int, columna: Int): List[List[Int]] = {
    val posiciones = List(
      List(fila-1, columna-1), List(fila-1, columna), List(fila-1, columna+1),
      List(fila, columna-1), List(fila,columna), List(fila, columna+1),
      List(fila+1, columna-1), List(fila+1, columna), List(fila+1, columna+1)
    )

    cambiar_estado_multipos(tablero, posiciones, 0)
  }

  /**
   * Permite mapear una lista de cadenas a lista de enteros
   * @param list lista inicial de cadenas
   * @return lista de enteros
   */
  def map_to_int(list: List[String]): List[Int] = {
    if (list == Nil) Nil
    else list.head.toInt :: map_to_int(list.tail)
  }

  /**
   * Dado un tablero, la puntuacion acumulada y las vidas restantes, se comprueba que no este vacio y las vidas no sean 0 y se solicita
   * una coordenada para el siguiente turno. Si el tablero esta vacio pero quedan vidas, se genera uno nuevo y continua la partida
   * si no quedan vidas, fin de la ejecucion
   * @param tablero tablero actual
   * @param puntuacion puntuacion
   * @param vidas vidas
   * @return lista con la puntuacion final y las vidas restantes
   */
  @tailrec
  def jugar(tablero: List[List[Int]], puntuacion: Int, vidas: Int):List[Int] = {
    if (!tablero_vacio(tablero) && vidas > 0) {
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

  /**
   * Crea una lista con los valores indicados como parametro
   * @param x parametro x
   * @param y parametro y
   * @return lista con los parametros x e y
   */
  def create_list(x: Integer, y:Integer):List[Int] = {
    List(x.intValue(), y.intValue())
  }

  /**
   * Segun el maximo numero de colores que pueda tener un tablero, se genera un numero aleatorio entre 0 y el maximo numero
   * @param n_colores maximo numero de colores que se pueden generar
   * @return numero de colores elegido aleatoriamente
   */
  def colores_tablero(n_colores:Int):Set[Int] = {
    if (n_colores == 0) Set()
    else Set(1 + Random.nextInt(7)).++(colores_tablero(n_colores-1))
  }

  /**
   * Dado un tablero y un numero de bombas, se colocan en posiciones del tablero aleatorias
   * @param tablero tablero actual
   * @param n_bombas numero de bombas a colocar en el tablero
   * @return tablero con bombas colocadas en las posiciones aleatorias que no tuviesen ya una bomba
   */
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

  /**
   * En funcion del nivel seleccionado, se genera un tablero con un determinado numero de colores, desde 1 hasta el maximo de ese nivel
   * y un numero de vidas que tambien depende del nivel.
   * @param nivel nivel seleccionado
   * @return tablero del nivel indicado
   */
  def generar_tablero(nivel: Int):List[List[Int]] = nivel match {
    case 1 =>
      val colores = colores_tablero(3)
      val tablero = generar_tablero_aleatorio(9,11, colores)
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

  /**
   * Crea una matriz vacia para la interfaz grafica de SWING porque alli no se pueden crear listas de Scala vacias
   * @return matriz vacia
   */
  def create_empty_matrix():List[List[Int]] = {
    Nil
  }

  /**
   * En caso de que las vidas se acaben, indica que ha terminado la partida. Si no, se genera el tablero y se inicia jugar.
   * @param nivel nivel de juego seleccionado
   * @param puntuacion puntuacion actual
   * @param vidas vidas actuales
   * @param partidas partidas jugadas (tableros vaciados)
   */
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

  /* *************************************************************************************
   *                                      LÓGICA IA                                      *
   ***************************************************************************************/

  /**
   * Obtiene la mejor coordenada buscando los grupos más pequeños mayores o iguales que 3.
   * @param tablero Tablero actual
   * @param mejor_grupo Grupo con mejor puntuación
   * @param mejor_pos Posicion de la mejor puntuación
   * @param fila Fila actual
   * @param columna Columna actual
   * @return mejor_pos mejor posicion de la mejor puntuación
   */
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

  /**
   * Recorre el tablero para buscar una posicion donde haya una bomba y devolver sus coordenadas
   * @param tablero tablero actual
   * @param fila fila posicion
   * @param columna columna posicion
   * @return coordenada de una bomba
   */
  @tailrec
  def buscar_bomba(tablero: List[List[Int]], fila:Int, columna: Int): List[Int] = {
    if (fila == tablero.length) coordenada_aleatoria(tablero)
    else if (columna == tablero.head.length) buscar_bomba(tablero, fila+1, 0)
    else {
      if (obtener_posicion(tablero, fila, columna) == 8) List(fila, columna)
      else buscar_bomba(tablero, fila, columna+1)
    }
  }

  /**
   * Elige una posicion permitida del tablero de forma aleatoria evitando las posiciones vacias.
   * @param tablero tablero actual
   * @return coordenada aleatoria que no esta vacia
   */
  @tailrec
  def coordenada_aleatoria(tablero:List[List[Int]]): List[Int] = {
    val fila = Random.nextInt(tablero.length)
    val columna = Random.nextInt(tablero.head.length)
    if (obtener_posicion(tablero, fila, columna) == 0) coordenada_aleatoria(tablero)
    else List(fila, columna)
  }

  /**
   * Hace una busqueda de la mejor coordenada dando un paso hacia delanete en el tablero. Recibe el tablero tras pulsar las coordenadas indicads
   * (coords_padre) y la puntuacion que se obtuvo en ese turno. Comprueba la mejor posicion que se puede pulsar en el tablero resultante
   * y devuelve la suma de ambas.
   * @param tablero tablero tras pulsar las coordenadas del padre
   * @param coords_padre coordenadas del padre
   * @param punt_padre puntuacion del padre
   * @return Mejor coordenada del tablero hijo
   */
  def mejor_coordenada_actual(tablero: List[List[Int]], coords_padre:List[Int], punt_padre:Int):List[Int] = {
    if (tablero_vacio(tablero)) coords_padre ::: List(punt_padre)
    else{
      val coord = obtener_coordenadas_bloque_IA(tablero, tablero.length*tablero.head.length, List(0,0), 0 ,0)
      val punt = contar_iguales(tablero, obtener_columna(coord, 0), obtener_columna(coord, 1), Nil).length
      if (punt <= 2) {
        val coord2 = buscar_bomba(tablero, 0, 0)
        val punt2 = contar_iguales(tablero, obtener_columna(coord2, 0), obtener_columna(coord2, 1), Nil).length

        if (punt2 <= 2) coord2 ::: List(0)
        else coord2 ::: List(punt2*10+ punt_padre)
      }
      else {
        List(obtener_columna(coord, 0), obtener_columna(coord, 1), punt*10+ punt_padre)
      }
    }
  }

  /**
   * Recibe el tablero y lo recorre de forma recursiva para obtener el resultado de pulsar cada una de las posiciones disponibles.
   * Devuelve la mejor coordenada y su puntuacion.
   * @param tablero tablero actual
   * @return Mejor coordenada encontrada
   */
  def mejor_coordenada(tablero:List[List[Int]]): List[Int] = {
    /**
     * Obtiene la coordenada con mejor puntuacion en el siguiente paso de manera recursiva.
     * @param tablero Tablero actual
     * @param fila Fila actual
     * @param columna Columna actual
     * @param mejor_grupo Mejor grupo encontrado
     * @param mejor_pos Posicion del mejor grupo
     * @return Mejor coordenada encontrada
     */
    def mejor_coordenada_aux(tablero:List[List[Int]], fila: Int, columna:Int, mejor_grupo:Int, mejor_pos:List[Int]):List[Int] = {
      if (fila == tablero.length) mejor_pos
      else if (columna == tablero.head.length) mejor_coordenada_aux(tablero, fila+1, 0, mejor_grupo, mejor_pos)
      else if (obtener_posicion(tablero, fila, columna) != 0) {
        val tablero_modificado = pulsar_bloque(tablero, fila, columna)
        val tablero_modificado1 = desplazar_bloques(tablero_modificado)
        val puntuacion = actualizar_puntuacion(tablero, tablero_modificado, 0, List(fila,columna))
        val coords = mejor_coordenada_actual(tablero_modificado1, List(fila, columna), puntuacion)
        if (obtener_columna(coords,2) >= mejor_grupo) mejor_coordenada_aux(tablero, fila, columna+1, obtener_columna(coords,2),
          List(fila, columna))
        else mejor_coordenada_aux(tablero, fila, columna+1, mejor_grupo, mejor_pos)
      } else mejor_coordenada_aux(tablero, fila, columna+1, mejor_grupo, mejor_pos)
    }

    val coords = mejor_coordenada_aux(tablero, 0, 0, 0, Nil)
    coords
  }

  /**
   * Se inicia la partida simulada para el nivel seleccionado. Mientras las vidas no se agoten durante un tablero,
   * la partida se reinicia cada vez que se vacia el tablero.
   * @param tablero tablero inicial
   * @param puntuacion puntuacion acumulada
   * @param vidas vidas actuales
   * @param nivel nivel de la partida
   * @param partidas partidas jugadas (numero de tableros vaciados)
   * @return Devuelve el tablero final cuando la partida acaba
   */
  @tailrec
  def lanzarIA(tablero: List[List[Int]], puntuacion: Int, vidas: Int, nivel:Int, partidas:Int):List[List[Int]] ={
    if (vidas == 0) {
      println("Partida finalizada con "+ puntuacion+ " puntos en "+partidas+" partidas.")
      tablero
    }else if (tablero_vacio(tablero)){
      lanzarIA(generar_tablero(nivel), puntuacion, 8, nivel, partidas+1)
    } else{
      println("Puntuacion: "+puntuacion+" Vidas: "+vidas)
      val coords = mejor_coordenada(tablero)
      val tablero_tras_pulsar = pulsar_bloque(tablero, obtener_columna(coords, 0), obtener_columna(coords, 1))
      val tablero_desplazado = desplazar_bloques(tablero_tras_pulsar)
      val puntuacion_nueva = actualizar_puntuacion(tablero, tablero_tras_pulsar, puntuacion, List(obtener_columna(coords, 0), obtener_columna(coords, 1)))
      pintar_tablero(tablero_desplazado)
      if(puntuacion_nueva == 0) lanzarIA(tablero_desplazado, puntuacion_nueva, vidas-1, nivel, partidas)
      else lanzarIA(tablero_desplazado, puntuacion_nueva, vidas, nivel, partidas)
    }
  }

  /**
   * Menu que se muestra por terminal para seleccionar el nivel de juego o jugar con la inteligencia artificial.
   */
  @tailrec
  def menu():Unit = {
    println("Elige el nivel que quieres jugar: \n\t1 - Facil, \n\t2 - Medio, \n\t3 - Dificil, \n\t4 - IA, \n\t5 - Salir \nNivel:")
    val nivel_elegido = readLine().toInt

    nivel_elegido match {
      case 1 => bucle_juego(1, 0, 8, 0)
      case 2 => bucle_juego(2, 0, 10,0)
      case 3 => bucle_juego(3, 0, 15,0)
      case 4 => lanzarIA(generar_tablero(1), 0, 8, 1, 0)
      case 5 => println("Salir.")
      case _ =>
        println("Elige un nivel válido")
        menu()
    }
  }
}
