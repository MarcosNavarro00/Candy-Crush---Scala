import java.util.Properties

object PL3{
  import scala.util.Random
  import io.AnsiColor._
  import scala.io.StdIn
  import scala.util.{Try, Success, Failure}
  import java.sql.{Connection, DriverManager}



  // ############################################################################################################################
  // ################################################### FUNCIONES Para Pedir por Pantalla ######################################
  // ############################################################################################################################

  //Funcion que pide por pantalla un numero entero, cualquier otra cosa es erronea
  def pedirEntero(): Int = {
    def pedirNumeroRecursivo(): Int = {
      Try(StdIn.readInt()) match {
        case Success(input) => input
        case Failure(_) =>
          println(" [ERROR] No has introducido un número entero válido.")
          print("Introduce de nuevo el numero esperado: ")
          pedirNumeroRecursivo()
      }
    }

    pedirNumeroRecursivo()
  }

  //Funcion que pide por pantalla el numero 1 o 2, cualquier otra cosa es erronea
  def pedirDificultad(): Int = {
    def aux(): Int = {
      Try(StdIn.readInt()) match {
        case Success(num) if num == 1 || num == 2 =>
          num
        case Success(_) =>
          println(" [ERROR] El número debe ser 1 o 2. Inténtalo de nuevo.")
          aux()
        case _ =>
          println(" [ERROR] No has introducido un número válido. Inténtalo de nuevo.")
          print("Introduce de nuevo el numero esperado: ")
          aux()
      }
    }

    aux()
  }

  //Funcion que pide por pantalla el numero 1 o 2, cualquier otra cosa es erronea
  def pedirModo(): Int = {
    def aux(): Int = {
      Try(StdIn.readInt()) match {
        case Success(num) if num == 0 || num == 1 =>
          num
        case Success(_) =>
          println(" [ERROR] El número debe ser 0 o 1. Inténtalo de nuevo.")
          aux()
        case _ =>
          println(" [ERROR] No has introducido un número válido. Inténtalo de nuevo.")
          print("Introduce de nuevo el numero esperado: ")
          aux()
      }
    }

    aux()
  }


  // ############################################################################################################################
  // ################################################### FUNCIONES DE LISTAS ####################################################
  // ############################################################################################################################
  //Devuelve Ture si una lista esta vacia, si no, devuelve false
  def listaVacia(lista: List[Any]): Boolean = lista match {
    case Nil => true
    case _ => false
  }

  //Funcion para concatenar 2 listas de forma recursiva
  def concatenar_listas(lista1: List[Int], lista2: List[Int]): List[Int] = {
    if (listaVacia(lista1)) lista2
    else lista1.head :: concatenar_listas(lista1.tail, lista2)
  }

  //Devuelve la lista con los primeros n elementos
  def toma(n: Int, l: List[Int]): List[Int] = {
    if (n <= 0 || listaVacia(l)) List()
    else l.head :: toma(n - 1, l.tail)
  }

  //Devuelve la lista sin los primeros n elementos
  def deja(n: Int, l: List[Int]): List[Int] = {
    if (n <= 0 || listaVacia(l)) l
    else deja(n - 1, l.tail)
  }

  //Devuelve la longitud de una lista
  def longitudLista(lista: List[Int], contador: Int = 0): Int = {
    lista match {
      case Nil => contador
      case _ :: tail => longitudLista(tail, contador + 1)
    }
  }

  //Devuelve si una lista contiene un valor dado
  def listaContains(lista: List[Int], valor: Int): Boolean = lista match {
    case Nil => false
    case x :: xs => if (x == valor) true else listaContains(xs, valor)
  }

  //Funcion que devuelve la lista al reves
  def reverseList(lista: List[Int]): List[Int] = {
    lista match {
      case Nil => Nil
      case cabeza :: resto => concatenar_listas(reverseList(lista.tail), List(lista.head))
    }
  }

  //Elimina un numero de una lista si existe
  def eliminarNumeroLista(lista: List[Int], num: Int): List[Int] = lista match {
    case Nil => Nil
    case head :: tail => if (head == num) eliminarNumeroLista(tail, num) else head :: eliminarNumeroLista(tail, num)
  }

  //Ordena una lista de menor a mayo
  def ordenarListaMenorMayor(lista: List[Int]): List[Int] = {
    def insertar(x: Int, lista: List[Int]): List[Int] = {
      lista match {
        case Nil => List(x)
        case y :: ys =>
          if (x < y) x :: lista
          else y :: insertar(x, ys)
      }
    }

    lista match {
      case Nil => Nil
      case x :: xs => insertar(x, ordenarListaMenorMayor(xs))
    }
  }
  //Coge la elementos de la matriz en base al indice
  def getElem(index: Int, matriz: List[Int]): Int = {
    if (index == 0) toma(1, matriz).head
    else getElem(index - 1, deja(1, matriz))
  }
  //Coge la fila entera de la matriz en base al indice
  def getFila(fila: Int, columna: Int, matriz: List[Int]): List[Int] = {
    if (fila == 0) toma(columna, matriz)
    else getFila(fila - 1, columna, deja(columna, matriz))
  }

  //Coge la columna entera de la matriz en base al indice
  def getColumna(columna: Int, matriz: List[Int], filas: Int, columnas: Int): List[Int] = {
    if (columna >= filas * columnas) List()
    else getElem(columna, matriz) :: getColumna(columna + columnas, matriz, filas, columnas)
  }
  // ############################################################################################################################


  // ############################################################################################################################
  // ################################################### FUNCIONES DE MATRICES ##################################################
  // ############################################################################################################################
  //Funcion para crear la matriz inicial en base a las dimensiones del usuario
  def crear_matriz(size: Int, dificultad: Int): List[Int] = {
    val random = new Random()
    if (size <= 0) List()
    else {
      if (dificultad == 1) concatenar_listas(crear_matriz(size - 1, 1), List(1 + random.nextInt(4)))
      else concatenar_listas(crear_matriz(size - 1, 2), List(1 + random.nextInt(6)))
    }
  }

  //Funcion que imprime la matriz
  def imprimirMatriz(l: List[Int], columnas: Int, filas: Int): Unit = {
    def printRow(row: List[Int]): Unit = row match {
      case Nil => println("|")
      case h :: tail if h == -3 => print(s"|${CYAN_B}R${RESET}"); printRow(tail)
      case h :: tail if h == -2 => print(s"|${RED_B}T${RESET}"); printRow(tail)
      case h :: tail if h == -1 => print(s"|${BLACK_B}B${RESET}"); printRow(tail)
      case h :: tail if h == 1 => print(s"|${BLUE}1${RESET}"); printRow(tail)
      case h :: tail if h == 2 => print(s"|${RED}2${RESET}"); printRow(tail)
      case h :: tail if h == 3 => print(s"|${BLACK}3${RESET}"); printRow(tail)
      case h :: tail if h == 4 => print(s"|${GREEN}4${RESET}"); printRow(tail)
      case h :: tail if h == 5 => print(s"|${MAGENTA}5${RESET}"); printRow(tail)
      case h :: tail if h == 6 => print(s"|${YELLOW}6${RESET}"); printRow(tail)
      case h :: tail => print(s"|$h"); printRow(tail)
    }

    l match {
      case Nil => println("")
      case _ if longitudLista(l) % columnas != 0 => println("Error: la longitud de la lista.")
      case _ => printRow(l.take(columnas)); imprimirMatriz(l.drop(columnas), columnas, filas)
    }
  }

  //Cambia el valor de una lista (matriz) en funcion de su indice y el nuevo valor
  def cambiardValorMatriz(matriz: List[Int], indice: Int, valor_nuevo: Int): List[Int] = {
    matriz match {
      case Nil => Nil
      case cabeza :: resto =>
        if (indice == 0) valor_nuevo :: resto
        else cabeza :: cambiardValorMatriz(resto, indice - 1, valor_nuevo)
    }
  }

  //Funcion que se encarga de gestionar la gravedad de la matriz (lista) con el check value "69"
  def gravedadMatriz(matriz: List[Int], tamano: Int, filas: Int, columnas: Int, dificultad: Int): List[Int] = {
    def obtenerIndices69(lista: List[Int]): List[Int] = {
      def obtenerIndices69Rec(lista: List[Int], indice: Int, acum: List[Int]): List[Int] = {
        if (listaVacia(lista)) acum
        else if (lista.head == 69) obtenerIndices69Rec(lista.tail, indice + 1, acum :+ indice)
        else obtenerIndices69Rec(lista.tail, indice + 1, acum)
      }

      obtenerIndices69Rec(lista, 0, List())
    }

    def indicesHastaCero(idx: Int, colum: Int): List[Int] = {
      if (idx < 0) Nil
      else idx :: indicesHastaCero(idx - colum, colum)
    }

    def sustituirValoresLista(lista_matriz: List[Int], lista_indices: List[Int]): List[Int] = {
      val random = new Random()
      val nivel = if (dificultad == 1) 4 else 6
      reverseList(lista_indices) match {
        case Nil => lista_matriz
        case cabeza :: resto =>
          if (cabeza < columnas) {
            val lista_aux = cambiardValorMatriz(lista_matriz, cabeza, (1 + random.nextInt(nivel)))
            sustituirValoresLista(lista_aux, reverseList(resto))
          }
          else {
            val lista_aux = cambiardValorMatriz(lista_matriz, cabeza, lista_matriz(cabeza - columnas))
            sustituirValoresLista(lista_aux, reverseList(resto))
          }
      }
    }

    val indices69 = obtenerIndices69(matriz)
    indices69 match {
      case Nil => matriz
      case cabeza :: resto =>
        val valor_inicial = reverseList(indices69).head
        val indices_cambiar = reverseList(indicesHastaCero(valor_inicial, columnas))
        val nueva_lista = sustituirValoresLista(matriz, indices_cambiar)
        gravedadMatriz(nueva_lista, tamano, filas, columnas, dificultad)
    }
  }

  //Cambia el valor de una lista (matriz) en funcion de los indices de una lista y el checkvalue "69"
  def listaACheckValueMatriz(matriz: List[Int], indices: List[Int], idx: Int = 0): List[Int] = {
    matriz match {
      case Nil => Nil
      case cabeza :: resto =>
        if (listaContains(indices, idx)) 69 :: listaACheckValueMatriz(resto, indices, idx + 1)
        else cabeza :: listaACheckValueMatriz(resto, indices, idx + 1)
    }
  }
  // ############################################################################################################################


  // ############################################################################################################################
  // #################################################### FUNCIONES DE JUEGO ####################################################
  // ############################################################################################################################
  //Funcion que aplica la gravedad sin tener que usar dos funciones distintas
  def gravedadTotal(matriz: List[Int], indices: List[Int], filas: Int, columnas: Int, dificultad: Int): List[Int] = {
    val tamano = (filas * columnas) - 1
    val matriz_aux = listaACheckValueMatriz(matriz, indices)
    return gravedadMatriz(matriz_aux, tamano, filas, columnas, dificultad)
  }

  //Comprueba si hay dos numeros iguales conlindante. RECORDAR QUE LAS COORDENADAS EMPIEZAN EN [0,0]
  def comprobarPares(matriz: List[Int], fila_examinar: Int, columna_examinar: Int, filas: Int, columnas: Int, dificultad: Int): (List[Int], Int) = {
    val tamano_matriz = filas * columnas
    val indice = (fila_examinar * columnas) + columna_examinar
    //Comprueba dobles arriba y si los hay, los cambia por "69" y luego aplica gravedad
    if (indice - columnas >= 0 && matriz(indice) == matriz(indice - columnas)) {
      val matriz_aux = cambiardValorMatriz(cambiardValorMatriz(matriz, indice, 69), indice - columnas, 69)
      val matriz_aux2 = gravedadMatriz(matriz_aux, filas * columnas - 1, filas, columnas, dificultad)
      return (matriz_aux2, 1)
    }
    //Comprueba dobles abajo y si los hay, los cambia por "69" y luego aplica gravedad
    else if (indice + columnas < tamano_matriz && matriz(indice) == matriz(indice + columnas)) {
      val matriz_aux = cambiardValorMatriz(cambiardValorMatriz(matriz, indice, 69), indice + columnas, 69)
      val matriz_aux2 = gravedadMatriz(matriz_aux, filas * columnas - 1, filas, columnas, dificultad)
      return (matriz_aux2, 1)
    }
    //Comprueba dobles a la derecha y si los hay, los cambia por "69" y luego aplica gravedad
    else if (indice + 1 < indice - columna_examinar + columnas && matriz(indice) == matriz(indice + 1)) {
      val matriz_aux = cambiardValorMatriz(cambiardValorMatriz(matriz, indice, 69), indice + 1, 69)
      val matriz_aux2 = gravedadMatriz(matriz_aux, filas * columnas - 1, filas, columnas, dificultad)
      return (matriz_aux2, 1)
    }
    else if (indice - 1 >= indice - columna_examinar && matriz(indice) == matriz(indice - 1)) {
      val matriz_aux = cambiardValorMatriz(cambiardValorMatriz(matriz, indice, 69), indice - 1, 69)
      val matriz_aux2 = gravedadMatriz(matriz_aux, filas * columnas - 1, filas, columnas, dificultad)
      return (matriz_aux2, 1)
    }
    else return (matriz, 0)
  }

  //Comprueba ROMPECABEZAS, es decir, si hay 7 numero iguales colindantes. RECORDAR QUE LAS COORDENADAS EMPIEZAN EN [0,0]
  def comprobarRompecabezas(matriz: List[Int], fila_examinar: Int, columna_examinar: Int, filas: Int, columnas: Int, dificultad: Int): (List[Int], Int) = {
    def obtenerFila(lista: List[Int], fl: Int): List[Int] = {
      if (fl <= 0) toma(columnas, lista)
      else obtenerFila(deja(columnas, lista), fl - 1)
    }

    def obtenerColumna(lista: List[Int], cl: Int): List[Int] = {
      val tamano_fixed = filas * columnas - 1

      def obtenerIndices(n: Int, b: Int, c: Int, valores: List[Int] = Nil): List[Int] = {
        if (n > c) {
          reverseList(valores)
        } else {
          val valor = n
          obtenerIndices(n + b, b, c, valor :: valores)
        }
      }

      val lista_indices = obtenerIndices(columna_examinar, columnas, tamano_fixed)

      def obtener_valores_indices(Lista_Indices: List[Int], Lista_Base: List[Int]): List[Int] = {
        Lista_Indices match {
          case Nil => Nil
          case cabeza :: resto =>
            Lista_Base(cabeza) :: obtener_valores_indices(resto, Lista_Base)
        }
      }

      obtener_valores_indices(lista_indices, lista)
    }

    val fila_examinada = obtenerFila(matriz, fila_examinar)
    val columnas_examinada = obtenerColumna(matriz, columna_examinar)

    def encontrarSeries(lista: List[Int], inicial: Int, indice: Int = 0): List[Int] = {
      if (indice == 0) {
        if (inicial - 1 >= 0 && inicial + 1 < longitudLista(lista)) {
          try {
            if (lista(inicial) != lista(inicial + 1) && lista(inicial) != lista(inicial - 1)) return List()
          } catch {
            case e: IndexOutOfBoundsException =>
          }
        }
        else {
          try {
            if (inicial - 1 >= 0) {
              if (lista(inicial) != lista(inicial - 1)) return List()
            }
            else if (inicial + 1 < longitudLista(lista)) {
              if (lista(inicial) != lista(inicial + 1)) return List()
            }
          } catch {
            case e: IndexOutOfBoundsException =>
          }
        }
      }

      lista match {
        case x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: _ if x1 == x2 && x2 == x3 && x3 == x4 && x4 == x5 && x5 == x6 && x6 == x7 => List(indice)
        case x :: xs => encontrarSeries(xs, inicial, indice + 1)
        case Nil => List()
      }
    }

    val serie_fila = encontrarSeries(fila_examinada, columna_examinar)
    val columna_fila = encontrarSeries(columnas_examinada, fila_examinar)
    val idx = (fila_examinar * columnas) + columna_examinar

    def crearListaIndices2(lista: List[Int], contador: Int = 0, longitud: Int = 7): List[Int] = {
      if (longitud == 0) {
        Nil
      } else {
        (lista.head + contador) :: crearListaIndices2(lista, contador + 1, longitud - 1)
      }
    }

    def sumaListaColumnas(lista: List[Int], n: Int, exm: Int): List[Int] = {
      lista match {
        case Nil => Nil
        case h :: hs if h == 0 => exm :: sumaListaColumnas(hs, n, exm)
        case _ => ((lista.head * n) + exm) :: sumaListaColumnas(lista.tail, n, exm)
      }
    }

    def sumaListaFilas(lista: List[Int], n: Int): List[Int] = {
      lista match {
        case Nil => Nil
        case _ => (lista.head + n) :: sumaListaFilas(lista.tail, n)
      }
    }

    serie_fila match {
      case Nil =>
        columna_fila match {
          case Nil => return (matriz, 0)
          case _ => val mt_aux1 = crearListaIndices2(columna_fila);
            val mt_aux2 = sumaListaColumnas(mt_aux1, columnas, columna_examinar);
            val mt_aux3 = cambiardValorMatriz(matriz, idx, -3);
            val mt_aux4 = eliminarNumeroLista(mt_aux2, idx)
            val mt_aux5 = gravedadTotal(mt_aux3, mt_aux4, filas, columnas, dificultad)
            return (mt_aux5, 1)
        }
      case _ => val indice_fila = (fila_examinar * columnas);
        val mt_aux1 = crearListaIndices2(serie_fila);
        val mt_aux2 = sumaListaFilas(mt_aux1, indice_fila);
        val mt_aux3 = cambiardValorMatriz(matriz, idx, -3);
        val mt_aux4 = eliminarNumeroLista(mt_aux2, idx)
        val mt_aux5 = gravedadTotal(mt_aux3, mt_aux4, filas, columnas, dificultad)
        return (mt_aux5, 1)
    }
  }

  //Comprueba si hay un rompecabezas (-3) en la coordenadas especificadas, en el caso afirmativo, lo explota
  def explotarRompecabezas(matriz: List[Int], fila_examinar: Int, columna_examinar: Int, filas: Int, columnas: Int, dificultad: Int): (List[Int], Int, Int) = {
    def encontrarIndicesRompecabezas(lista: List[Int], n: Int, indice: Int = 0, res: List[Int] = List()): List[Int] = {
      lista match {
        case Nil => reverseList(res)
        case head :: tail => {
          if (head == n) encontrarIndicesRompecabezas(tail, n, indice + 1, indice :: res)
          else encontrarIndicesRompecabezas(tail, n, indice + 1, res)
        }
      }
    }

    val random = new Random()
    val idx = (fila_examinar * columnas) + columna_examinar
    if (matriz(idx) == -3) {
      if (dificultad == 1) {
        val numero_explotar = 1 + random.nextInt(4)
        print("Rompecabezas encontrado en la posicion examinada\n")
        print("Numero aleatorio a explotar: " + numero_explotar + "\n")
        val list_idx = concatenar_listas(encontrarIndicesRompecabezas(matriz, numero_explotar), List(idx))
        return (gravedadTotal(matriz, list_idx, filas, columnas, dificultad), 1, longitudLista(list_idx))
      }
      else {
        val numero_explotar = 1 + random.nextInt(6)
        print("Rompecabezas encontrado en la posicion examinada\n")
        print("Numero aleatorio a explotar: " + numero_explotar + "\n")
        val list_idx = concatenar_listas(encontrarIndicesRompecabezas(matriz, numero_explotar), List(idx))
        return (gravedadTotal(matriz, list_idx, filas, columnas, dificultad), 1, longitudLista(list_idx))
      }
    }
    else return (matriz, 0, 0)
  }

  //Al explotar la bomba, cmabia todos los valores de la fila por 69 y aplica la gravedad
  def canbiarValoresFila(matriz: List[Int], indice: Int, columnas: Int): List[Int] = {
    if (columnas == 0) {
      matriz
    } else {
      val matriz_aux = cambiardValorMatriz(matriz, indice + columnas - 1, 69)
      canbiarValoresFila(matriz_aux, indice, columnas - 1)
    }
  }

  //Al explotar la bomba, cmabia todos los valores de la columna por 69 y aplica la gravedad
  def canbiarValoresColumna(matriz: List[Int], indice: Int, fila: Int, columna: Int): List[Int] = {
    if (fila == 0) {
      matriz
    } else {
      val matriz_aux = cambiardValorMatriz(matriz, indice + (columna * (fila - 1)), 69)
      canbiarValoresColumna(matriz_aux, indice, fila - 1, columna)
    }
  }

  //Funcion que encuentre si hay posiciones adyacentes que tienen el mismo valor, si es asi, devuelve las posiciones
  def adyacentes(lista: List[Int], valor: Int, count: Int, count_aux: Int, adyacencias: Int, valorPosicion: Int, posiciones: List[Int]): (List[Int], Int) = {

    if (count == adyacencias && longitudLista(posiciones) == adyacencias && listaContains(posiciones, valorPosicion) == true) {
      (posiciones, 1)
    } else if (longitudLista(lista) > 0) {
      if (valor == lista.head) {
        val posiciones_aux = count_aux :: posiciones
        adyacentes(lista.tail, valor, count + 1, count_aux + 1, adyacencias, valorPosicion, posiciones_aux)
      } else {
        adyacentes(lista.tail, valor, 0, count_aux + 1, adyacencias, valorPosicion, List())
      }
    } else {
      (List(), 0)
    }
  }

  //Funcion que calcula la posicion mas a la IZQ que puede llegar la explosion del TNT
  def calcularPosIzq(indice: Int, columna: Int, count: Int): Int = {
    //println(s"El valor del indice $indice")
    val fila_actual = indice / columna
    if (indice == 0 || count == 4 || fila_actual != (indice - 1) / columna) {
      indice
    } else {
      calcularPosIzq(indice - 1, columna, count + 1)
    }
  }

  //Funcion que calcula la posicion mas a la DERE que puede llegar la explosion del TNT
  def calcularPosDere(indice: Int, columna: Int, tamaño: Int, count: Int): Int = {
    //println(s"El valor del indice $indice")
    val fila_actual = indice / columna

    if (indice == 0 || count == 4 || indice == tamaño || fila_actual != (indice + 1) / columna) {
      indice
    } else {
      calcularPosDere(indice + 1, columna, tamaño, count + 1)
    }
  }

  //Funcion que calcula la posicion mas ARRIBA que puede llegar la explosion del TNT
  def calcularPosArriba(columnas: Int, indice: Int, count: Int): Int = {
    //println(s"El valor del indice $indice")
    if (count == 4 || (indice - columnas) - 1 <= 0) {
      indice
    } else {
      calcularPosArriba(columnas, indice - columnas, count + 1)
    }
  }

  //Funcion que calcula la posicion mas ABAJO que puede llegar la explosion del TNT
  def calcularPosAbajo(columnas: Int, tamaño: Int, indice: Int, count: Int): Int = {
    //println(s"El valor del indice $indice")
    if (count == 4 || tamaño <= indice + columnas) {
      indice
    } else {
      calcularPosAbajo(columnas, tamaño, indice + columnas, count + 1)
    }
  }

  //Funcion que pone 69 en la posiciones inidicadas en la fila
  def poner69fila(matriz: List[Int], filas: Int, columnas: Int, indice: Int, columna_dere: Int, count: Int): List[Int] = {
    if (count == columna_dere) {
      matriz
    } else {
      val matriz_aux = cambiardValorMatriz(matriz, indice, 69)
      poner69fila(matriz_aux, filas, columnas, indice + 1, columna_dere, count + 1)
    }


  }

  //Funcion que pone 69 en la posiciones inidicadas en forma de columna y llama a poner69fila que las pone en forma de fila
  def poner69(matriz: List[Int], filas: Int, columnas: Int, indice: Int, fila_final: Int, columna_dere: Int, count: Int, count2: Int): List[Int] = {
    if (count == fila_final) {
      matriz
    } else {
      val matriz_aux = poner69fila(matriz, filas, columnas, indice, columna_dere, count2)
      poner69(matriz_aux, filas, columnas, indice + columnas, fila_final, columna_dere, count + 1, count2)

    }

  }

  //Funcion que explota el TNT con un radio de 4 en todas las dirreciones
  def explotarTNT(matriz: List[Int], fila_examinar: Int, columna_examinar: Int, filas: Int, columnas: Int, dificultad: Int): (List[Int], Int, Int) = {
    val tamano_matriz = filas * columnas
    val indice = (fila_examinar * columnas) + columna_examinar
    val valor = matriz(indice)
    //println(s"indice $indice")
    //Se comprueba si el valor es una bomba
    if (valor == -2) {
      //Se calcula hasta donde llega la explosion arriba
      val indice_arriba = calcularPosArriba(columnas, indice, 0)
      //println(s"indice_arriba $indice_arriba")
      //Se calcula cual es el elemento arriba a la izquierda en llegar
      val indice_cominezo = calcularPosIzq(indice_arriba, columnas, 0)
      //println(s"indice_cominezo $indice_cominezo")

      //Se calcula cual es el elemento abajo a la derecha en llegar
      val indice_abajo = calcularPosAbajo(columnas, tamano_matriz, indice, 0)
      val indice_final = calcularPosDere(indice_abajo, columnas, tamano_matriz - 1, 0)

      //println(s"indice_final  $indice_final")
      val fila_final = (indice_final / columnas) + 1
      //println(s"Fila final $fila_final")

      val columna_final = (indice_final % columnas) + 1
      //println(s" columna_final $columna_final")
      //Se calcula cuantas filas y columnas afecta las explosion
      val filas_recorrer = fila_final - (indice_cominezo / columnas)
      val columnas_recorrer = columna_final - (indice_cominezo % columnas)

      //println(s" filas_recorrer $filas_recorrer")
      //println(s" columnas_recorrer $columnas_recorrer")

      val matriz_aux = poner69(matriz, filas, columnas, indice_cominezo, filas_recorrer, columnas_recorrer, 0, 0)
      val matriz_aux2 = gravedadMatriz(matriz_aux, filas * columnas - 1, filas, columnas, dificultad)
      (matriz_aux2, 1, filas_recorrer*columnas_recorrer)
    } else {
      (matriz, 0, 0)

    }
  }


  //Funcion que comprueba si hay 6 posiciones adyacentes con el mismo valor, si es asi, pone -2 y aplica gravedad
  def ponerTNT(matriz: List[Int], fila_examinar: Int, columna_examinar: Int, filas: Int, columnas: Int, dificultad: Int): (List[Int], Int) = {
    val tamano_matriz = filas * columnas
    val indice = (fila_examinar * columnas) + columna_examinar
    val valor = matriz(indice)

    //Se obtiene la fila completa que se quiere examinar
    val fila_comprobar = getFila(fila_examinar, columnas, matriz)

    //Se obtiene la columna completa que se quiere examinar
    val columna_comprobar = getColumna(columna_examinar, matriz, filas, columnas)
    //println(columna_comprobar)

    //Se comprueba si hay 5 iguales en una fila o columna
    if (adyacentes(fila_comprobar, valor, 0, 0, 6, columna_examinar, List())._2 == 1) {
      //Se obtiene en que posiciones estan los 5 adyacentes
      val posiciones = adyacentes(fila_comprobar, valor, 0, 0, 6, columna_examinar, List())._1
      val posiciones2 = reverseList(posiciones)
      //Se elimina la coordenada en la que se va a poner la bomba
      val posicion3 = posiciones2.filterNot(_ == indice % columnas)

      val matriz_aux = cambiardValorMatriz(cambiardValorMatriz(cambiardValorMatriz(cambiardValorMatriz(cambiardValorMatriz(matriz, posicion3(0) + (columnas * fila_examinar), 69), posicion3(1) + (columnas * fila_examinar), 69), posicion3(2) + (columnas * fila_examinar), 69), posicion3(3) + (columnas * fila_examinar), 69), posicion3(4) + (columnas * fila_examinar), 69)
      val matriz_aux2 = gravedadMatriz(matriz_aux, filas * columnas - 1, filas, columnas, dificultad)
      val matriz_aux3 = cambiardValorMatriz(matriz_aux2, indice, -2)
      (matriz_aux3, 1)
    } else if (adyacentes(columna_comprobar, valor, 0, 0, 6, fila_examinar, List())._2 == 1) {
      //Se obtiene en que posiciones estan los 5 adyacentes
      val posiciones = adyacentes(columna_comprobar, valor, 0, 0, 6, fila_examinar, List())._1
      val posiciones2 = reverseList(posiciones)

      val matriz_aux = cambiardValorMatriz(cambiardValorMatriz(cambiardValorMatriz(cambiardValorMatriz(cambiardValorMatriz(matriz, posiciones2(0) * columnas + columna_examinar, 69), posiciones2(1) * columnas + columna_examinar, 69), posiciones2(2) * columnas + columna_examinar, 69), posiciones2(3) * columnas + columna_examinar, 69), posiciones2(4) * columnas + columna_examinar, 69)
      val matriz_aux2 = gravedadMatriz(matriz_aux, filas * columnas - 1, filas, columnas, dificultad)
      val matriz_aux3 = cambiardValorMatriz(matriz_aux2, posiciones2(5) * columnas + columna_examinar, -2)
      (matriz_aux3, 1)
    } else {
      //println("No encontro")
      (matriz, 0)
    }
  }


  //Explota la bomba generando una nueva fila o columna
  def explotarBomba(matriz: List[Int], fila_examinar: Int, columna_examinar: Int, filas: Int, columnas: Int, dificultad: Int): (List[Int], Int, Int) = {
    val tamano_matriz = filas * columnas
    val indice = (fila_examinar * columnas) + columna_examinar
    val valor = matriz(indice)

    //obtengo el primer elemento de la fila
    val indice_fila_comienzo = indice - columna_examinar
    //obtengo el primer valor de la columna
    val indice_columna_comienzo = indice % columnas

    if (valor == -1) {
      //val randomInt = Random.nextInt(2)
      val randomInt = 1
      //Se elimina la fila
      if (randomInt == 0) {
        println("\n ---Se Explota una Fila---")
        val matriz_aux = canbiarValoresFila(matriz, indice_fila_comienzo, columnas)
        val matriz_aux2 = gravedadMatriz(matriz_aux, filas * columnas - 1, filas, columnas, dificultad)
        (matriz_aux2, 1, filas) //Explota tantos bloques como filas
      } //Se elimina una columna
      else {
        println("\n ---Se Explota una Columna---")
        val matriz_aux = canbiarValoresColumna(matriz, indice_columna_comienzo, filas, columnas)
        val matriz_aux2 = gravedadMatriz(matriz_aux, filas * columnas - 1, filas, columnas, dificultad)
        (matriz_aux2, 1, columnas) //Explota tantos bloques como columnas
      }
    } else {
      (matriz, 0, 0)
    }

  }

  //Pone una bomba en la matriz en caso de encontrarlar
  def ponerBomba2(matriz: List[Int], fila_examinar: Int, columna_examinar: Int, filas: Int, columnas: Int, dificultad: Int): (List[Int], Int) = {
    val tamano_matriz = filas * columnas
    val indice = (fila_examinar * columnas) + columna_examinar
    val valor = matriz(indice)

    //Se obtiene la fila completa que se quiere examinar
    val fila_comprobar = getFila(fila_examinar, columnas, matriz)

    //Se obtiene la columna completa que se quiere examinar
    val columna_comprobar = getColumna(columna_examinar, matriz, filas, columnas)
    //println(columna_comprobar)

    //Se comprueba si hay 5 iguales en una fila o columna
    if (adyacentes(fila_comprobar, valor, 0, 0, 5, columna_examinar, List())._2 == 1) {
      //Se obtiene en que posiciones estan los 5 adyacentes
      val posiciones = adyacentes(fila_comprobar, valor, 0, 0, 5, columna_examinar, List())._1
      val posiciones2 = reverseList(posiciones)
      //Se elimina la coordenada en la que se va a poner la bomba
      val posicion3 = posiciones2.filterNot(_ == indice % columnas)

      val matriz_aux = cambiardValorMatriz(cambiardValorMatriz(cambiardValorMatriz(cambiardValorMatriz(matriz, posicion3(0) + (columnas * fila_examinar), 69), posicion3(1) + (columnas * fila_examinar), 69), posicion3(2) + (columnas * fila_examinar), 69), posicion3(3) + (columnas * fila_examinar), 69)
      val matriz_aux2 = gravedadMatriz(matriz_aux, filas * columnas - 1, filas, columnas, dificultad)
      val matriz_aux3 = cambiardValorMatriz(matriz_aux2, indice, -1)
      (matriz_aux3, 1)
    } else if (adyacentes(columna_comprobar, valor, 0, 0, 5, fila_examinar, List())._2 == 1) {
      //Se obtiene en que posiciones estan los 5 adyacentes
      val posiciones = adyacentes(columna_comprobar, valor, 0, 0, 5, fila_examinar, List())._1
      val posiciones2 = reverseList(posiciones)
      //println(posiciones2)

      val matriz_aux = cambiardValorMatriz(cambiardValorMatriz(cambiardValorMatriz(cambiardValorMatriz(matriz, posiciones2(0) * columnas + columna_examinar, 69), posiciones2(1) * columnas + columna_examinar, 69), posiciones2(2) * columnas + columna_examinar, 69), posiciones2(3) * columnas + columna_examinar, 69)
      val matriz_aux2 = gravedadMatriz(matriz_aux, filas * columnas - 1, filas, columnas, dificultad)
      val matriz_aux3 = cambiardValorMatriz(matriz_aux2, posiciones2(4) * columnas + columna_examinar, -1)
      (matriz_aux3, 1)
    } else {
      //println("No encontro")
      (matriz, 0)
    }
  }

  def Optimizado(matriz: List[Int], filas: Int, columnas: Int, dificultad: Int): (List[Int], Int, Int) = {
    val tam = filas * columnas - 1
    val random = new Random()

    def examinar_idx(indice: Int = 0, mejor_idx: Int, accion: Int, mejor_matriz: List[Int], blqs_rts: Int): (List[Int], Int, Int) = {
      def obtenerFilaYColumna(idx: Int): (Int, Int) = {
        val cl = idx % columnas
        val fl = math.floor(idx / columnas).toInt
        return (cl, fl)
      }

      if (indice > tam) {
        if (accion == 0) return (matriz, 0, 0)
        else {
          val (id_aux1, id_aux2) = obtenerFilaYColumna(mejor_idx)
          if (accion == 1) {
            print("\n [ACCION]: PARES ENCONTRADOS EN: f" + id_aux2 + " c" + id_aux1 + "\n\n"); return (mejor_matriz, 1, 2)
          } //2 bloques rotos = 2 pts
          else if (accion == 2) {
            print("\n [ACCION]: BOMBA PUESTA EN: f" + id_aux2 + " c" + id_aux1 + "\n\n"); return (mejor_matriz, 1, 5 + 5)
          } //5 bloques rotos = 5pts + seleccionar bomba (5 pts)
          else if (accion == 3) {
            print("\n [ACCION]: BOMBA EXPLOTADA EN: f" + id_aux2 + " c" + id_aux1 + "\n\n"); return (mejor_matriz, 1, blqs_rts)
          } //x bloques rotos = x pts + bonus
          else if (accion == 4) {
            print("\n [ACCION]: TNT PUESTA EN: f" + id_aux2 + " c" + id_aux1 + "\n\n"); return (mejor_matriz, 1, 6 + 10)
          } //6 bloques rotos = 6pts + seleccionar TNT (10 pts)
          else if (accion == 5) {
            print("\n [ACCION]: TNT EXPLOTADA EN: f" + id_aux2 + " c" + id_aux1 + "\n\n"); return (mejor_matriz, 1, blqs_rts)
          } //x bloques rotos = x pts + bonus
          else if (accion == 6) {
            print("\n [ACCION]: ROMPECABEZAS PUESTO EN: f" + id_aux2 + " c" + id_aux1 + "\n\n"); return (mejor_matriz, 1, 7 + 15)
          } //7 bloques rotos = 7pts + seleccionar Rompecabezas (15 pts)
          else (mejor_matriz, 0, 0) //A
        }
      }
      else {
        //Acciones: 0 - Nada; 1 - Pares; 2: Poner Bomba; 3 - Explotar Bomba; 4 - Poner TNT; 5 - Explotar TNT; 6 - Poner Rompecabezas; 7 - Explotar Rompecabezas
        val (c, f) = obtenerFilaYColumna(indice)
        val (rompecabezas_exp_lista, rompecabezas_exp_true, rotos) = explotarRompecabezas(matriz, f, c, filas, columnas, dificultad)
        if (rompecabezas_exp_true == 1) {
          print("\n [ACCION]: ROMPECABEZAS EXPLOTADO EN: " + f + " " + c + "\n\n"); return (rompecabezas_exp_lista, 1, (rotos + (rotos / 10).floor).toInt)
        } //Devuelve los bloques rotos + bonus 1 pt extra por 10 explotados
        else {
          val (rompecabezas_aux_lista, rompecabezas_aux_true) = comprobarRompecabezas(matriz, f, c, filas, columnas, dificultad)
          if (rompecabezas_aux_true == 1) examinar_idx(indice + 1, indice, 6, rompecabezas_aux_lista, blqs_rts)
          else {
            val (tnt_exp_lista, tnt_exp_true, rotos_TNT) = explotarTNT(matriz, f, c, filas, columnas, dificultad)
            if (tnt_exp_true == 1 && accion < 6) examinar_idx(indice + 1, indice, 5, tnt_exp_lista, (rotos_TNT + (rotos_TNT / 10).floor).toInt) //Devuelve los bloques rotos + bonus 1 pt extra por 10 explotados
            else {
              val (tnt_aux_lista, tnt_aux_true) = ponerTNT(matriz, f, c, filas, columnas, dificultad)
              if (tnt_aux_true == 1 && accion < 5) examinar_idx(indice + 1, indice, 4, tnt_aux_lista, blqs_rts)
              else {
                val (bomba_exp_lista, bomba_exp_true, rotos_bomba) = explotarBomba(matriz, f, c, filas, columnas, dificultad)
                if (bomba_exp_true == 1 && accion < 4) examinar_idx(indice + 1, indice, 3, bomba_exp_lista, (rotos_bomba + (rotos_bomba / 10).floor).toInt) //Devuelve los bloques rotos + bonus 1 pt extra por 10 explotados
                else {
                  val (bomba_aux_lista, bomba_aux_true) = ponerBomba2(matriz, f, c, filas, columnas, dificultad)
                  if (bomba_aux_true == 1 && accion < 3) examinar_idx(indice + 1, indice, 2, bomba_aux_lista, blqs_rts)
                  else {
                    val (pares_aux_lista, pares_aux_true) = comprobarPares(matriz, f, c, filas, columnas, dificultad)
                    if (pares_aux_true == 1 && accion < 2) examinar_idx(indice + 1, indice, 1, pares_aux_lista, blqs_rts)
                    else {
                      if (accion > 0) examinar_idx(indice + 1, mejor_idx, accion, mejor_matriz, blqs_rts)
                      else examinar_idx(indice + 1, indice, 0, matriz, blqs_rts)
                    }
                  }
                }
              }
            }
          }
        }

      }
    }

    val chk = examinar_idx(0, 0, 0, matriz, 0)
    if (chk._2 == 0) return (matriz, 0, 0)
    else return (chk._1, 1, chk._3)
  }
  // ############################################################################################################################


  // ############################################################################################################################
  // ###################################################### PROGRAMA MAIN #######################################################
  // ############################################################################################################################
  def main(args: Array[String]): Unit = {
    val random = new Random()

    print("\n----> BIENVENIDO A CANDY CROSH SOGA <----\n")
    print("Introduce las filas deseadas: ")
    val filas = pedirEntero()
    print("Introduce las columnas deseadas: ")
    val columnas = pedirEntero()
    val tamano_matriz = filas * columnas
    print("Introduce la dificultad deseada (1 = Facil | 2 = Dificil): ")
    val dificultad = pedirDificultad()
    print("Introduce la ejecucion deseada (0 = Automatico | 1 = Manual): ")
    val ejecucion = pedirModo()
    val puntos = 0
    //var matriz = crear_matriz(tamano_matriz, dificultad)

    var matriz = List(2, 1, 2, 2, 2, 2, 2, 5, 1, 1,
      1, 3, 6, 2, 2, 2, 6, 6, 5, 2,
      6, 3, 5, 1, 6, 3, 5, 2, 3, 1,
      1, 3, 2, 3, 3, 3, 1, 4, 4, 1,
      4, 3, 4, 4, 6, 1, 1, 1, 6, 1,
      1, 3, 2, 1, 1, 1, 6, 2, 2, 1,
      6, 3, 5, 1, 6, 1, 5, 1, 6, 1,
      5, 1, 3, 3, 3, 1, 2, 3, 3, 3,
      1, 4, 4, 4, 4, 4, 4, 4, 6, 1,
      1, 3, 6, 1, 1, 2, 2, 1, 1, 1)

    def juego(matriz: List[Int], filas: Int, columnas: Int, dificultad: Int, ejecucion: Int, puntos: Int, contador_pares: Int, vidas: Int = 5): Int = {
      if (vidas == 0) {
        puntos
      } else {
        imprimirMatriz(matriz, columnas, filas)
        val fila: Int = if (ejecucion == 0) random.nextInt(filas) else {
          print("Fila a examinar: "); pedirEntero();
        }
        val columna: Int = if (ejecucion == 0) random.nextInt(columnas) else {
          print("Columna a examinar: "); pedirEntero();
        }
        if (ejecucion == 0) print("Fila a examinar: " + fila + "\n");
        print("Columna a examinar: " + columna + "\n")

        val (rompecabezas_exp_lista, rompecabezas_exp_true, rot_rompecabezas) = explotarRompecabezas(matriz, fila, columna, filas, columnas, dificultad) //Mete en variables si se puede explotar un rompecabezas y la matriz resultante
        if (rompecabezas_exp_true == 0) { //No ha podido explotar rompecabezas, seguir comprobando con => PONER ROMPECABEZAS
          val (rompecabezas_aux_lista, rompecabezas_aux_true) = comprobarRompecabezas(matriz, fila, columna, filas, columnas, dificultad) //Mete en variables si se puede poner un rompecabezas y la matriz resultante
          if (rompecabezas_aux_true == 0) { //No ha podido poner rompecabezas, seguir comprobando con => EXPLOTAR TNT
            val (tnt_exp_lista, tnt_exp_true, rot_tnt) = explotarTNT(matriz, fila, columna, filas, columnas, dificultad)
            if (tnt_exp_true == 0) { //No ha podido explotar TNT, seguir comprobando con => PONER TNT
              val (tnt_aux_lista, tnt_aux_true) = ponerTNT(matriz, fila, columna, filas, columnas, dificultad) //Mete en variables si se puede poner un TNT y la matriz resultante
              if (tnt_aux_true == 0) { //No ha podido poner TNT, seguir comprobando con => EXPLOTAR BOMBA
                val (bomba_exp_lista, bomba_exp_true, rot_bmb) = explotarBomba(matriz, fila, columna, filas, columnas, dificultad) //Mete en variables si se puede explotar una Bomba y la matriz resultante
                if (bomba_exp_true == 0) { //No ha podido explotar BOMBA, seguir comprobando con => PONER BOMBA
                  val (bomba_aux_lista, bomba_aux_true) = ponerBomba2(matriz, fila, columna, filas, columnas, dificultad) //Mete en variables si se puede poner una Bomba y la matriz resultante
                  if (bomba_aux_true == 0) { //No ha podido poner BOMBA, seguir comprobando con => COMPROBAR PARES
                    val (pares_aux_lista, pares_aux_true) = comprobarPares(matriz, fila, columna, filas, columnas, dificultad)
                    if (pares_aux_true == 0) { //No ha podido comprobar pares => VIDA PERDIDA, no hay mas comprobaciones

                      print("\n [INFORMACION]: VIDA PERDIDA\n")
                      print("[VIDAS]: " + (vidas - 1) + "\n\n")
                      print("[INFORMACION]: Puntos Actuales: " + puntos + "\n\n")
                      juego(matriz, filas, columnas, dificultad, ejecucion, puntos, contador_pares, vidas - 1) //VIDA PERDIDA
                    }
                    else {
                      print("\n [ACCION]: PARES ENCONTRADOS\n\n");
                      if (contador_pares + 1 == 10) {
                        val puntos_aux = puntos + 2
                        print("[INFORMACION]: Se han eliminado: " + (contador_pares + 1) + " bloques de pares, se suma un punto adicional" + "\n\n")
                        print("[INFORMACION]: Puntos Actuales: " + puntos_aux + "\n\n")
                        juego(pares_aux_lista, filas, columnas, dificultad, ejecucion, puntos_aux, 0, vidas) //Pares comprobados, seguir jugando sin perder vida
                      } else {
                        val puntos_aux = puntos + 1
                        print("[INFORMACION]: Puntos Actuales: " + puntos_aux + "\n\n")
                        juego(pares_aux_lista, filas, columnas, dificultad, ejecucion, puntos_aux, contador_pares + 1, vidas) //Pares comprobados, seguir jugando sin perder vida
                      }
                    }
                  }
                  else {
                    print("\n [ACCION]: BOMBA PUESTA\n\n");
                    print("[INFORMACION]: Puntos Actuales: " + puntos + "\n\n")
                    juego(bomba_aux_lista, filas, columnas, dificultad, ejecucion, puntos, contador_pares, vidas)

                  } //BOMBA puesta, seguir jugando sin perder vida
                }
                else {
                  print("\n [ACCION]: BOMBA EXPLOTADA\n\n");
                  val puntos_aux = puntos + 5
                  print("[INFORMACION]: Puntos Actuales: " + puntos_aux + "\n\n")
                  juego(bomba_exp_lista, filas, columnas, dificultad, ejecucion, puntos_aux, contador_pares, vidas)

                } //BOMBA explotada, seguir jugando sin perder vida
              }
              else {
                print("\n [ACCION]: TNT PUESTA\n\n");
                print("[INFORMACION]: Puntos Actuales: " + puntos + "\n\n")
                juego(tnt_aux_lista, filas, columnas, dificultad, ejecucion, puntos, contador_pares, vidas)

              } //TNT puesta, seguir jugando sin perder vida
            }
            else {
              print("\n [ACCION]: TNT EXPLOTADA\n\n");
              val puntos_aux = puntos + 10
              print("[INFORMACION]: Puntos Actuales: " + puntos_aux + "\n\n")
              juego(tnt_exp_lista, filas, columnas, dificultad, ejecucion, puntos_aux, contador_pares, vidas)
            } //TNT explotada, seguir jugando sin perder vida
          }
          else {
            print("\n [ACCION]: ROMPECABEZAS PUESTO\n\n");
            print("[INFORMACION]: Puntos Actuales: " + puntos + "\n\n")
            juego(rompecabezas_aux_lista, filas, columnas, dificultad, ejecucion, puntos, contador_pares, vidas)
          } //Rompecabezas puesto, seguir jugando sin perder vida
        }
        else {
          print("\n [ACCION]: ROMPECABEZAS EXPLOTADO\n\n");
          val puntos_aux = puntos + 15
          print("[INFORMACION]: Puntos Actuales: " + puntos_aux + "\n\n")
          juego(rompecabezas_exp_lista, filas, columnas, dificultad, ejecucion, puntos_aux, contador_pares, vidas)
        } //Rompecabezas explotado, seguir jugando sin perder vida
      }
    }

    def juego2(matriz: List[Int], filas: Int, columnas: Int, dificultad: Int, vidas: Int = 5, puntos:Int = 0): Int = {
      print("\n [PUNTOS]: TOTAL-> " + puntos + "\n")
      if (vidas <= 0) return puntos
      imprimirMatriz(matriz, columnas, filas)
      val (mat2, accion, pts) = Optimizado(matriz, filas, columnas, dificultad)
      if (accion == 0) {
        print("\n [INFORMACION]: VIDA PERDIDA, NO HAY ACCION RELEVANTE\n"); print("\n [PUNTOS]: NO SE HAN GANADO PUNTOS EN LA JUGADA\n"); juego2(matriz, filas, columnas, dificultad, vidas - 1, puntos)
      }
      else {
        print("\n [PUNTOS]: GANADOS EN LA JUGADA -> " + pts + "\n"); juego2(mat2, filas, columnas, dificultad, vidas, puntos + pts)
      }
    }

    print("\n [MASTER]: JUEGO INICIADO\n\n")
    val puntos_final = if (ejecucion == 0) juego2(matriz, filas, columnas, dificultad) else juego(matriz, filas, columnas, dificultad, ejecucion, puntos, 0) //Juego automatico o manueal
    print("\n!!!! FIN DEL JUEGO !!!!\n")
    print("\nTODAS LAS VIDAD PERDIDAS\n")

    val puntos_multiplicador = if (dificultad == 1) puntos_final else puntos_final*2

    if (dificultad == 1) {
      print("\n**** [Informacion] Como se ha jugado en dificultad facil, los puntos finales se multiplican por 1:" + " ****\n")
      print("\n**** [Informacion] Los puntos totales obtenidos durante la partida son:" + puntos_multiplicador + " ****\n")
    } else if (dificultad == 2) {
      print("\n**** [Informacion] Como se ha jugado en dificultad dificil, los puntos finales se multiplican por 2:" + "****\n")
      print("\n**** [Informacion] Los puntos totales obtenidos durante la partida son:" + puntos_multiplicador + " ****\n")
    }



    val jugador_usuario: String = scala.io.StdIn.readLine("Por favor, introduce tu nombre: ")

    val jdbcUrl = "jdbc:postgresql://postgresql-db-scala.postgres.database.azure.com:5432/candycrush"
    val user = "Kev@postgresql-db-scala"
    val password = "Scala000"
    val connection = DriverManager.getConnection(jdbcUrl, user, password)

    val statement = connection.createStatement()
    val sql = "INSERT INTO puntuaciones (nombre, puntos) VALUES ('"+jugador_usuario+"','"+puntos_multiplicador+"')"

    val filasInsertadas = statement.executeUpdate(sql)

    println(s"$filasInsertadas filas insertadas")

    statement.close()
    connection.close()


    return 0
    // ############################################################################################################################
  }
}
