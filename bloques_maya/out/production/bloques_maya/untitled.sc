
import scala.collection.GenSeq
import scala.collection.parallel.immutable.{ParSeq, ParVector}

def par_to_list(par: ParVector[GenSeq[Int]]): List[List[Int]] = {
  if (par.isEmpty) Nil
  else par.head.toList :: par_to_list(par.tail)
}

def list_to_par(list: List[List[Int]]): ParVector[ParSeq[Int]] = {
  if (list.isEmpty) ParVector.empty
  else ParVector(list.head.par) ++ list_to_par(list.tail)
}

val par = ParVector(List(1,2,3), List(4,5,6), List(7,8,9))
val lista = par_to_list(par)

println(lista)


val par2 =  ParVector(ParVector(List(1,2,3), List(4,5,6), List(7,8,9)), ParVector(List(1,2,3), List(4,5,6), List(7,8,9)))

def aplanar(par: ParVector[ParSeq[GenSeq[Int]]]): ParVector[GenSeq[Int]] = {
  def aplanar_aux(par: ParSeq[GenSeq[Int]]): ParVector[GenSeq[Int]] = {
    if (par.isEmpty) ParVector()
    else ParVector(par.head) ++ aplanar_aux(par.tail)
  }

  if (par.isEmpty) ParVector()
  else aplanar_aux(par.head) ++ aplanar(par.tail)
}

val par3 = aplanar(par2)
println(par3)

//List(i,j,obtener_columna(coords,2))
