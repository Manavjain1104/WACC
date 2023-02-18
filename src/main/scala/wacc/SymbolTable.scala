package wacc

import scala.collection.mutable

class SymbolTable[A](val encSymTable: Option[SymbolTable[A]],
                  val map: mutable.Map[String, A] = mutable.Map.empty[String, A]) {


  def add(name: String, ty: A) = {
    map.addOne(name, ty)
  }

  def lookup(name: String): Option[A] = map.get(name)

  def lookupAll(name: String): Option[A] = {
    var table: Option[SymbolTable[A]] = Option(this)

    while (table.isDefined) {
      val opType = table.get.lookup(name)
      if (opType.isDefined) {
        return opType
      }
      table = table.get.encSymTable
    }
    None
  }
}
