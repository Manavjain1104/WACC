package wacc

import scala.collection.mutable

class SymbolTable[A](val encSymTable: Option[SymbolTable[A]],
                     val map: mutable.Map[String, A] = mutable.Map.empty[String, A]) {

  var topLevelEntryCount = 0

  def add(name: String, ty: A): Unit = {
    map.addOne(name, ty)
    incrementTopLevelCount()
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

  def incrementTopLevelCount(): Unit = {
    var liveMap = Option(this)
    while (liveMap.get.encSymTable.isDefined) {
      liveMap = liveMap.get.encSymTable
    }
    liveMap.get.topLevelEntryCount += 1
  }

  def getNestedEntries(): Int = {
    var liveMap = Option(this)
    while (liveMap.get.encSymTable.isDefined) {
      liveMap = liveMap.get.encSymTable
    }
    liveMap.get.topLevelEntryCount
  }
}
