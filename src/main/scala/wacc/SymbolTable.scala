package wacc

import scala.collection.mutable

class SymbolTable[A](val encSymTable: Option[GenericTable[A]],
                     val map: mutable.Map[String, A] = mutable.Map.empty[String, A]) extends GenericTable[A] {

  var topLevelEntryCount = 0

  def add(name: String, ty: A): Unit = {
    map.addOne(name, ty)
    incrementTopLevelCount()
  }

  def lookup(name: String): Option[A] = map.get(name)

  def lookupAll(name: String): Option[A] = {
    var table: Option[GenericTable[A]] = Option(this)

    while (table.isDefined) {
      val opType = table.get.lookup(name)
      if (opType.isDefined) {
        return opType
      }
      table = table.get.getParent()
    }
    None
  }

  def getParent(): Option[GenericTable[A]] = encSymTable

  def incrementCount(): Unit = topLevelEntryCount += 1

  def getCount(): Int = topLevelEntryCount

  def incrementTopLevelCount(): Unit = {
    var liveMap: Option[GenericTable[A]] = Option(this)
    while (liveMap.get.getParent().isDefined) {
      liveMap = liveMap.get.getParent()
    }
    liveMap.get.incrementCount()
  }

  def getNestedEntries(): Int = {
    var liveMap: Option[GenericTable[A]] = Option(this)
    while (liveMap.get.getParent().isDefined) {
      liveMap = liveMap.get.getParent()
    }
    liveMap.get.getCount()
  }
}
