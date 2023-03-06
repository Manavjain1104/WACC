package wacc

trait GenericTable[A] {
  def add(name: String, ty: A)
  def lookup(name: String): Option[A]
  def lookupAll(name: String): Option[A]
  def getNestedEntries(): Int
  def getParent(): Option[GenericTable[A]]
  def incrementCount(): Unit
  def getCount(): Int
}
