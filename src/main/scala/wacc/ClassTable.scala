package wacc

import wacc.AST.{Param, Public, Scope, VarDec}
import wacc.SemTypes.CharSemType

import scala.collection.mutable.ListBuffer

object ClassTable {

  case class ClassTable() {
    private val classMap = collection.mutable.Map.empty[String, ClassDefinition]

    def lookup(name : String) : Option[ClassDefinition]  =  classMap.get(name)
    def add(name : String) : Unit = classMap.addOne(name, ClassDefinition(name))
    def getKeys: Iterable[String] = classMap.keys
  }

  case class ClassDefinition(className : String) {

    private final val CLASS_FIELD_PREFIX = "this."
    private final val CLASS_METHOD_PREFIX = "wacc_class_" + className + "_"

    private val types: SymbolTable[SemTypes.SemType] = new SymbolTable[SemTypes.SemType](None)
    private val offsetScope: SymbolTable[(Option[Int], Scope)] = new SymbolTable[(Option[Int], Scope)](None)
    private val params: ListBuffer[Param] = new ListBuffer[Param].empty
    private val varDecs : ListBuffer[VarDec] = new ListBuffer[VarDec].empty
    private var size = 0

    def getDefinition(): (SymbolTable[SemTypes.SemType], SymbolTable[(Option[Int], Scope)], ListBuffer[Param]) = (types, offsetScope, params)

    def getTypeTable: SymbolTable[SemTypes.SemType] = types

    // for the types of class methods
    def addMethodType(name : String, semTypes: SemTypes.SemType): Unit = types.add(CLASS_METHOD_PREFIX + name, semTypes)
    def getMethodType(name : String): Option[SemTypes.SemType] = types.lookup(CLASS_METHOD_PREFIX + name)

    def addFieldType(name : String, semTypes: SemTypes.SemType): Unit = types.add(CLASS_FIELD_PREFIX + name, semTypes)
    def getFieldType(name : String): Option[SemTypes.SemType] = types.lookup(CLASS_FIELD_PREFIX + name)

    // for generation of fields during class creation
    def addField(vd: AST.VarDec) : Unit = varDecs.append(vd)

    def getFields: List[VarDec] = varDecs.toList

    // for parameter semantic analysis
    def addParam(param: Param, offset : Int, paramType : SemTypes.SemType) : Unit = {
      params.append(param)
      addFieldType(param.ident, paramType)
      addFieldOffsetScope(param.ident, offset, Public()(param.pos))
    }
    def getParams: List[Param] = params.toList
    def numParams: Int = params.size

    // for offset and scope of class members and methods (offset of methods is None)
    def getFieldOffset(name : String) : Option[Int] = getOffset(CLASS_FIELD_PREFIX + name)
    def getMethodOffset(name : String) : Option[Int] = getOffset(CLASS_METHOD_PREFIX + name)
    private def getOffset(name : String) : Option[Int] = {
      val opOffsetScope = offsetScope.lookup(name)
      if (opOffsetScope.isDefined) {
        return opOffsetScope.get._1
      }
      None
    }

    def getFieldScope(name : String) : Option[Scope] = getScope(CLASS_FIELD_PREFIX + name)
    def getMethodScope(name : String) : Option[Scope] = getScope(CLASS_METHOD_PREFIX + name)
    private def getScope(name : String) : Option[Scope] = {
      val opOffsetScope = offsetScope.lookup(name)
      if (opOffsetScope.isDefined) {
        return Some(opOffsetScope.get._2)
      }
      None
    }

    def addFieldOffsetScope(name : String, offset : Int, scope: Scope) : Unit
      = addOffsetScope(CLASS_FIELD_PREFIX + name, Some(offset), scope)
    def addMethodOffsetScope(name : String, scope: Scope) : Unit
      = addOffsetScope(CLASS_METHOD_PREFIX + name, None, scope)

    private def addOffsetScope(name : String,  offset : Option[Int], scope: Scope) : Unit = offsetScope.add(name, (offset, scope))

    def setClassSize(totalSize : Int) : Unit = size = totalSize
    def getClassSize: Int = size

  }

}
