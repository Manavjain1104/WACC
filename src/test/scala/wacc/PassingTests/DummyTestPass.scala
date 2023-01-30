package wacc

import collection.mutable.Stack
import org.scalatest.flatspec.AnyFlatSpec

class DummyTestPass extends AnyFlatSpec {

  "A Stack" should "pop values in last-in-first-out order" in {
    val stack = new Stack[Int]
    stack.push(1)
    stack.push(2)
    assert(stack.pop() === 2)
    assert(stack.pop() === 1)
  }

  "A Stack2" should "pop values in last-in-first-out order2" in {
    val stack = new Stack[Int]
    stack.push(3)
    stack.push(4)
    assert(stack.pop() === 4)
    assert(stack.pop() === 3)
  }
}