package while2scala

abstract class WHILEProgram {
  def apply(initialValues: Long*): Long = apply(new WHILEState(initialValues))
  def apply(initialState: WHILEState): Long = execute(initialState)(0)
  def execute(state: WHILEState): WHILEState
}

// x := y + n
case class Plus(x: Int, y: Int, n: Long) extends WHILEProgram {
  def execute(state: WHILEState): WHILEState = state + (x, state(y) + n)
}

// x := y - n
case class Minus(x: Int, y: Int, n: Long) extends WHILEProgram {
  def execute(state: WHILEState): WHILEState = state + (x, Util.modDiff(state(y), n))
}

// p ; q
case class Composition(p: WHILEProgram, q: WHILEProgram) extends WHILEProgram {
  def execute(state: WHILEState): WHILEState = q.execute(p.execute(state))
}

// LOOP x DO p END
case class Loop(x: Int, p: WHILEProgram) extends WHILEProgram {
  def execute(state: WHILEState): WHILEState = {
    var n = state(x)
    while (n > 0) {
      p.execute(state)
      n -= 1
    }
    state
  }
}

// WHILE x!=0 DO p END
case class While(x: Int, p: WHILEProgram) extends WHILEProgram {
  def execute(state: WHILEState): WHILEState = {
    while (state(x) != 0)
      p.execute(state)
    state
  }
}

object Util {
  def modDiff(x: Long, y: Long): Long = scala.math.max(0, x - y)
}