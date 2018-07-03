import Day8.Comparison.Comparison
import Day8.Mutation.Mutation

object Day8 {
  def main(args: Array[String]): Unit = {
    println(day8Part2("b inc 5 if a > 1\na inc 1 if b < 5\nc dec -10 if a >= 1\nc inc -20 if c == 10"))
  }

  def day8Part2(s: String): Int = {
    def go(acc: List[Int], registers: Array[Register], data: List[Instruction]): Int = data match {
      case Nil => acc.max
      case h :: tail => if (Comparison.compare(findRegister(registers, h.condition.register.name).value, h.condition.value, h.condition.comparison)) {
        val currRegister = findRegister(registers, h.register.name)
        val newR = currRegister.copy(value = Mutation.mutate(currRegister.value, h.value, h.mutation))

        go(List(newR.value) ++ acc, registers.updated(registers.indexOf(currRegister), newR), tail)
      } else go(acc, registers, tail)
    }

    val data = s.split("\n").map(_.trim).map(i => getInstruction(i.split(" ")))
    val registers = data.map(_.register)

    go(Nil, registers, data.toList)
  }

  // Not playing with options here, data should be consistent
  def findRegister(registers: Array[Register], name: String): Register = registers.find(_.name == name).get

  def day8Part1(s: String): Int = {
    def go(registers: Array[Register], data: List[Instruction]): Int = data match {
      case Nil => registers.maxBy(_.value).value
      case h :: tail => if (Comparison.compare(registers.find(_.name == h.condition.register.name).get.value, h.condition.value, h.condition.comparison)) {
        val currRegister = registers.find(_.name == h.register.name).get
        val newR = currRegister.copy(value = Mutation.mutate(currRegister.value, h.value, h.mutation))

        go(registers.updated(registers.indexOf(currRegister), newR), tail)
      } else go(registers, tail)
    }

    val data = s.split("\n").map(_.trim).map(i => getInstruction(i.split(" ")))
    val registers = data.map(_.register)

    go(registers, data.toList)
  }

  // Assuming the instruction is always well formatted "b inc 5 if a > 1"
  def getInstruction(ins: Array[String]): Instruction = Instruction(
    Register(ins(0)),
    Mutation.withName(ins(1)),
    ins(2).toInt,
    Condition(Register(ins(4)), Comparison.getVal(ins(5).toString), ins(6).toInt)
  )

  case class Register(name: String, value: Int = 0)

  case class Condition(register: Register, comparison: Comparison, value: Int)

  case class Instruction(register: Register, mutation: Mutation, value: Int, condition: Condition)

  object Mutation extends Enumeration {
    type Mutation = Value
    val inc, dec = Value

    def mutate(a: Int, amount: Int, value: Value): Int = value match {
      case `inc` => a + amount
      case `dec` => a - amount
      case _ => throw new Exception("Bad Comparison")
    }
  }

  object Comparison extends Enumeration {
    type Comparison = Value
    // Can't enum signs < > = and then for ex withName("<") for some reason
    val eq, le, gt, leq, gte, ne = Value

    def getVal(s: String): Value = s match {
      case "==" => eq
      case "<" => le
      case ">" => gt
      case "<=" => leq
      case ">=" => gte
      case "!=" => ne
      case _ => throw new Exception("Bad Comparison")
    }

    def compare(a: Int, b: Int, value: Value): Boolean = value match {
      case `eq` => a == b
      case `le` => a < b
      case `gt` => a > b
      case `leq` => a <= b
      case `gte` => a >= b
      case `ne` => a != b
      case _ => throw new Exception("Bad Comparison")
    }
  }
}
