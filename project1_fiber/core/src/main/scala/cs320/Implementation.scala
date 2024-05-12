package cs320

import Value._

object Implementation extends Template {

  // Add, Mul
  def IntVop1(op: (BigInt, BigInt) => BigInt): (Value, Value) => IntV = (_, _) match {
    case (IntV(x), IntV(y)) => IntV(op(x, y))
    case (x, y) => error(s"not both numbers")
  }

  // Div, Mod
  def IntVop2(op: (BigInt, BigInt) => BigInt): (Value, Value) => IntV = (_, _) match {
    case (IntV(x), IntV(y)) => {
      if (y == 0) error(s"divided by zero")
      IntV(op(x, y))
    }
    case (x, y) => error(s"not both numbers")
  }

  // Eq, Lt
  def IntVop3(op: (BigInt, BigInt) => Boolean): (Value, Value) => BooleanV = (_, _) match {
    case (IntV(x), IntV(y)) => BooleanV(op(x, y))
    case (x, y) => error(s"not both numbers")
  }

  val IntVAdd = IntVop1(_ + _)
  val IntVMul = IntVop1(_ * _)
  val IntVDiv = IntVop2(_ / _)
  val IntVMod = IntVop2(_ % _)
  val IntVEq = IntVop3(_ == _)
  val IntVLt = IntVop3(_ < _)

  def helper(expr: Expr, env: Env): Value = expr match{
    // variable
    case Id(name: String) => env.getOrElse(name, error(s"free identifier"))
    // integer
    case IntE(value: BigInt) => IntV(value)
    // boolean
    case BooleanE(value: Boolean) => BooleanV(value)
    // addition
    case Add(left: Expr, right: Expr) => IntVAdd(helper(left, env), helper(right, env))
    // multiplication
    case Mul(left: Expr, right: Expr) => IntVMul(helper(left, env), helper(right, env))
    // division
    case Div(left: Expr, right: Expr) => IntVDiv(helper(left, env), helper(right, env))
    // modulo
    case Mod(left: Expr, right: Expr) => IntVMod(helper(left, env), helper(right, env))
    // equal-to
    case Eq(left: Expr, right: Expr) => IntVEq(helper(left, env), helper(right, env))
    // less-then
    case Lt(left: Expr, right: Expr) => IntVLt(helper(left, env), helper(right, env))
    // conditional
    case If(condition: Expr, trueBranch: Expr, falseBranch: Expr) => helper(condition, env) match{
      case BooleanV(true) => helper(trueBranch, env)
      case BooleanV(false) => helper(falseBranch, env)
      case v => error(s"not a boolean")
    }
    // tuple
    case TupleE(expressions: List[Expr]) => TupleV(expressions.map(helper(_, env)))
    // projection
    case Proj(expression: Expr, index: Int) => helper(expression, env) match{
      case TupleV(values) => {
        val len: Int = values.length
        if (index > len) error(s"Index out of range")
        values(index - 1)
      }
      case v => error(s"not a tuple")
    }
    // nil
    case NilE => NilV
    // cons
    case ConsE(head: Expr, tail: Expr) => {
      val h: Value = helper(head, env)
      val t: Value = helper(tail, env) 
      t match{
        case NilV => ConsV(h, t)
        case ConsV(_, _) => ConsV(h, t)
        case v => error(s"not a NilV or ConsV")
      }
    }
    // is-empty
    case Empty(expression: Expr) => helper(expression, env) match{
      case NilV => BooleanV(true)
      case ConsV(_, _) => BooleanV(false)
      case v => error(s"not a NilV or ConsV")
    }
    // head
    case Head(expression: Expr) => helper(expression, env) match{
      case ConsV(head, tail) => head
      case v => error(s"not a nonempty list")
    }
    // tail
    case Tail(expression: Expr) => helper(expression, env) match{
      case ConsV(head, tail) => tail
      case v => error(s"not a nonempty list")
    }
    // local variable
    case Val(name: String, expression: Expr, body: Expr) => helper(body, env + (name -> helper(expression, env)))
    // anonymous function
    case Fun(parameters: List[String], body: Expr) => CloV(parameters, body, env)
    // recursive function
    case RecFuns(functions: List[FunDef], body: Expr) => {
      val clovs: List[CloV] = functions.map {case FunDef(name, parameters, fbody) => CloV(parameters, fbody, env)}
      val names: List[String] = functions.map {case FunDef(name, parameters, fbody) => name}
      val nenv: Env = env ++ (names zip clovs)
      clovs.map(clov => {clov.env = nenv})
      helper(body, nenv)
    }
    // function application
    case App(function: Expr, arguments: List[Expr]) => helper(function, env) match{
      case CloV(parameters, body, fenv) => {
        val avals = arguments.map(helper(_, env))
        if (parameters.length != avals.length) error(s"wrong arguments")
        helper(body, fenv ++ (parameters zip avals))
      }
      case v => error(s"not a closure")
    }
    // type test
    case Test(expression: Expr, typ: Type) => helper(expression, env) match{
      case IntV(_) => typ match{
        case IntT => BooleanV(true)
        case _ => BooleanV(false)
      }
      case BooleanV(_) => typ match{
        case BooleanT => BooleanV(true)
        case _ => BooleanV(false)
      }
      case TupleV(_) => typ match{
        case TupleT => BooleanV(true)
        case _ => BooleanV(false)
      }
      case NilV => typ match{
        case ListT => BooleanV(true)
        case _ => BooleanV(false)
      }
      case ConsV(_, _) => typ match{
        case ListT => BooleanV(true)
        case _ => BooleanV(false)
      }
      case CloV(_, _, _) => typ match{
        case FunctionT => BooleanV(true)
        case _ => BooleanV(false)
      }
      case v => error(s"undefined value")
    }

    // otherwise
    case v => error(s"undefined expression")
  }

  def interp(expr: Expr): Value = helper(expr, Map())
}