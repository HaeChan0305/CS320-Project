package cs320

import Value._

object Implementation extends Template {

  case class Handler(expr_h: Expr, env_h: Env, handler_h: Option[Handler], k_h: Cont)

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

  def helper(expr: Expr, env: Env, hand: Option[Handler], k: Cont): Value = expr match{
    // variable
    case Id(name: String) => k(env.getOrElse(name, error(s"free identifier")))
    
    // integer
    case IntE(value: BigInt) => k(IntV(value))
    
    // boolean
    case BooleanE(value: Boolean) => k(BooleanV(value))
    
    // addition
    case Add(left: Expr, right: Expr) => 
      helper(left, env, hand, lv => 
        helper(right, env, hand, rv => 
          k(IntVAdd(lv, rv))))
    
    // multiplication
    case Mul(left: Expr, right: Expr) => 
      helper(left, env, hand, lv => 
        helper(right, env, hand, rv => 
          k(IntVMul(lv, rv))))
    
    // division
    case Div(left: Expr, right: Expr) => 
      helper(left, env, hand,lv => 
        helper(right, env, hand, rv => 
          k(IntVDiv(lv, rv))))
    
    // modulo
    case Mod(left: Expr, right: Expr) => 
      helper(left, env, hand, lv => 
        helper(right, env, hand, rv => 
          k(IntVMod(lv, rv))))
    
    // equal-to
    case Eq(left: Expr, right: Expr) => 
      helper(left, env, hand, lv => 
        helper(right, env, hand, rv => 
          k(IntVEq(lv, rv))))
    
    // less-then
    case Lt(left: Expr, right: Expr) => 
      helper(left, env, hand, lv => 
        helper(right, env, hand, rv => 
          k(IntVLt(lv, rv))))
    
    // conditional
    case If(condition: Expr, trueBranch: Expr, falseBranch: Expr) => 
      helper(condition, env, hand, cv => 
        cv match{
          case BooleanV(true) => helper(trueBranch, env, hand, k)
          case BooleanV(false) => helper(falseBranch, env, hand, k)
          case v => error(s"not a boolean")
        }
      )
  
    // tuple
    case TupleE(expressions: List[Expr]) => {
      def recurExp(expressions: List[Expr], env: Env, hand: Option[Handler], k: Cont, evals: List[Value]): Value = {
        if (! expressions.isEmpty) {
          helper(expressions.head, env, hand, ev => 
            recurExp(expressions.tail, env, hand, k, evals :+ ev)
          )
        }
        else {
          k(TupleV(evals))
        }
      }
      recurExp(expressions, env, hand, k, List.empty[Value])
    }
    
    // projection
    case Proj(expression: Expr, index: Int) =>
      helper(expression, env, hand, tv =>
        tv match{
          case TupleV(values) => {
            val len: Int = values.length
            if (index > len) error(s"Index out of range")
            k(values(index - 1))
          }
          case v => error(s"not a tuple")
        }
      )

    // nil
    case NilE => k(NilV)

    // cons
    case ConsE(head: Expr, tail: Expr) =>
      helper(head, env, hand, hv =>
        helper(tail, env, hand, tv =>
          tv match {
            case NilV => k(ConsV(hv, tv))
            case ConsV(_, _) => k(ConsV(hv, tv))
            case v => error(s"not a NilV or ConsV")
          }
        )
      )
    
    // is-empty
    case Empty(expression: Expr) => 
      helper(expression, env, hand, ev => 
        ev match{
          case NilV => k(BooleanV(true))
          case ConsV(_, _) => k(BooleanV(false))
          case v => error(s"not a NilV or ConsV")
        }
      )

    // head
    case Head(expression: Expr) => 
      helper(expression, env, hand, ev => 
        ev match{
          case ConsV(head, _) => k(head)
          case v => error(s"not a nonempty list")
        }
      )
    
    // tail
    case Tail(expression: Expr) => 
      helper(expression, env, hand, ev =>
        ev match{
          case ConsV(_, tail) => k(tail)
          case v => error(s"not a nonempty list")
        }
      ) 

    // local variable
    case Val(name: String, expression: Expr, body: Expr) => 
      helper(expression, env, hand, ev =>
        helper(body, env + (name -> ev), hand, k)
      )

    // continuation binding
    case Vcc(name: String, body: Expr) =>
      helper(body, env + (name -> ContV(k)), hand, k)
    
    // anonymous function
    case Fun(parameters: List[String], body: Expr) =>
      k(CloV(parameters, body, env))
    
    // recursive function
    case RecFuns(functions: List[FunDef], body: Expr) => {
      val clovs: List[CloV] = functions.map {case FunDef(name, parameters, fbody) => CloV(parameters, fbody, env)}
      val names: List[String] = functions.map {case FunDef(name, parameters, fbody) => name}
      val nenv: Env = env ++ (names zip clovs)
      clovs.map(clov => {clov.env = nenv})
      helper(body, nenv, hand, k)
    }

    // function application
    case App(function: Expr, arguments: List[Expr]) => {
      def recurArg(fv: Any, arguments: List[Expr], env: Env, hand: Option[Handler], k: Cont, avals: List[Value]): Value = {
        if (!arguments.isEmpty){
          helper(arguments.head, env, hand, av =>
            recurArg(fv, arguments.tail, env, hand, k, avals :+ av)
          )
        }
        else{
          fv match{
            case CloV(parameters, body, fenv) => {
              if (parameters.length != avals.length) error(s"wrong arguments")
              helper(body, fenv ++ (parameters zip avals), hand, k)
            }
            case ContV(kv) => {
              if (avals.length != 1) error(s"number of arugments of ContV should be 1")
              kv(avals.head)
            }
            case v => error(s"not a CloV or ContV")
          }
        }
      }
      helper(function, env, hand, fv => recurArg(fv, arguments, env, hand, k, List.empty[Value]))
    }

    // type test
    case Test(expression: Expr, typ: Type) => 
      helper(expression, env, hand, ev =>
        ev match{
          case IntV(_) => typ match{
            case IntT => k(BooleanV(true))
            case _ => k(BooleanV(false))
          }
          case BooleanV(_) => typ match{
            case BooleanT => k(BooleanV(true))
            case _ => k(BooleanV(false))
          }
          case TupleV(_) => typ match{
            case TupleT => k(BooleanV(true))
            case _ => k(BooleanV(false))
          }
          case NilV => typ match{
            case ListT => k(BooleanV(true))
            case _ => k(BooleanV(false))
          }
          case ConsV(_, _) => typ match{
            case ListT => k(BooleanV(true))
            case _ => k(BooleanV(false))
          }
          case CloV(_, _, _) => typ match{
            case FunctionT => k(BooleanV(true))
            case _ => k(BooleanV(false))
          }
          case ContV(_) => typ match{
            case FunctionT => k(BooleanV(true))
            case _ => k(BooleanV(false))
          }
          case v => error(s"undefined value")
        }
      )
    
    // throwing exception
    case Throw(expression: Expr) => 
      helper(expression, env, hand, ev => 
        hand match{
          case Some(Handler(expr_h: Expr, env_h: Env, handler_h: Option[Handler], k_h: Cont)) => 
            helper(expr_h, env_h, handler_h, ev_h =>
              ev_h match{
                case CloV(parameters, body, fenv) => {
                  if (parameters.length != 1) error(s"number of parameters of CloV should be 1")
                  helper(body, fenv + (parameters.head -> ev), handler_h, k_h)
                }
                case ContV(kv) => kv(ev)
                case _ => error(s"It it not a CloV or ContV")
              }
            )
          case None => error(s"There is no handelr")
        }
      )
    
    // handler registration
    case Try(expression: Expr, handler: Expr) => 
      helper(expression, env, Some(Handler(handler, env, hand, k)), k)


    // otherwise
    case v => error(s"undefined expression")
  }

  def interp(expr: Expr): Value = helper(expr, Map(), None, x => x)
}