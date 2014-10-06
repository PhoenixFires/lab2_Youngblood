object Lab2 extends jsy.util.JsyApplication {
  import jsy.lab2.Parser
  import jsy.lab2.ast._

  /*
	 * CSCI 3155: Lab 2
	 * Catherine Youngblood
	 * 
	 * Partner 1: Catherine Dewerd
	 * Partner 2: Mark Ariniello 
	 * Collaborators: <Any Collaborators>
	 */

  /*
	 * Do not make other modifications to this template, such as
	 * - adding "extends App" or "extends Application" to your Lab object,
	 * - adding a "main" method, and
	 * - leaving any failing asserts.
	 */

  /* We represent a variable environment is as a map from a string of the
	 * variable name to the value to which it is bound.
	 * 
	 * You may use the following provided helper functions to manipulate
	 * environments, which are just thin wrappers around the Map type
	 * in the Scala standard library.  You can use the Scala standard
	 * library directly, but these are the only interfaces that you
	 * need.
	 */
  type Env = Map[String, Expr]
  val emp: Env = Map()
  def get(env: Env, x: String): Expr = { env(x) }
  def extend(env: Env, x: String, v: Expr): Env = {
    require(isValue(v))
    (env + (x -> v))
  }

  /* Some useful Scala methods for working with Scala values include:
	 * - Double.NaN
	 * - s.toDouble (for s: String)
	 * - n.isNaN (for n: Double)
	 * - n.isWhole (for n: Double)
	 * - s (for n: Double)
	 * - s format n (for s: String [a format string like for printf], n: Double)
	 */

  def toNumber(v: Expr): Double = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => n
      case S(str) => try { str.toDouble } catch { case e: Exception => { Double.NaN } }
      case B(b) => if (b) { 1.0 } else { 0.0 }
      case Undefined => Double.NaN
      case _ => Double.NaN
    }
  }

  def toBoolean(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      case B(b) => b
      case N(n) => if (n == 0.0 || n.isNaN) { false } else { true }
      case S(str) => if (str.isEmpty()) { false } else { true }
      case Var(x) => if (x.isEmpty()) { false } else { true }
      case Undefined => false
      case _ => false
    }
  }

  def toStr(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case S(str) => str
      case N(n) => if ((n % 1.0) == 0.0) { (n.toInt).toString() } else { n.toString() }
      case B(b) => if (b) { "true" } else { "false" }
      case Var(x) => x
      case Undefined => "undefined"
      case _ => throw new UnsupportedOperationException
    }
  }

  def eval(env: Env, e: Expr): Expr = {
    /* Some helper functions for convenience. */
    def eToVal(e: Expr): Expr = eval(env, e)

    e match {
      /* Base Cases */
      case N(n) => N(n)
      case B(b) => B(b)
      case S(str) => S(str)
      case Var(x) => { eToVal(get(env, x)) }

      /* Inductive Cases */
      case ConstDecl(x, e1, e2) => { eval(extend(env, x, eToVal(e1)), e2) }

      case Unary(uop, e1) => (e1: @unchecked) match {
        case Undefined => B(false)
        case _ => (uop: @unchecked) match {
          case Neg => N(-toNumber(eToVal(e1)))
          case Not => B(!toBoolean(e1))
          case _ => throw new UnsupportedOperationException
        }
      }
      /*case Binary(bop, e1, e2) => (e1: @unchecked) match {
        case Undefined => B(false)
        case _ => (e2: @unchecked) match {
          case Undefined => B(false)
          case _ => (bop: @unchecked) match {
            case Plus => N(toNumber(eToVal(e1)) + toNumber(eToVal(e2)))
            case Minus => N(toNumber(eToVal(e1)) - toNumber(eToVal(e2)))
            case Times => N(toNumber(eToVal(e1)) * toNumber(eToVal(e2)))
            case Div => N(toNumber(eToVal(e1)) / toNumber(eToVal(e2)))

            case Eq => B(toNumber(eToVal(e1)) == toNumber(eToVal(e2)))
            case Ne => B(toNumber(eToVal(e1)) != toNumber(eToVal(e2)))
            case Lt => B(toNumber(eToVal(e1)) < toNumber(eToVal(e2)))
            case Le => B(toNumber(eToVal(e1)) <= toNumber(eToVal(e2)))
            case Gt => B(toNumber(eToVal(e1)) > toNumber(eToVal(e2)))
            case Ge => B(toNumber(eToVal(e1)) >= toNumber(eToVal(e2)))

            case And => e1 match {
              case N(n) => if (toBoolean(eToVal(e1)) && toBoolean(eToVal(e2))) { e1 } else { N(0.0) }
              case S(str) => S(toStr(B(toBoolean(eToVal(e1)) && toBoolean(eToVal(e2)))))
              case _ => e2 match {
                case N(n) => if (toBoolean(eToVal(e1)) && toBoolean(eToVal(e2))) { e2 } else { N(0.0) }
                case B(b) => B(toBoolean(eToVal(e1)) && toBoolean(eToVal(e2)))
                case S(str) => S(toStr(B(toBoolean(eToVal(e1)) && toBoolean(eToVal(e2)))))
                case _ => throw new UnsupportedOperationException
              }
            }
            case Or => e1 match {
              case N(n) => if (toBoolean(eToVal(e1)) || toBoolean(eToVal(e2))) { e1 } else { N(0.0) }
              case S(str) => S(toStr(B(toBoolean(eToVal(e1)) || toBoolean(eToVal(e2)))))
              case _ => e2 match {
                case N(n) => if (toBoolean(eToVal(e1)) || toBoolean(eToVal(e2))) { e2 } else { N(0.0) }
                case B(b) => B(toBoolean(eToVal(e1)) || toBoolean(eToVal(e2)))
                case S(str) => S(toStr(B(toBoolean(eToVal(e1)) || toBoolean(eToVal(e2)))))
                case _ => throw new UnsupportedOperationException
              }
            }

            case Seq => { eToVal(e1); eToVal(e2) }

            case _ => throw new UnsupportedOperationException
          }
        }
      }*/

      case Binary(bop, e1, e2) => (e1, e2) match {
        case (Undefined, e2) => bop match {
          case Plus => N(Double.NaN)
          case Minus => N(Double.NaN)
          case Times => N(Double.NaN)
          case Div => N(Double.NaN)

          case Eq => if (e2 == Undefined) { B(true) } else { B(false) }
          case Ne => if (e2 == Undefined) { B(false) } else { B(true) }
          case Lt => B(false)
          case Le => B(false)
          case Gt => B(false)
          case Ge => B(false)
          case And => Undefined
          case Or => eToVal(e2)
          case Seq => eToVal(e2)
        }
        case (e1, Undefined) => bop match {
          case Plus => N(Double.NaN)
          case Minus => N(Double.NaN)
          case Times => N(Double.NaN)
          case Div => N(Double.NaN)

          case Eq => B(false)
          case Ne => B(true)
          case Lt => B(false)
          case Le => B(false)
          case Gt => B(false)
          case Ge => B(false)
          case And => Undefined
          case Or => eToVal(e1)
          case Seq => Undefined
        }
        case (_, _) => (bop: @unchecked) match {
          case Plus => (e1, e2) match {
            case (S(str), _) => S(toStr(eToVal(e1)) + toStr(eToVal(e2)))
            case (_, S(str)) => S(toStr(eToVal(e1)) + toStr(eToVal(e2)))
            case (_, _) => N(toNumber(eToVal(e1)) + toNumber(eToVal(e2)))
          }
          case Minus => N(toNumber(eToVal(e1)) - toNumber(eToVal(e2)))
          case Times => N(toNumber(eToVal(e1)) * toNumber(eToVal(e2)))
          case Div => N(toNumber(eToVal(e1)) / toNumber(eToVal(e2)))

          case Eq => (e1, e2) match {
            case (S(s1), S(s2)) => B(s1 == s2)
            case (S(s), _) => B(false)
            case (_, S(s)) => B(false)
            case (_, _) => B(toNumber(eToVal(e1)) == toNumber(eToVal(e2)))
          }
          case Ne => (e1, e2) match {
            case (S(s1), S(s2)) => B(s1 != s2)
            case (S(s), _) => B(true)
            case (_, S(s)) => B(true)
            case (_, _) => B(toNumber(eToVal(e1)) == toNumber(eToVal(e2)))
          }
          case Lt => (e1, e2) match {
            case (S(s1), S(s2)) => B(s1 < s2)
            case (S(s), _) => B(false)
            case (_, S(s)) => B(false)
            case (_, _) => B(toNumber(eToVal(e1)) < toNumber(eToVal(e2)))
          }
          case Le => (e1, e2) match {
            case (S(s1), S(s2)) => B(s1 <= s2)
            case (S(s), _) => B(false)
            case (_, S(s)) => B(false)
            case (_, _) => B(toNumber(eToVal(e1)) <= toNumber(eToVal(e2)))
          }
          case Gt => (e1, e2) match {
            case (S(s1), S(s2)) => B(s1 > s2)
            case (S(s), _) => B(false)
            case (_, S(s)) => B(false)
            case (_, _) => B(toNumber(eToVal(e1)) > toNumber(eToVal(e2)))
          }
          case Ge => (e1, e2) match {
            case (S(s1), S(s2)) => B(s1 >= s2)
            case (S(s), _) => B(false)
            case (_, S(s)) => B(false)
            case (_, _) => B(toNumber(eToVal(e1)) >= toNumber(eToVal(e2)))
          }

          case And => (e1, e2) match {
            case (S(s1), S(s2)) => S(s2)
            case (B(b1), B(b2)) => B(b1 && b2)
            case (B(b), e2) => if (b) { eToVal(e2) } else { B(false) }
            case (e1, B(b)) => if (b) { B(b) } else { B(false) }
            case (_, e2) => eToVal(e2)
            /*
            case N(n) => if (toBoolean(eToVal(e1)) && toBoolean(eToVal(e2))) { e1 } else { N(0.0) }
            case S(str) => S(toStr(B(toBoolean(eToVal(e1)) && toBoolean(eToVal(e2)))))
            case _ => e2 match {
              case N(n) => if (toBoolean(eToVal(e1)) && toBoolean(eToVal(e2))) { e2 } else { N(0.0) }
              case B(b) => B(toBoolean(eToVal(e1)) && toBoolean(eToVal(e2)))
              case S(str) => S(toStr(B(toBoolean(eToVal(e1)) && toBoolean(eToVal(e2)))))
              case _ => throw new UnsupportedOperationException
            }
            * 
            */
          }
          case Or => (e1, e2) match {
            //case (B(b1),B(b2)) => if (!b1) {B(b2)} else {B(b1)}
            case (B(b), e2) => if (!b) { eToVal(e2) } else { B(b) }
            case (e1, e2) => eToVal(e1)
            /*
            case N(n) => if (toBoolean(eToVal(e1)) || toBoolean(eToVal(e2))) { e1 } else { N(0.0) }
            case S(str) => S(toStr(B(toBoolean(eToVal(e1)) || toBoolean(eToVal(e2)))))
            case _ => e2 match {
              case N(n) => if (toBoolean(eToVal(e1)) || toBoolean(eToVal(e2))) { e2 } else { N(0.0) }
              case B(b) => B(toBoolean(eToVal(e1)) || toBoolean(eToVal(e2)))
              case S(str) => S(toStr(B(toBoolean(eToVal(e1)) || toBoolean(eToVal(e2)))))
              case _ => throw new UnsupportedOperationException
            }*/
          }

          case Seq => { eToVal(e1); eToVal(e2) }

          case _ => throw new UnsupportedOperationException

        }
      }

      case If(e1, e2, e3) => if (toBoolean(eToVal(e1))) { eToVal(e2) } else { eval(e3) }
      case Print(e1) =>
        println(pretty(eToVal(e1))); Undefined
      case Undefined => Undefined
      case _ => throw new UnsupportedOperationException
    }
  }

  // Interface to run your interpreter starting from an empty environment.
  def eval(e: Expr): Expr = { eval(emp, e) }

  // Interface to run your interpreter from a string.  This is convenient
  // for unit testing.
  def eval(s: String): Expr = { eval(Parser.parse(s)) }

  /* Interface to run your interpreter from the command-line.  You can ignore what's below. */
  def processFile(file: java.io.File) {
    if (debug) { println("Parsing ...") }

    val expr = Parser.parseFile(file)

    if (debug) {
      println("\nExpression AST:\n  " + expr)
      println("------------------------------------------------------------")
    }

    if (debug) { println("Evaluating ...") }

    val v = eval(expr)

    println(pretty(v))
  }

}