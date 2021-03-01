import kotlin.math.*

sealed class Expr {
    abstract fun eval(env: Env): Expr
    abstract fun diff(variable: Variable): Expr
}

data class Value(val value: Double) : Expr() {
    override fun eval(env: Env): Expr = this
    override fun diff(variable: Variable): Expr = Value(0.0)

    override fun toString(): String = value.toString()
}

data class Variable(val name: String) : Expr() {
    override fun eval(env: Env): Expr = env.getOrElse(this) { return this }
    override fun diff(variable: Variable): Expr =
        if (name == variable.name) {
            Value(1.0)
        } else {
            Value(0.0)
        }

    override fun toString(): String = name
}

sealed class BinOp(open val x: Expr, open val y: Expr) : Expr() {
    fun evalXY(env: Env): Pair<Expr, Expr> {
        val x = env.eval(this.x)
        val y = env.eval(this.y)
        return Pair(x, y)
    }
}

data class Plus(override val x: Expr, override val y: Expr) : BinOp(x, y) {
    override fun eval(env: Env): Expr {
        val p = evalXY(env)
        val x = p.first
        val y = p.second
        return when {
            (x is Value && y is Value) -> Value(x.value + y.value)
            (x is Value && x.value == 0.0) -> y
            (y is Value && y.value == 0.0) -> x
            (x == y) -> Multiply(Value(2.0), x)
            ((x is UnMinus && x.x == y) || (y is UnMinus && y.x == y)) -> Value(0.0)
            (x is UnMinus && y is UnMinus) -> UnMinus(Plus(x.x, y.x))
            x is UnMinus -> Minus(y, x.x)
            y is UnMinus -> Minus(x, y.x)
            else -> Plus(x, y)
        }
    }

    override fun diff(variable: Variable): Expr {
        val diffX = x.diff(variable)
        val diffY = y.diff(variable)
        return Plus(diffX, diffY)
    }

    override fun equals(other: Any?): Boolean =
        (other is Plus) && ((x == other.x && y == other.y) || (x == other.y && y == other.x))

    override fun hashCode(): Int = x.hashCode() + y.hashCode()

    override fun toString(): String = "($x) + ($y)"
}

data class Minus(override val x: Expr, override val y: Expr) : BinOp(x, y) {
    override fun eval(env: Env): Expr {
        val p = evalXY(env)
        val x = p.first
        val y = p.second
        return when {
            (x is Value && y is Value) -> Value(x.value - y.value)
            (x is Value && x.value == 0.0) -> UnMinus(y).eval(env)
            (y is Value && y.value == 0.0) -> x
            (x == y) -> Value(0.0)
            (x is UnMinus && y is UnMinus) -> Minus(y.x, x.x)
            x is UnMinus -> UnMinus(Plus(x.x, y))
            y is UnMinus -> Plus(x, y.x)
            else -> Minus(x, y)
        }
    }

    override fun diff(variable: Variable): Expr {
        val diffX = x.diff(variable)
        val diffY = y.diff(variable)
        return Minus(diffX, diffY)
    }
}

data class Multiply(override val x: Expr, override val y: Expr) : BinOp(x, y) {
    override fun eval(env: Env): Expr {
        val p = evalXY(env)
        val x = p.first
        val y = p.second
        return when {
            (x is Value && y is Value) -> Value(x.value * y.value)
            ((x is Value && x.value == 0.0) || (y is Value && y.value == 0.0)) -> Value(0.0)
            (x is Value && x.value == 1.0) -> y
            (y is Value && y.value == 1.0) -> x
            (x is Value && x.value == -1.0) -> UnMinus(y).eval(env)
            (y is Value && y.value == -1.0) -> UnMinus(x).eval(env)
            (x == y) -> Pow(x, Value(2.0))
            (x is UnMinus && y is UnMinus) -> Multiply(y.x, x.x)
            x is UnMinus -> UnMinus(Multiply(x.x, y))
            y is UnMinus -> UnMinus(Multiply(x, y.x))
            else -> Multiply(x, y)
        }
    }

    override fun diff(variable: Variable): Expr {
        val diffX = x.diff(variable)
        val diffY = y.diff(variable)
        return Plus(Multiply(diffX, y), Multiply(x, diffY))
    }

    override fun equals(other: Any?): Boolean =
        (other is Multiply) && ((x == other.x && y == other.y) || (x == other.y && y == other.x))

    override fun hashCode(): Int = x.hashCode() + y.hashCode()
}

data class Divide(override val x: Expr, override val y: Expr) : BinOp(x, y) {
    override fun eval(env: Env): Expr {
        val p = evalXY(env)
        val x = p.first
        val y = p.second
        return when {
            (x is Value && y is Value) -> Value(x.value / y.value)
            (x is Value && x.value == 0.0) -> Value(0.0)
            (y is Value && y.value == 0.0) -> throw ArithmeticException("Division by zero")
            (y is Value && y.value == 1.0) -> x
            (x == y) -> Value(1.0)
            (x is UnMinus && y is UnMinus) -> Divide(y.x, x.x)
            x is UnMinus -> UnMinus(Divide(x.x, y))
            y is UnMinus -> UnMinus(Divide(x, y.x))
            else -> Divide(x, y)
        }
    }

    override fun diff(variable: Variable): Expr {
        val diffX = x.diff(variable)
        val diffY = y.diff(variable)
        return when {
            !x.hasVariable(variable) -> UnMinus(Divide(Multiply(x, diffY), Pow(y, Value(2.0))))
            !y.hasVariable(variable) -> Divide(diffX, y)
            else -> Divide(Minus(Multiply(y, diffX), Multiply(x, diffY)), Multiply(y, y))
        }
    }
}

data class Pow(override val x: Expr, override val y: Expr) : BinOp(x, y) {
    override fun eval(env: Env): Expr {
        val p = evalXY(env)
        val x = p.first
        val y = p.second
        return when {
            (x is Value && y is Value) -> Value(x.value.pow(y.value))
            (x is Value && (x.value == 0.0 || x.value == 1.0)) -> Value(x.value)
            (y is Value && y.value == 0.0) -> Value(1.0)
            (y is Value && y.value == 1.0) -> x
            else -> Pow(x, y)
        }
    }

    // dz x^y = x^y * (xdz * y / x + ydz * ln x)
    override fun diff(variable: Variable): Expr {
        val diffX = x.diff(variable)
        val diffY = y.diff(variable)
        return when {
            !y.hasVariable(variable) -> Multiply(y, Multiply(diffX, Pow(x, Minus(y, Value(1.0)))))
            !x.hasVariable(variable) -> Multiply(
                Pow(x, y),
                Multiply(diffY, Ln(x))
            )
            else -> Multiply(
                Pow(x, y),
                Plus(
                    Multiply(diffX, Divide(y, x)),
                    Multiply(diffY, Ln(x))
                )
            )
        }
    }
}

// y - base, x - value
data class Log(override val x: Expr, override val y: Expr) : BinOp(x, y) {
    override fun eval(env: Env): Expr {
        val p = evalXY(env)
        val x = p.first
        val y = p.second
        return when {
            (x is Value && y is Value) -> Value(log(x.value, y.value))
            (y is Value && (y.value <= 0.0 || y.value == 1.0)) -> throw ArithmeticException("Base of real logarithm can not be less or equal to zero or equal to one")
            (x is Value && x.value == 1.0) -> Value(0.0)
            (x is Value && x.value <= 0.0) -> throw ArithmeticException("Value in the logarithm should be positive")
            (x == y) -> Value(1.0)
            else -> Log(x, y)
        }
    }

    override fun diff(variable: Variable): Expr =
        when {
            !x.hasVariable(variable) -> UnMinus(Divide(Ln(x), Multiply(y, Ln(y))))
            !y.hasVariable(variable) -> Divide(Value(1.0), Multiply(x, Ln(y)))
            else -> Divide(Ln(x), Ln(y)).diff(variable)
        }
}

sealed class UnOp(open val x: Expr) : Expr() {
    fun evalX(env: Env): Expr = env.eval(x)
}

data class UnMinus(override val x: Expr) : UnOp(x) {
    override fun eval(env: Env): Expr {
        val x = evalX(env)
        return when {
            x is Value -> Value(-x.value)
            (x is UnMinus) -> env.eval(x.x)
            (x is Sin) -> Sin(UnMinus(x.x)).eval(env)
            else -> UnMinus(x)
        }
    }

    override fun diff(variable: Variable): Expr = UnMinus(x.diff(variable))
}

data class Exp(override val x: Expr) : UnOp(x) {
    override fun eval(env: Env): Expr {
        val x = evalX(env)
        return when {
            (x is Value) -> Value(exp(x.value))
            (x is Ln) -> env.eval(x.x)
            else -> Exp(x)
        }
    }

    override fun diff(variable: Variable): Expr {
        val diffX = x.diff(variable)
        return Multiply(Exp(x), diffX)
    }
}

data class Sin(override val x: Expr) : UnOp(x) {
    override fun eval(env: Env): Expr {
        val x = evalX(env)
        return when {
            (x is Value) -> Value(sin(x.value))
            else -> Sin(x)
        }
    }

    override fun diff(variable: Variable): Expr {
        val diffX = x.diff(variable)
        return Multiply(diffX, Cos(x))
    }
}

data class Cos(override val x: Expr) : UnOp(x) {
    override fun eval(env: Env): Expr {
        val x = evalX(env)
        return when {
            (x is Value) -> Value(cos(x.value))
            else -> Cos(x)
        }
    }

    override fun diff(variable: Variable): Expr {
        val diffX = x.diff(variable)
        return UnMinus(Multiply(diffX, Sin(x)))
    }
}

data class Ln(override val x: Expr) : UnOp(x) {
    override fun eval(env: Env): Expr {
        val x = evalX(env)
        return when {
            (x is Value) -> Value(ln(x.value))
            (x is Exp) -> env.eval(x.x)
            else -> Ln(x)
        }
    }

    override fun diff(variable: Variable): Expr {
        val diffX = x.diff(variable)
        return Divide(diffX, x)
    }
}

typealias Env = MutableMap<Variable, Value>

fun emptyEnv(): Env = emptyMap<Variable, Value>().toMutableMap()

fun Env.eval(expr: Expr): Expr = expr.eval(this)
fun Env.diffAndEval(expr: Expr, variable: Variable): Expr = this.eval(expr.diff(variable))

fun Expr.hasVariable(variable: Variable): Boolean =
    when(this) {
        is Value -> false
        is Variable -> this == variable
        is BinOp -> this.x.hasVariable(variable) || this.y.hasVariable(variable)
        is UnOp -> this.x.hasVariable(variable)
    }