import org.junit.Test
import kotlin.ArithmeticException
import kotlin.math.*
import kotlin.test.assertFailsWith
import kotlin.test.junit.JUnitAsserter.assertEquals

class TestEval {
    private fun testEvaluation(expr: Expr, expected: Expr, env: Env = emptyEnv()) {
        val actual = env.eval(expr)
        assertEquals(message = null, expected = expected, actual = actual)
    }

    @Test
    fun `2 = 2`() = testEvaluation(
        expr = Value(2.0),
        expected = Value(2.0)
    )

    @Test
    fun `x = x`() = testEvaluation(
        expr = Variable("x"),
        expected = Variable("x")
    )

    @Test
    fun `x, {x = 10} = 10`() = testEvaluation(
        expr = Variable("x"),
        expected = Value(10.0),
        env = mutableMapOf(Variable("x") to Value(10.0))
    )

    @Test
    fun `1,5 + 3,5 = 5`() = testEvaluation(
        expr = Plus(Value(1.5), Value(3.5)),
        expected = Value(5.0)
    )

    @Test
    fun `x + y, {x = 1, y = 4} = 5`() = testEvaluation(
        expr = Plus(Variable("x"), Variable("y")),
        expected = Value(5.0),
        env = mutableMapOf(Variable("x") to Value(1.0), Variable("y") to Value(4.0))
    )

    @Test
    fun `(x + y) + (x + y) = 2 * (x + y)`() = testEvaluation(
        expr = Plus(Plus(Variable("x"), Variable("y")), Plus(Variable("x"), Variable("y"))),
        expected = Multiply(Value(2.0), Plus(Variable("x"), Variable("y")))
    )

    @Test
    fun `x + y, {y = 0} = x`() = testEvaluation(
        expr = Plus(Variable("x"), Variable("y")),
        expected = Variable("x"),
        env = mutableMapOf(Variable("y") to Value(0.0))
    )

    @Test
    fun `1,5 - 3,5 = -2`() = testEvaluation(
        expr = Minus(Value(1.5), Value(3.5)),
        expected = Value(-2.0)
    )

    @Test
    fun `x - y, {x = 1, y = 4} = -3`() = testEvaluation(
        expr = Minus(Variable("x"), Variable("y")),
        expected = Value(-3.0),
        env = mutableMapOf(Variable("x") to Value(1.0), Variable("y") to Value(4.0))
    )

    @Test
    fun `(x + y) - (y + x) = 0`() = testEvaluation(
        expr = Minus(Plus(Variable("x"), Variable("y")), Plus(Variable("y"), Variable("x"))),
        expected = Value(0.0)
    )

    @Test
    fun `x - y, {y = 0} = x`() = testEvaluation(
        expr = Minus(Variable("x"), Variable("y")),
        expected = Variable("x"),
        env = mutableMapOf(Variable("y") to Value(0.0))
    )

    @Test
    fun `2 * 3,5 = 7`() = testEvaluation(
        expr = Multiply(Value(2.0), Value(3.5)),
        expected = Value(7.0)
    )

    @Test
    fun `x * y, {x = 1, y = 4} = 4`() = testEvaluation(
        expr = Multiply(Variable("x"), Variable("y")),
        expected = Value(4.0),
        env = mutableMapOf(Variable("x") to Value(1.0), Variable("y") to Value(4.0))
    )

    @Test
    fun `(x + y) * (y + x) = (x + y)^2`() = testEvaluation(
        expr = Multiply(Plus(Variable("x"), Variable("y")), Plus(Variable("y"), Variable("x"))),
        expected = Pow(Plus(Variable("x"), Variable("y")), Value(2.0))
    )

    @Test
    fun `x * y, {y = 0} = 0`() = testEvaluation(
        expr = Multiply(Variable("x"), Variable("y")),
        expected = Value(0.0),
        env = mutableMapOf(Variable("y") to Value(0.0))
    )

    @Test
    fun `x * y, {y = 1} = x`() = testEvaluation(
        expr = Multiply(Variable("x"), Variable("y")),
        expected = Variable("x"),
        env = mutableMapOf(Variable("y") to Value(1.0))
    )

    @Test
    fun `10,5 div 3,5 = 3`() = testEvaluation(
        expr = Divide(Value(10.5), Value(3.5)),
        expected = Value(3.0)
    )

    @Test
    fun `x div y, {x = 1, y = 4} = 0,25`() = testEvaluation(
        expr = Divide(Variable("x"), Variable("y")),
        expected = Value(0.25),
        env = mutableMapOf(Variable("x") to Value(1.0), Variable("y") to Value(4.0))
    )

    @Test
    fun `(x + y) div (y + x) = 1`() = testEvaluation(
        expr = Divide(Plus(Variable("x"), Variable("y")), Plus(Variable("x"), Variable("y"))),
        expected = Value(1.0)
    )

    @Test
    fun `x div y, {y = 0} = 0`() {
        val expr = Divide(Variable("x"), Variable("y"))
        val env = mutableMapOf(Variable("y") to Value(0.0))
        assertFailsWith<ArithmeticException> {
            env.eval(expr)
        }
    }

    @Test
    fun `x div y, {y = 1} = x`() = testEvaluation(
        expr = Divide(Variable("x"), Variable("y")),
        expected = Variable("x"),
        env = mutableMapOf(Variable("y") to Value(1.0))
    )

    @Test
    fun `9^0,5 = 3`() = testEvaluation(
        expr = Pow(Value(9.0), Value(0.5)),
        expected = Value(3.0)
    )

    @Test
    fun `x^y, {x = 1, y = 4} = 1`() = testEvaluation(
        expr = Pow(Variable("x"), Variable("y")),
        expected = Value(1.0),
        env = mutableMapOf(Variable("x") to Value(1.0), Variable("y") to Value(4.0))
    )

    @Test
    fun `x^y, {y = 0} = 1`() = testEvaluation(
        expr = Pow(Variable("x"), Variable("y")),
        expected = Value(1.0),
        env = mutableMapOf(Variable("y") to Value(0.0))
    )

    @Test
    fun `x^1 = x`() = testEvaluation(
        expr = Pow(Variable("x"), Value(1.0)),
        expected = Variable("x")
    )

    @Test
    fun `log_9(3) = 0,5`() = testEvaluation(
        expr = Log(Value(3.0), Value(9.0)),
        expected = Value(0.5)
    )

    @Test
    fun `log_y(x), {x = 4, y = 2} = 2`() = testEvaluation(
        expr = Log(Variable("x"), Variable("y")),
        expected = Value(2.0),
        env = mutableMapOf(Variable("x") to Value(4.0), Variable("y") to Value(2.0))
    )

    @Test
    fun `log_y(x), {x = 0} = ArithmeticException`() {
        val expr = Log(Variable("x"), Variable("y"))
        val env = mutableMapOf(Variable("x") to Value(0.0))

        assertFailsWith<java.lang.ArithmeticException> {
            env.eval(expr)
        }
    }

    @Test
    fun `log_y(x), {y = 1} = ArithmeticException`() {
        val expr = Log(Variable("x"), Variable("y"))
        val env = mutableMapOf(Variable("y") to Value(1.0))

        assertFailsWith<java.lang.ArithmeticException> {
            env.eval(expr)
        }
    }

    @Test
    fun `-(1,5) = -1,5`() = testEvaluation(
        expr = UnMinus(Value(1.5)),
        expected = Value(-1.5)
    )

    @Test
    fun `-(x), {x = 1} = -1`() = testEvaluation(
        expr = UnMinus(Variable("x")),
        expected = Value(-1.0),
        env = mutableMapOf(Variable("x") to Value(1.0))
    )

    @Test
    fun `Cos(1,5) = cos(1,5)`() = testEvaluation(
        expr = Cos(Value(1.5)),
        expected = Value(cos(1.5))
    )

    @Test
    fun `Sin(1,5) = sin(1,5)`() = testEvaluation(
        expr = Sin(Value(1.5)),
        expected = Value(sin(1.5))
    )

    @Test
    fun `Exp(5) = exp(5)`() = testEvaluation(
        expr = Exp(Value(5.0)),
        expected = Value(exp(5.0))
    )

    @Test
    fun `Ln(e) = 1`() = testEvaluation(
        expr = Ln(Value(E)),
        expected = Value(1.0)
    )

    @Test
    fun `2 * cos(x) + exp(x), x = 2`() = testEvaluation(
        expr = Plus(
            Multiply(
                Value(2.0),
                Cos(Variable("x"))
            ),
            Exp(Variable("x"))
        ),
        expected = Value(2.0 * cos(2.0) + exp(2.0)),
        env = mutableMapOf(Variable("x") to Value(2.0))
    )

    @Test
    fun `(2 * 3) * (x * x) = 4x^2`() = testEvaluation(
        expr = Multiply(
            Multiply(
                Value(2.0),
                Value(3.0)
            ),
            Multiply(
                Variable("x"),
                Variable("x")
            )
        ),
        expected = Multiply(Value(6.0), Pow(Variable("x"), Value(2.0)))
    )

    @Test
    fun `cos^2x + sin^2x, {x = 5} = 1`() = testEvaluation(
        expr = Plus(
            Pow(
                Cos(Variable("x")),
                Value(2.0)
            ),
            Pow(
                Sin(Variable("x")),
                Value(2.0)
            )
        ),
        expected = Value(1.0),
        env = mutableMapOf(Variable("x") to Value(1.0))
    )

    @Test
    fun `exp(ln(x)) = x`() = testEvaluation(
        expr = Exp(Ln(Variable("x"))),
        expected = Variable("x")
    )

    @Test
    fun `(x + x) div (y * y) = (2 * x) div y^2`() = testEvaluation(
        expr = Divide(
            Plus(Variable("x"), Variable("x")),
            Multiply(Variable("y"), Variable("y"))
        ),
        expected = Divide(
            Multiply(Value(2.0), Variable("x")),
            Pow(Variable("y"), Value(2.0))
        )
    )

    @Test
    fun `(-x) + x = 0`()  = testEvaluation(
        expr = Plus(
            UnMinus(Variable("x")),
            Variable("x")
        ),
        expected = Value(0.0)
    )
}

class TestDiff {
    private fun testDifferentiation(expr: Expr, expected: Expr, variable: Variable = Variable("x")) {
        val actual = expr.diff(variable).eval(emptyEnv())
        assertEquals(
            message = null,
            expected = expected,
            actual = actual
        )
    }

    @Test
    fun `cdx = 1`() = testDifferentiation(
        expr = Value(2.0),
        expected = Value(0.0)
    )

    @Test
    fun `xdx = 1`() = testDifferentiation(
        expr = Variable("x"),
        expected = Value(1.0),
        variable = Variable("x")
    )

    @Test
    fun `ydx = 0`() = testDifferentiation(
        expr = Variable("y"),
        expected = Value(0.0),
        variable = Variable("x")
    )

    @Test
    fun `(x + y)dx = 1`() = testDifferentiation(
        expr = Plus(Variable("x"), Variable("y")),
        expected = Value(1.0),
        variable = Variable("x")
    )

    @Test
    fun `(y - x)dx = -1`() = testDifferentiation(
        expr = Minus(Variable("y"), Variable("x")),
        expected = Value(-1.0),
        variable = Variable("x")
    )

    @Test
    fun `(x * x)dx = 2 * x`() = testDifferentiation(
        expr = Multiply(Variable("x"), Variable("x")),
        expected = Multiply(Value(2.0), Variable("x")),
        variable = Variable("x")
    )

    @Test
    fun `(x div x)dx = 0`() = testDifferentiation(
        expr = Divide(Variable("x"), Variable("x")),
        expected = Value(0.0),
        variable = Variable("x")
    )

    @Test
    fun `(x div y)dy = -x div y^2`() = testDifferentiation(
        expr = Divide(Variable("x"), Variable("y")),
        expected = UnMinus(Divide(Variable("x"), Pow(Variable("y"), Value(2.0)))),
        variable = Variable("y")
    )

    @Test
    fun `(x ^ y)dx = y * x^(y - 1)`() = testDifferentiation(
        expr = Pow(Variable("x"), Variable("y")),
        expected = Multiply(Variable("y"), Pow(Variable("x"), Minus(Variable("y"), Value(1.0)))),
        variable = Variable("x")
    )

    @Test
    fun `x^y dy = x^y * ln x`()  = testDifferentiation(
        expr = Pow(Variable("x"), Variable("y")),
        expected = Multiply(Pow(Variable("x"), Variable("y")), Ln(Variable("x"))),
        variable = Variable("y")
    )

    @Test
    fun `log_y (x) dx = 1 div (x * ln y)`() = testDifferentiation(
        expr = Log(Variable("x"), Variable("y")),
        expected = Divide(Value(1.0), Multiply(Variable("x"), Ln(Variable("y")))),
        variable = Variable("x")
    )

    @Test
    fun `log_y (x) dy = - ln x div y * (ln y)^2`() = testDifferentiation(
        expr = Log(Variable("x"), Variable("y")),
        expected = UnMinus(Divide(
            Ln(Variable("x")),
            Multiply(
                Variable("y"),
                Ln(Variable("y"))
            )
        )),
        variable = Variable("y")
    )

    @Test
    fun `cos(x * y) dx = -y * sin(x * y)`() = testDifferentiation(
        expr = Cos(Multiply(Variable("x"), Variable("y"))),
        expected = UnMinus(Multiply(Variable("y"), Sin(Multiply(Variable("x"), Variable("y"))))),
        variable = Variable("x")
    )

    @Test
    fun `sin(x^2) dx = 2x * cos(x^2)`() = testDifferentiation(
        expr = Sin(Pow(Variable("x"), Value(2.0))),
        expected = Multiply(Multiply(Value(2.0), Variable("x")), Cos(Pow(Variable("x"), Value(2.0)))),
        variable = Variable("x")
    )

    @Test
    fun `e^(x + y) dx = e^(x + y)`() = testDifferentiation(
        expr = Exp(Plus(Variable("x"), Variable("y"))),
        expected = Exp(Plus(Variable("x"), Variable("y"))),
        variable = Variable("x")
    )

    @Test
    fun `ln(2x + e^x) dx = (2 + e^x) div (2x + e^x)`() = testDifferentiation(
        expr = Ln(Plus(Multiply(Value(2.0), Variable("x")), Exp(Variable("x")))),
        expected = Divide(Plus(Value(2.0), Exp(Variable("x"))), Plus(Multiply(Value(2.0), Variable("x")), Exp(Variable("x")))),
        variable = Variable("x")
    )
}

class TestDiffAndEval {
    private fun testDifferentiationAndEvaluation(expr: Expr, expected: Expr, variable: Variable = Variable("x"), env: Env = emptyEnv()) {
        val actual = env.diffAndEval(expr, variable)
        assertEquals(
            message = null,
            expected = expected,
            actual = actual
        )
    }

    @Test
    fun `(x - 5)(2x - 5)dx = 4x - 15, {x = 15} = 45`() = testDifferentiationAndEvaluation(
        expr = Multiply(Minus(Variable("x"), Value(5.0)), Minus(Multiply(Value(2.0), Variable("x")), Value(5.0))),
        expected = Value(45.0),
        variable = Variable("x"),
        env = mutableMapOf(Variable("x") to Value(15.0))
    )

    @Test
    fun `(x - 5) div (2x - 5) dx = 5 div (2x - 5)^2, {x = 5} = 0,2`()  = testDifferentiationAndEvaluation(
        expr = Divide(Minus(Variable("x"), Value(5.0)), Minus(Multiply(Value(2.0), Variable("x")), Value(5.0))),
        expected = Value(0.2),
        variable = Variable("x"),
        env = mutableMapOf(Variable("x") to Value(5.0))
    )

    @Test
    fun `(2x - 1)(x^0,5) dx = (6x - 1) div (2x^0,5), {x = 1} = 2,5`() = testDifferentiationAndEvaluation(
        expr = Multiply(Minus(Multiply(Value(2.0), Variable("x")), Value(1.0)), Pow(Variable("x"), Value(0.5))),
        expected = Value(2.5),
        variable = Variable("x"),
        env = mutableMapOf(Variable("x") to Value(1.0))
    )

    @Test
    fun `x^0,5 * cos x * sin x dx, {x = Pi} = sqrt(Pi)`() = testDifferentiationAndEvaluation(
        expr = Multiply(Pow(Variable("x"), Value(0.5)), Multiply(Cos(Variable("x")), Sin(Variable("x")))),
        expected = Value(sqrt(PI)),
        variable = Variable("x"),
        env = mutableMapOf(Variable("x") to Value(PI))
    )

    @Test
    fun `3e^x - 3^x dx = 3e^x - 3^x * ln3, {x = 0} = 3 - ln3`() = testDifferentiationAndEvaluation(
        expr = Minus(Multiply(Value(3.0), Exp(Variable("x"))), Pow(Value(3.0), Variable("x"))),
        expected = Value(3 - ln(3.0)),
        variable = Variable("x"),
        env = mutableMapOf(Variable("x") to Value(0.0))
    )

    @Test
    fun `4 div (xe^x) dx = - (4 + 4x) div (x^2e^x), {x = 1} = -8 div e`() = testDifferentiationAndEvaluation(
        expr = Divide(Value(4.0), Multiply(Variable("x"), Exp(Variable("x")))),
        expected = Value(-8.0 / E),
        variable = Variable("x"),
        env = mutableMapOf(Variable("x") to Value(1.0))
    )

    @Test
    fun `sin((y div x^3)^0,5) dx, {x = 1, y = PI^2} = -1,5 sqrt(y div x^5) cos(sqrt(y div x^3)) = 1,5*PI^2`() = testDifferentiationAndEvaluation(
        expr = Sin(Pow(
            Divide(
                Variable("y"),
                Pow(Variable("x"), Value(3.0))
            ),
            Value(0.5)
        )),
        expected = Value(1.5 * PI),
        variable = Variable("x"),
        env = mutableMapOf(Variable("x") to Value(1.0), Variable("y") to Value(PI.pow(2)))
    )
}