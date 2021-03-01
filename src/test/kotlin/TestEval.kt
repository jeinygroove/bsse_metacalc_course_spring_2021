import org.junit.Test
import kotlin.math.E
import kotlin.math.cos
import kotlin.math.exp
import kotlin.math.sin
import kotlin.test.assertFailsWith
import kotlin.test.junit.JUnitAsserter

class TestEval {
    private fun testEvaluation(expr: Expr, expected: Expr, env: Env = emptyEnv()) {
        val actual = env.eval(expr)
        JUnitAsserter.assertEquals(message = null, expected = expected, actual = actual)
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