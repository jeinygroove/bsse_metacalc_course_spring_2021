import org.junit.Test
import kotlin.math.*
import kotlin.test.junit.JUnitAsserter.assertEquals

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