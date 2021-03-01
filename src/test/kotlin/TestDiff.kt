import org.junit.Test
import kotlin.test.junit.JUnitAsserter

class TestDiff {
    private fun testDifferentiation(expr: Expr, expected: Expr, variable: Variable = Variable("x")) {
        val actual = expr.diff(variable).eval(emptyEnv())
        JUnitAsserter.assertEquals(
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