package generators

import org.hipparchus.analysis.MultivariateFunction
import org.hipparchus.optim.{InitialGuess, MaxEval, MaxIter, SimpleBounds}
import org.hipparchus.optim.linear.NonNegativeConstraint
import org.hipparchus.optim.nonlinear.scalar.noderiv.PowellOptimizer
import org.hipparchus.optim.nonlinear.scalar.{GoalType, ObjectiveFunction}

object MelodySolver extends App {

  val optimizer = new PowellOptimizer(0.1, 0.1)
  val multivariateFunction = new MultivariateFunction {
    override def value(point: Array[Double]): Double = {
      point.sum
    }
  }

  val objectiveFunction = new ObjectiveFunction(multivariateFunction)
  val goalType = GoalType.MINIMIZE
  val nonNegativeConstraint = new NonNegativeConstraint(true)
  val simpleBounds = new SimpleBounds(List.fill(5)(1.0).toArray, List.fill(5)(7.0).toArray)
  val initialGuess = new InitialGuess(List(1.0,2.0,3.0,4.0,5.0).toArray)
  val maxEval = new MaxEval(400)
  val maxIter = new MaxIter(400)

  optimizer.optimize(objectiveFunction, goalType, nonNegativeConstraint, initialGuess, maxEval)

}
