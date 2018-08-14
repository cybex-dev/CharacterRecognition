import scala.util.Random

class Neuron(learningRate: Double, accuracy: Double) {

  // Define bias & weight
  var bias: Double = 1.0

  // Initialize with random variable between [0.00; 1.00]
  var biasWeight: Double = Random.nextGaussian()

  // List of weights
  var listWeights: Array[Double] = Array()

  // Temp holder for inputs
  var inputs: List[Double] = List()

  // Temp holder for output
  var output: Double = 0.0

  // Holder for desired value, value either [0, 1]
  var desiredValue: Double = 0.0

  // SSE
  var sumSquareError: Double = 0.0

  /**
    * Call this method to reset neuron after a training set
    */
  def newPattern = {
    inputs = List()
    sumSquareError = 0.0
    output = 0.0
  }


  private def sigmoid(sum: Double) = {
    // Calculate neuron activation value
    1 / (1 + Math.pow(Math.E, -1.0 * sum))
  }

  /**
    * Called with desired value which is used to update the weights using gradient decent
    */
  def updateWeights() = {

    // Update all weights
    for((weight, index) <- listWeights.zipWithIndex) {
      val newWeight = weight *  learningRate * output * (1 - output) * (desiredValue - output) * 2.0 * inputs(index)
      listWeights.updated(index, newWeight)
    }

    // Update bias weight
    biasWeight = biasWeight * learningRate * output * (1 - output) * (desiredValue - output) * 2.0 * bias

  }

  /**
    * Evaluates a single pattern input
    * During training, the updateWeights should be called to apply the new changed weights
    *
    * @param singleInput
    */
  def evaluate(singleInput: List[Double], desiredValue: Double) = {

    // Assign new inputs to temp holder for later usage
    inputs = singleInput

    // Train if SSE of last if bigger than the desired accuracy
    if(accuracy < sumSquareError) {

      // Initialize sum to 0
      var sum: Double = 0.0

      // Calculate weighted 'sum'
      for ((input, index) <- singleInput.zipWithIndex) {
        sum += input * listWeights(index)
      }

      // Add bias
      sum += bias * biasWeight

      // Activate 'sum'
      output = sigmoid(sum)

      // Add error to SSE
      sumSquareError += Math.pow(desiredValue - output, 2)

      //return calculated output
      output
    }
  }
}
