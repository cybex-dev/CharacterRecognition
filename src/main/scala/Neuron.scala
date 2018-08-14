import scala.util.Random

class Neuron(learningRate: Double, accuracy: Double) {

  // Define bias & weight
  var bias: Double = 1.0

  // Initialize with random variable between [0.00; 1.00]
  var biasWeight: Double = Random.nextGaussian()

  // List of weights
  var listWeights: List[Double] = List()

  // Temp holder for inputs
  var inputs: List[Double] = List()

  // Temp holder for output
  var output: Double = 0.0

  // SSE
  var sumSquareError: Double = 0.0

  // Target value (updated on each call to evaluate)
  var target: Double = 0

  // Temporary container for delta
  var delta: Double = 0

  def init(numInputs: Int): Unit = {
    for (i <- 0 until numInputs) {
      listWeights = Random.nextGaussian() :: listWeights
    }
  }

  /**
    * Call this method to reset neuron after a training set
    */
  def reset(): Neuron = {
    inputs = List()
    sumSquareError = 0.0
    this
  }

  private def sigmoid(sum: Double) = {
    // Calculate neuron activation value
    1.0 / (1.0 + Math.pow(Math.E, -1.0 * sum))
  }

  /**
    * Called with desired value which is used to update the weights using gradient decent
    */
  def updateWeights(isHidden: Boolean = false)(deltas: List[Double] = Nil)(outputWeights: List[Double] = Nil): Unit = {

    // Calculate constant deltaK
    val error = target - output
    val derivative = output * (1 - output)
    delta = derivative * error

    for ((weight, index) <- listWeights.zipWithIndex) {

      if (isHidden) {

        // Iterate through each error + sigmoid deritivite calculation AND weights going into a specific output neuron
        for (
          delta <- deltas;
          outputW <- outputWeights
        ) {

          // For each of the unputs into a specific hidden layer neuron
          for(input <- inputs) {

            // Calculate hidden layer weight updates
            val weightChange = -1.0 * delta * outputW * (1-output) * input
            val newWeight = weight - weightChange
            listWeights.updated(index, newWeight)
          }
        }
      } else {
        // Calculate output layer weight updates
        // Update all weights of neuron

        val weightChange = -1.0 * learningRate * delta * inputs(index)
        val newWeight = weight - weightChange
        listWeights.updated(index, newWeight)
      }

    }

    // Update bias weight
    val weightChangeK = -1.0 * learningRate * delta * bias
    biasWeight -= weightChangeK
  }

  /**
    * Calculates the SSE of a specific training pattern
    *
    * @param targetValue desired value
    */
  def calculateSSE(targetValue: Double): Unit = {
    // Save target value to temporary global holder (incase of weight update request)
    target = targetValue

    // Calculate error of current evaluation
    val error = targetValue - output

    // add square of error to SSE
    sumSquareError += Math.pow(error, 2)
  }

  /**
    * Evaluates a single pattern input
    * During training, the updateWeights should be called to apply the new changed weights
    *
    * @param singleInput a single input vector
    */
  def evaluate(singleInput: List[Double]): Unit = {

    // Assign new inputs to temp holder for later usage
    inputs = singleInput

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
  }
}
