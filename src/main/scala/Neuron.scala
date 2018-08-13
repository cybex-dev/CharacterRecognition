import scala.util.Random

class Neuron {

  // Define bias & weight
  var bias: Double = 1.0

  // Initialize with random variable between [0.00; 1.00]
  var biasWeight: Double = Random.nextGaussian()

  // List of weights
  var listWeights: Array[Double] = Array()

  // SSE
  var sumSquareError: Double = 0.0

  def sigmoid(sum: Double) = {

  }

  def tanh(sum: Double) = {

  }

  def train(singleInput: List[Double]) = {

  }


}
