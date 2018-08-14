import java.io.InputStream

import scala.io.Source

class NeuralNetwork {
  // Accuracy
  val accuracy = 0.8

  // Parameter holding learning rate variable
  val learningRate = 0.1

  // Parameter holding number of neurons in each layer
  val HiddenLayerSize = 100
  val OutputLayerSize = 5

  // Characters container
  var characters: Map[String, Character] = Map()

  // Training set container
  val trainingSet: List[String] = List()

  // create hidden layer container
  var hiddenLayer: List[Neuron] = List()
  // create output layer container
  var outputLayer: List[Neuron] = List()

  // Holder of latest SSE of training
  var SSE: Double = 0

  def reset() : Unit = {
    SSE = 0
    hiddenLayer.foreach(f => f.reset())
    outputLayer.foreach(f => f.reset())
  }

  /**
    * Read characters from file and create Character map
    *
    * @param resource file name
    * @return
    */
  def readCharactersFromFile(resource: String): Map[String, Character] = {

//  Read iterator from characters text file
    val inputStream = getClass.getResourceAsStream(resource)
    val bufferedSource = Source.fromInputStream(inputStream)
    val readmeText: Iterator[String] = bufferedSource.getLines

    // Create characters from text file
    var char: Character = null
    readmeText.foreach(f => {

      // For each line input, check if it is a 'B' or a '0,0,1...' string
      if (f.length != 0) {
        if (f.length == 1) {

          // Check for first run
          if (char != null) {
            characters = characters + (char.desired -> char)
          }

          // Create new char since we have a first letter
          char = new Character()

          // Set character desired value
          char.setDesired(f)
        } else {
          // Add current vector string to character vectors
          char.addVector(f.split(',').map(c => c.toDouble).toList)
        }
      }

      // Add last letter to character map
      characters = characters + (char.desired -> char)
    })

    // Return characters
    characters
  }

  /**
    * Read prediction data from file
    */
  def readTestingData(): Unit = {

  }

  /**
    * Create neural network layers and add to neuralNetwork container
    */
  def setupNetwork(): Unit = {

    // setup all neurons in hidden layer
    for (i <- 0 until HiddenLayerSize) {
      val n = new Neuron(learningRate, accuracy)
      n.init(characters.head._2.vectorWidth())
      hiddenLayer =  n :: hiddenLayer
    }

    // setup all neurons in output layer
    for (i <- 0 until OutputLayerSize) {
      val out = new Neuron(learningRate, accuracy)
      out.init(HiddenLayerSize)
      outputLayer = out :: outputLayer
    }
  }

  def trainSet(): Unit = {
    // for every character to train
    for (char <- characters) {

      // Feed Forward algorithm:
      // Evaluate on all hidden layer neurons
      for ((neuronJ, index) <- hiddenLayer.zipWithIndex){
        // pass through to hidden layer, for each neuron, will receive all values
        // We do not want to calculate the SSE for hidden the hidden layer
        neuronJ.evaluate(char._2.getVectors)
      }

      // Get all hidden layer outputs and compile it into a list
      val hiddenLayerOutput: List[Double] = hiddenLayer.map(neuronJ => neuronJ.output)

      // Evaluate output layer neurons
      for((neuronK, index) <- outputLayer.zipWithIndex) {

        // Pass through the computed hidden layer values to each output neuron
        neuronK.evaluate(hiddenLayerOutput)

        //Calculate SSE of output layer as we are concerned with the accuracy
        neuronK.calculateSSE(char._2.desiredCharBinary()(index))

        // Backward Propagation (with Gradient Decent):
        // Update output layer neuron weights
        neuronK.updateWeights()(Nil)(Nil)
      }

      // Backward Propagation (with Gradient Decent):
      // Update all hidden neurons' weights
      hiddenLayer.foreach(neuronJ => {
        neuronJ.updateWeights(true)(outputLayer.map(f => f.delta))(outputLayer.map(f => f.output))
      })
    }

    // SSE after training a set
    val sum = outputLayer.map(k => k.sumSquareError).sum
    SSE = ( sum / OutputLayerSize) * 0.5
  }


}
