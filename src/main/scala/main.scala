import java.io.File

object main {
  def main(args: Array[String]): Unit = {
    val neuralNetwork = new NeuralNetwork()
    neuralNetwork.readCharactersFromFile("/char_profiles_alphabet.txt")
    neuralNetwork.readTestingData()
    neuralNetwork.setupNetwork()
    for(i <- 0 until 5000) {
      printf("Training # " + i + " : ")
      neuralNetwork.reset()
      neuralNetwork.trainSet()
      printf(neuralNetwork.SSE + "\n")
    }
  }

}
