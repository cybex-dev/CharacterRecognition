import scala.util.Try

class Character {
  var vectors: List[Double] = List()
  var desired: String = ""

  def getVectors: List[Double] = vectors

  def getDesired: String = desired

  def vectorWidth(): Int = Try(vectors.length).getOrElse(0)

  def vectorsAsString(): String = vectors.mkString("")

  /**
    * Gets the 0/1 value at a specific index position in the stringified vectors list
    * @param index position of value to get
    * @return
    */
  def getVectorValueAtIndex(index: Int): Double = vectorsAsString().charAt(index).toDouble

  def addVector(vector: List[Double]) : Unit = vectors = vectors:::vector

  def setDesired(v: String) : Unit = this.desired = v

  def desiredCharBinary(): List[Double] = {
    desired match {
      case "A" => List(0,0,0,0,1)
      case "B" => List(0,0,0,1,0)
      case "C" => List(0,0,0,1,1)
      case "D" => List(0,0,1,0,0)
      case "E" => List(0,0,1,0,1)
      case "F" => List(0,0,1,1,0)
      case "G" => List(0,0,1,1,1)
      case "H" => List(0,1,0,0,0)
      case "I" => List(0,1,0,0,1)
      case "J" => List(0,1,0,1,0)
      case "K" => List(0,1,0,1,1)
      case "L" => List(0,1,1,0,0)
      case "M" => List(0,1,1,0,1)
      case "N" => List(0,1,1,1,0)
      case "O" => List(0,1,1,1,1)
      case "P" => List(1,0,0,0,0)
      case "Q" => List(1,0,0,0,1)
      case "R" => List(1,0,0,1,0)
      case "S" => List(1,0,0,1,1)
      case "T" => List(1,0,1,0,0)
      case "U" => List(1,0,1,0,1)
      case "V" => List(1,0,1,1,0)
      case "W" => List(1,0,1,1,1)
      case "X" => List(1,1,0,0,0)
      case "Y" => List(1,1,0,0,1)
      case "Z" => List(1,1,0,1,0)
      case _ => List()
    }
  }
}


































