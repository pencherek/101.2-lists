
object Main {
  def main(args: Array[String]): Unit = {
      val des = Array("Vouvry", "Monthey", "Sion")
      val l = new LinkedList()
      l.addToStart("Vouvry")
      l.addToStart("Monthey")
      l.addToStart("Sion")
      l.addToEnd("St-Gingolph")
      println(l.getSize() + " " + l.toString)
      l.removeElement("Sion")
      println(l.getSize() + " " + l.toString)
  }
}