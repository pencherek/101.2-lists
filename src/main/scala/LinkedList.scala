
//Linked list where head contain a node(string, node)
class LinkedList(var head: Node){
  def this(){
    this(null)
  }
  //add a new node at the start of the list
  def addToStart(s: String): Unit = {
    head = new Node(s, head)
  }
  def addToEnd(s: String): Unit = {
    if (head == null){
      addToStart(s)
    }else{
      getLastElement().next = new Node(s, null)
    }
  }
  def insertAfter(s: String, e: String): Unit ={
    if (head != null){
      val temp = findElement(s)
      if (temp != null){
        val temp = findElement(s).next
        findElement(s).next = new Node(e, temp)
      }
    }
  }
  def getSize(): Int = {
    return getSize(head)
  }
  private def getSize(n: Node): Int = {
    if (head == null) return 0
    return if (n.next != null) 1 + getSize(n.next) else 1
  }
  override def toString: String = {
    try {
      return f"($getSize) " + makeString(head)
    }catch{
      case e: Exception => "0"
    }
  }
  //create a recursive String
  private def makeString(n: Node): String = {
    return if (n.next != null) n.item + " -> " + makeString(n.next) else n.item
  }
  //remove the first node and return it
  def removeFirstElement(): Node = {
    try{
      val temp = head
      head = head.next
      return temp
    }catch{
      case e: Exception => null
    }
  }
  def removeLastElement(): Node = {
    try {
      if (getSize() == 1){
        head = null
        return null
      }else{
        val temp = getLastElement()
        getBeforeLastElement(head).next = null
        return temp
      }
    } catch {
      case e: Exception => null
    }
  }
  def removeElement(s: String): Node = {
    try {
      val temp = findElement(s)
      if (head.item == s){
        head = head.next
      }else{
        findElementBefore(s, head).next = temp.next
      }
      return temp
    } catch {
      case e: Exception => null
    }
  }
  def getLastElement(): Node = {
    try {
      return getLastElement(head)
    }catch{
      case e: Exception => null
    }
  }
  private def getLastElement(n: Node): Node = {
    return if (n.next != null) getLastElement(n.next) else n
  }
  private def getBeforeLastElement(n: Node): Node = {
    if (n.next == null) return n
    return if (n.next.next != null) getBeforeLastElement(n.next) else n
  }
  def isPresent(s: String): Boolean = {
    try {
      return isPresent(s, head)
    }catch{
      case e: Exception => false
    }
  }
  private def isPresent(s: String, n: Node): Boolean = {
    return if (s == n.item) true else if (n.next != null) isPresent(s, n.next) else false
  }
  //return the first node where item == s
  def findElement(s: String): Node = {
    try {
      return findElement(s, head)
    }
    catch {
      case e: Exception => null
    }
  }
  private def findElement(s: String, n: Node): Node = {
    return if (s == n.item) n else if (n.next != null) findElement(s, n.next) else null
  }
  private def findElementBefore(s: String, n: Node): Node = {
    return if (s == n.next.item) n else if (n.next != null) findElementBefore(s, n.next) else null
  }
  def swapElements(s1: String, s2: String): Unit ={
      val n1 = findElement(s1)
      val n2 = findElement(s2)
      if (n1 != null && n2 != null){
        n1.item = s2
        n2.item = s1
      }
  }
}
