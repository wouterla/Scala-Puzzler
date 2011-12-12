package org.geenz.puzzler

trait Grid {

  var values: Map[(Int, Int), Char] = initEmptyGrid()
  var width = 11
  var height = 5
  val EMPTY = '0'
  val EDGE = '|'

  def convertStringToMap(initString: String): Map[(Int, Int), Char] = {
    var y = height
    var x = 1
    var values = Map.empty[(Int, Int), Char]
    initString.foreach(char => {
      if (char == '\n') {
        y -= 1
        x = 1
      } else {
        values += (x, y) -> char
        x += 1
      }
    })
    values
  }
  
  def initGridWithMap(newWidth: Int, newHeight: Int, newValues: Map[(Int, Int), Char]) = {
    values = newValues
    height = newHeight
    width = newWidth
  }

  def initGridWithString(newWidth: Int, newHeight: Int, newValues: String) = {
    height = newHeight
    width = newWidth
    values = convertStringToMap(newValues)
  }

  def initEmptyGrid(): Map[(Int, Int), Char] = {
    var returnMap: Map[(Int, Int), Char] = Map.empty[(Int, Int), Char]
    for (y <- 1 to height) {
      for (x <- 1 to width) {
        returnMap += (x, y) -> EMPTY
      }
    }
    returnMap
  }
  
  def getWidthFromInitString(init:String): Int = {
    val rows = init.split("\n")
    rows(0).size
  }
  
  def getHeightFromInitString(init:String): Int = {
    val rows = init.split("\n")
    rows.length
  }

  def debugInfo(): String = {
    var str = new String
    str += "Width = " + width + "\n"
    str += "Height = " + height + "\n"
    str += "Values:"
    values.foreach{ case(key, value) => {
      val (x, y) = key
      str += "(" + x + ", " + y + ") -> " + value + "\n"
    }}
    str
  }
  
  override def toString(): String = {
    var str = new String    
    for (y <- height to 1 by -1) {
      for (x <- 1 to width) {
        str += values(x, y)
      }
      str += "\n"
    }
    str
  }
}