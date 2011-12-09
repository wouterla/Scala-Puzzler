package org.geenz.puzzler
import scala.collection.mutable.ArrayBuffer

class Piece(pieceWidth: Int, pieceHeight: Int, initValues: Map[(Int, Int), Char]) extends Grid {
  
  values = initValues
  width = pieceWidth
  height = pieceHeight

  def this(initString: String) = {
    this(0, 0, Map.empty[(Int, Int), Char])
    width = getWidthFromInitString(initString)
    height = getHeightFromInitString(initString)
    values = convertStringToMap(initString)
  }
  
  def getRotations(): Map[Int, Piece] = {
    var allRotations = Map.empty[Int, Piece]
    allRotations += 0 -> this
    allRotations += 90 -> rotateBy90(this)
    allRotations += 180 -> rotateBy90(allRotations(90))
    allRotations += 270 -> rotateBy90(allRotations(180))
    allRotations
  }
  
  def rotateBy90(toRotate: Piece): Piece = {
    var newValues = Map.empty[(Int, Int), Char]
    toRotate.values.foreach( tuple => {
      val ((x, y), value) = tuple
      var newX = y
      var newY = toRotate.width + 1 - x
      newValues += (newX, newY) -> value
    })
    new Piece(toRotate.height, toRotate.width, newValues)
  }
  
  def flip(): Piece = {
    var newValues = Map.empty[(Int, Int), Char]
    values.foreach( tuple => {
      val ((x, y), value) = tuple
      var newY = height - y + 1
      newValues += (x, newY) -> value
    })
    new Piece(width, height, newValues)
  }

  def orientations(): Array[Piece] = {
    var allOrientations = ArrayBuffer.empty[Piece]
    getRotations().foreach( tuple => {
      var (key, piece) = tuple
      allOrientations += piece
      allOrientations += piece.flip
    })
    allOrientations.toArray[Piece]
  }
    
  def uniqueOrientations(): Set[Piece] = {
    orientations().toSet[Piece]    
  }
  
  override def equals(that: Any): Boolean = that match {
    case that: Piece => {
      that.values.foreach( tuple => {
        var (key, value) = tuple        
        try {
        	if (this.values(key) != that.values(key)) return false
        } catch {
          case _ => return false
        }        
      })
      return true
    }
    case _ => false    
  }
  
  def getColor(): Char = {
    values.foreach( tuple => {
      var (key, value) = tuple
      if (value != EMPTY) {
        return value
      }
    })
    EMPTY
  }
}