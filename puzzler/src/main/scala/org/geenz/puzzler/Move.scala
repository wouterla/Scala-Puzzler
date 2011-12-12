package org.geenz.puzzler

case class Move(score: Int, piece: Piece, x: Int, y: Int) {
   
  override def equals(that: Any): Boolean = that match {
    case that: Move => {
      if (score == that.score
          && x == that.x
          && y == that.y
          && piece == that.piece) {
        return true
      }
      return false
    }
    case _ => false    
  }

}