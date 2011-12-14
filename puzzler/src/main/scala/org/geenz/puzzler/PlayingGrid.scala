package org.geenz.puzzler

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.immutable.Map
import scala.collection.immutable.TreeMap
import scala.collection.immutable.Stack
import scala.collection.immutable.SortedSet
import scala.collection.immutable.SortedMap

class PlayingGrid(val gridWidth:Int = 11, val gridHeight:Int = 5) extends Grid {
  
  height = gridHeight
  width = gridWidth
  values = initEmptyGrid()
    
  var _pieces = Set.empty[Piece]
  
  def pieces = _pieces
  def pieces_= (values:Set[Piece]): Unit = {
    _pieces = values
  }
  
  def this(width: Int, height: Int, initString: String) = {
    this(width, height)
    values = convertStringToMap(initString)
  }
  
  def this() {
    this(11, 5)
  }
 
  def place(x: Int, y: Int, piece: Piece): Boolean = {
    if (!pieceFits(x, y, piece)) return false
    for (((pieceX, pieceY), value) <- piece.values
        if (value != EMPTY)) {
      values += (absolutePosition(x, pieceX), absolutePosition(y, pieceY)) -> value
    }
    return true
  }
  
  def pieceFits(x: Int, y: Int, piece: Piece): Boolean = {
    for (((pieceX, pieceY), value) <- piece.values
        if (value != EMPTY)) {
    	val absX = absolutePosition(x, pieceX)
    	val absY = absolutePosition(y, pieceY)
    	if (!isOnGrid(absX, absY) || !isEmpty(absX, absY)) {
    	  return false      
    	}
    }
    return true
  }
  
  def absolutePosition(basePosition: Int, piecePosition: Int): Int = {
    basePosition + piecePosition - 1
  }
  
  def isOnGrid(x: Int, y: Int): Boolean = {
    ((x <= width)
    	&& (x > 0)
    	&& (y <= height)
    	&& (y > 0))
  }
  
  def isEmpty(x: Int, y: Int): Boolean = {
    values(x, y) == EMPTY
  }
  
  def findAllPlacesFor(piece: Piece): Array[(Int, Int)] = {
    var foundPlaces = new ArrayBuffer[(Int, Int)]
    for (((x, y), value) <- values
      if pieceFits(x, y, piece)) {
    	foundPlaces += ((x, y))
    }
    foundPlaces.toArray[(Int, Int)]
  }
  
  def findAllPlacesFor(pieces: Set[Piece]): Map[Piece, Array[(Int, Int)]] = {
    var found = for {piece <- pieces
        places = findAllPlacesFor(piece)
        if (!places.isEmpty)} yield {
       piece -> places
    }
    found.toMap
  }
  
  def generateAllPossibleOrientations(pieces: Set[Piece]): Set[Piece] = {
	  for (piece <- pieces; orientation <-piece.uniqueOrientations()) yield {
		  orientation
	  }
  }
  
  def remove(piece: Piece) {
    for ((pos, value) <- values
        if (value == piece.getColor)) values += pos -> EMPTY
  }
  
  def getNumberOfSurroundingFilledCells(x: Int, y: Int, piece: Piece): Int = {
    if (!pieceFits(x, y, piece)) {
      throw new NotEmptyException(x, y, piece, this)
    }
    
    var nr = 0
    var placedCells = getPlacedCells(x, y, piece)
    
    for (((placedX, placedY), value) <- placedCells) {
      var surroundingCells = getSurroundingCells(placedX, placedY)
      var edges = getEdges(placedX, placedY)
      for ((cellX, cellY) <- surroundingCells.keys
        if (surroundingCells(cellX, cellY) != EMPTY);
        if (!placedCells.contains(cellX, cellY) 
            || placedCells(cellX, cellY) == EMPTY)
      ) nr += 1
      nr += edges.size
    }
    nr
  }
  
  def getPlacedCells(gridX: Int, gridY: Int, piece: Piece): Map[(Int, Int), Char] = {
    var placedCells = Map.empty[(Int, Int), Char]
    piece.values.foreach( tuple => {
      var ((x, y), value) = tuple
      if (value != EMPTY) {
        var absX = absolutePosition(gridX, x)
        var absY = absolutePosition(gridY, y)
        if (isOnGrid(absX, absY)) {
          placedCells += (absX, absY) -> value
        }
      }
    })
    placedCells
  }
  
  def getSurroundingCells(x: Int, y: Int): Map[(Int, Int), Char] = {
    var surroundingCells = Map.empty[(Int, Int), Char]
    for ((cellX, cellY) <- surroundingCoordinates(x, y)
        if values.contains(cellX, cellY)) {
      surroundingCells += (cellX, cellY) -> values(cellX, cellY)
    }
    surroundingCells
  }
  
  def surroundingCoordinates(x: Int, y: Int): Array[(Int, Int)] = {
    Array((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1))
  }
  
  def getEdges(x: Int, y: Int): Map[(Int, Int), Char] = {
    var edges = Map.empty[(Int, Int), Char]
    for ((cellX, cellY) <- surroundingCoordinates(x, y)
        if (!isOnGrid(cellX, cellY))) {
      edges += (cellX, cellY) -> EDGE
    }
    edges
  }
  
  def sortPositionOnMostSidesConnected(pieces: Map[Piece, Array[(Int, Int)]]): 
	  Array[Move] = {
    
    var piecesMap = SortedMap.empty[Int, ArrayBuffer[Move]]
    var piecesSortedByConnections = ArrayBuffer.empty[Move]
    
    for ((piece, locations) <- pieces) {
      for ((x, y) <- locations) {
        var nr = getNumberOfSurroundingFilledCells(x, y, piece)
        if (!piecesMap.contains(nr)) {
          var arr = ArrayBuffer.empty[Move]
          piecesMap += nr -> arr
        }
        var newMove = new Move(nr, piece, x, y)
        piecesMap(nr) += newMove        
      }
    }
    var tmp = piecesMap.toSeq.sortWith(_._1 > _._1)
    for ((nr, moves) <- tmp ) {
      for (move <- moves) {
        piecesSortedByConnections += move
      }
    }
    piecesSortedByConnections.toArray
  }
  
  def getNextMove(pieces: Set[Piece]): Move = {
    var possibleMoves = findAllPlacesFor(pieces)
    var sortedMoves = sortPositionOnMostSidesConnected(possibleMoves)
    sortedMoves.last
  }
  
  def solve(pieces: Set[Piece]): PlayingGrid = {
    solve(pieces, getSortedMoves(pieces), this)
  }  
  
  def solve(pieces: Set[Piece], grid: PlayingGrid): PlayingGrid = {
    solve(pieces, grid.getSortedMoves(pieces), grid)
  }

  def solve(pieces: Set[Piece], moves: Array[Move], grid: PlayingGrid): PlayingGrid = {

    printStatus(grid, pieces, moves.size)
    
    if (grid.solved) return grid
    if (moves.isEmpty) return this
    
    var move = moves.head
    var newGrid = grid.copy()
    var placed = newGrid.place(move.x, move.y, move.piece)

    if (placed && !newGrid.failsHeuristics()) {
    	var resultGrid = newGrid.solve(removeColor(pieces, move.piece.getColor()), 
    			removeColor(moves, move.piece.getColor()), newGrid) //go depth
    	if (resultGrid.solved) {
    		return resultGrid
    	}    			
    }     
    return solve(pieces, moves.tail, grid.copy()) // go width	
  }
  
  def printStatus(grid: PlayingGrid, pieces: Set[Piece], openMoves: Int) {
    println("--------------------------")
    println(grid.toString())
    println("--------------------------")
    println("moves queued: " + openMoves)
    print("piece left  : ")
    for (piece <- pieces) print(piece.getColor)
    println(" (" + pieces.size + ")")
  }
  
  def failsHeuristics(): Boolean = {
    return hasSingleOpenSpace() || hasTwinOpenSpace()
  }
  
  def hasSingleOpenSpace(): Boolean = {
    for(((x, y), value) <- values
      if (isEmpty(x, y))) {    	
    	if (getSurroundingCount(x, y) == 4) return true
    }
    return false
  }
  
  def hasTwinOpenSpace(): Boolean = {
    for (((x, y), value) <- values
        if (isEmpty(x, y))) {
    		if (getSurroundingCount(x, y) == 3) {
    			var (nx, ny) = getFirstEmptyNeighbour(x, y)
    			if (getSurroundingCount(nx, ny) == 3) {
    				return true
    			}
    		}
    }
    false
  }
  
  def getFirstEmptyNeighbour(x: Int, y: Int): (Int, Int) = {
    for ((sx, sy) <- surroundingCoordinates(x, y)) {
    	  if (isOnGrid(sx, sy) && isEmpty(sx, sy)) {
    	    return (sx, sy)
    	  }
    }
    return null
  }
  
  def getSurroundingCount(x: Int, y: Int): Int = {
    	var surroundingCount = 0
    	for ((sx, sy) <- surroundingCoordinates(x, y)) {
    	  if (!isOnGrid(sx, sy)) {
    		  surroundingCount += 1 
    	  } else if (!isEmpty(sx, sy)) {
    	    surroundingCount += 1
    	  }
    	}
    	surroundingCount    
  }
  
  def removeColor(pieces: Set[Piece], color: Char): Set[Piece] = {
    var filteredPieces = Set.empty[Piece]
    for(piece <- pieces
      if (piece.getColor() != color)) {
    	filteredPieces += piece
    }
    filteredPieces
  }
  
  def removeColor(moves: Array[Move], color: Char): Array[Move] = {
    var filteredMoves = ArrayBuffer.empty[Move]
    for(move <- moves
      if (move.piece.getColor() != color)) {
    	filteredMoves += move
    }
    filteredMoves.toArray    
  }

  def numberOfEmptyCells() = {
    var total = 0
    for ((_, value) <- values 
      if (value == EMPTY)) {
        total += 1
    }
    total
  }  
  
  def solved() = (numberOfEmptyCells == 0)

  def copy(): PlayingGrid = {
    var copy = new PlayingGrid()
    copy.initGridWithMap(width, height, values)
    copy
  } 
  
  private def getPiecesNotYetUsed(pieces: Set[Piece]): Set[Piece] = {
    var piecesNotUsed = ArrayBuffer.empty[Piece]
    for (piece <- pieces
          if (!colorInUse(piece.getColor))) {
            piecesNotUsed += piece
      }
    piecesNotUsed.toSet
  }
  
  def colorInUse(color: Char): Boolean = {
    for (((_, _), cell) <-values
        if (cell == color)) {
      return true
    }
    return false
  }
  
  def getSortedMoves(pieces: Set[Piece]): Array[Move] = {
    
    var piecesToUse = getPiecesNotYetUsed(pieces)
    var orientations = DefaultPieces.allOrientationsFor(piecesToUse)
    var possibleMoves = findAllPlacesFor(orientations)
    var sortedMoves = sortPositionOnMostSidesConnected(possibleMoves)
    sortedMoves
  }
}


class NotEmptyException(x: Int, y: Int, piece: Piece, grid: Grid) extends RuntimeException {
  
  override def toString(): String = {
    return "NotEmptyException for: (" + x + ", " + y + ") -- \n" + 
    		piece + "\n --" + grid + "\n --"    		
  }
}

class NoSolutionFoundException extends RuntimeException {
  
  override def toString(): String = {
    return "No Solution found!"    		
  }
}