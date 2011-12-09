package org.geenz.puzzler

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.Map
import scala.collection.immutable.TreeMap
import scala.collection.immutable.Stack

class PlayingGrid(val gridWidth:Int = 11, val gridHeight:Int = 5) extends Grid {
  
  height = gridHeight
  width = gridWidth
  values = initEmptyGrid()
  
  var possiblePlacements = Map.empty[Piece, Array[(Int, Int)]]  
  var _pieces = Set.empty[Piece]
  
  def pieces = _pieces
  def pieces_= (values:Set[Piece]): Unit = {
    _pieces = values
    possiblePlacements = findAllPlacesFor(pieces)
  }
  
  def this(width: Int, height: Int, initString: String) = {
    this(width, height)
    values = convertStringToMap(initString)
  }
 
  def place(x: Int, y: Int, piece: Piece) = {
    if (!pieceFits(x, y, piece)) {
      throw new NotEmptyException(x, y, piece, this);
    }
    for (((pieceX, pieceY), value) <- piece.values
        if (value != EMPTY)) {
      values += (absolutePosition(x, pieceX), absolutePosition(y, pieceY)) -> value
    }
  }
  
  def pieceFits(x: Int, y: Int, piece: Piece): Boolean = {
    for (((pieceX, pieceY), value) <- piece.values
        if (value != EMPTY)) {
    	val absX = absolutePosition(x, pieceX)
    	val absY = absolutePosition(y, pieceY)
    	if (!positionIsOnGrid(absX, absY) || !isEmpty(absX, absY)) {
    	  return false      
    	}
    }
    return true
  }
  
  def absolutePosition(basePosition: Int, piecePosition: Int): Int = {
    basePosition + piecePosition - 1
  }
  
  def positionIsOnGrid(x: Int, y: Int): Boolean = {
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
    var mapOfFoundPlaces = Map.empty[Piece, Array[(Int, Int)]]
    pieces.foreach( piece => { 
      var places = findAllPlacesFor(piece)
      if (!places.isEmpty) mapOfFoundPlaces += piece -> places
    })
    mapOfFoundPlaces
  }
  
  def generateAllPossibleOrientations(pieces: Set[Piece]): Set[Piece] = {
    var foundOrientations = Set.empty[Piece]
    pieces.foreach( piece => {
      foundOrientations ++= piece.uniqueOrientations()
    })
    foundOrientations
  }
  
  def remove(piece: Piece) {
    values.foreach( tuple => {
      var (pos, value) = tuple
      if (value == piece.getColor()) {
        values += pos -> EMPTY
      }
    })
  }
  
  def getNumberOfConnectedCells(x: Int, y: Int, piece: Piece): Int = {
    if (!pieceFits(x, y, piece)) {
      throw new NotEmptyException(x, y, piece, this)
    }
    
    var nr = 0
    var placedCells = getPlacedCells(x, y, piece)
    
    placedCells.foreach( tuple => {
      var ((placedX, placedY), value) = tuple
      var surroundingCells = getSurroundingCells(placedX, placedY)
      var edges = getEdges(placedX, placedY)
      for ((cellX, cellY) <- surroundingCells.keys
        if (surroundingCells(cellX, cellY) != EMPTY);
        if (!placedCells.contains(cellX, cellY) 
            || placedCells(cellX, cellY) == EMPTY)
      ) nr += 1
      nr += edges.size
    })
    nr
  }
  
  def getPlacedCells(gridX: Int, gridY: Int, piece: Piece): Map[(Int, Int), Char] = {
    var placedCells = Map.empty[(Int, Int), Char]
    piece.values.foreach( tuple => {
      var ((x, y), value) = tuple
      if (value != EMPTY) {
        var absX = absolutePosition(gridX, x)
        var absY = absolutePosition(gridY, y)
        if (positionIsOnGrid(absX, absY)) {
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
        if (!positionIsOnGrid(cellX, cellY))) {
      edges += (cellX, cellY) -> EDGE
    }
    edges
  }
  
  def sortPositionOnMostSidesConnected(pieces: Map[Piece, Array[(Int, Int)]]): 
	  TreeMap[Int, Set[(Int, Int, Piece)]] = {
    
    var piecesSortedByConnections = TreeMap.empty[Int, Set[(Int, Int, Piece)]]
    
    for ((piece, locations) <- pieces) {
      for ((x, y) <- locations) {
        var nr = getNumberOfConnectedCells(x, y, piece)
        if (!piecesSortedByConnections.contains(nr)) {
          var set = Set.empty[(Int, Int, Piece)]
          piecesSortedByConnections += nr -> set
        }
        var nrSet = piecesSortedByConnections(nr) + ((x, y, piece))  
        piecesSortedByConnections += nr -> nrSet
      }
    }
    piecesSortedByConnections
  }
  
  def getNextMove(pieces: Set[Piece]): (Piece, (Int, Int)) = {
    var possibleMoves = findAllPlacesFor(pieces)
    var sortedMoves = sortPositionOnMostSidesConnected(possibleMoves)
    var topMoves = sortedMoves.values.last
    var (x, y, piece) = topMoves.first
    (piece, (x, y))    
  }
  
  def getSolution(pieces: Set[Piece]): Stack[(Int, Int, Piece)] = {
    var moves = new Stack[(Int, Int, Piece)]
    var possibleMoves = findAllPlacesFor(pieces)
    var sortedMoves = sortPositionOnMostSidesConnected(possibleMoves)
    var nextMoves = sortedMoves.values.last
    var (x, y, piece) = nextMoves.head
    moves.push(x, y, piece)
    place(x, y, piece)
    //solve()
    moves
  }
/*
  def solve(pieces: Set[Piece], moves: Stack[(Int, Int, Piece)]): Stack[(Int, Int, Piece)] = {
    
    if (solved) moves
    
    var orientations = DefaultPieces.allOrientationsFor(pieces)
    var possibleMoves = findAllPlacesFor(orientations)
    var sortedMoves = sortPositionOnMostSidesConnected(possibleMoves)
    
    var newMoves = moves
    for (prio <- sortedMoves.keys) {
      for ((x, y, piece) <- sortedMoves(prio)) {
        if (solved) return moves
        place(x, y, piece)
        println("Added piece:\n" + toString())
        newMoves = solve(pieces - piece, moves.push((x, y, piece)))
        if (!solved) {
          remove(piece)
          println("Removed piece:\n" + toString())
          newMoves = moves
        }
      }
    }  
    return newMoves
  }
*/  

/*  def solve(pieces: Set[Piece], 
      grid: PlayingGrid, 
      moves: Set[(Int, Int, Piece)]): PlayingGrid = {
    
    if (grid.solved) return grid
    
    println("moves = " + moves.size)
    
    var allMoves = Set.empty[(Int, Int, Piece)]
    if (moves.isEmpty) {
	    var orientations = DefaultPieces.allOrientationsFor(pieces)
	    var possibleMoves = findAllPlacesFor(orientations)
	    var sortedMoves = sortPositionOnMostSidesConnected(possibleMoves)	    
	    
	    for (prio <- sortedMoves.keys) {
	      allMoves ++= sortedMoves(prio)
	    }	    
    } else {
      allMoves = moves
    }
    if (allMoves.isEmpty) return grid
    
    println ("allMoves = " + allMoves.size)
    var move = allMoves.last
    var (x, y, piece) = move
    
    var newGrid = new PlayingGrid(grid.width, grid.height)
	newGrid.values = grid.values
	try {
		newGrid.place(x, y, piece)
		if (solved) {
		  return newGrid
		}		
	} catch {
	  case e: NotEmptyException => //println(e) 
	}
	println("Added piece:\n" + newGrid.toString())
	return solve(pieces - piece, newGrid, allMoves - move)
  }
*/

  def solve(pieces: Set[Piece], grid: PlayingGrid): PlayingGrid = {
    
    if (solved) grid
    
    var orientations = DefaultPieces.allOrientationsFor(pieces)
    var possibleMoves = findAllPlacesFor(orientations)
    var sortedMoves = sortPositionOnMostSidesConnected(possibleMoves)

    while (!sortedMoves.isEmpty) {
      var key = sortedMoves.keys.last
      if (grid.solved) return grid
      for ((x, y, piece) <- sortedMoves(key)) {
        if (grid.solved) return grid
        
        grid.place(x, y, piece)
        println("Added piece:\n" + grid.toString())
        
        var newGrid = solve(removeColor(pieces, piece.getColor()), grid)
        if (!newGrid.solved) {
          grid.remove(piece)
          println("Removed piece:\n" + toString())
        }        
      } 
      sortedMoves -= key
    }
    return grid
  }
  
  def removeColor(pieces: Set[Piece], color: Char): Set[Piece] = {
    var filteredPieces = Set.empty[Piece]
    for(piece <- pieces
      if (piece.getColor() != color)) {
    	filteredPieces += piece
    }
    filteredPieces
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

}


class NotEmptyException(x: Int, y: Int, piece: Piece, grid: Grid) extends RuntimeException {
  
  override def toString(): String = {
    return "NotEmptyException for: (" + x + ", " + y + ") -- \n" + 
    		piece + "\n --" + grid + "\n --"    		
  }
}