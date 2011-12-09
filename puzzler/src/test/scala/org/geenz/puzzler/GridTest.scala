package org.geenz.puzzler

import org.junit._
import Assert._
import org.scalatest.Assertions
import scala.collection.immutable.Stack


class GridTest extends Assertions {

  @Test def gridShouldHaveDefaultSize() {
	val grid = new PlayingGrid()
	assert(11 === grid.width)
	assertEquals(5, grid.height)
  }
  
  @Test def gridShouldHaveSpecifiedSize() {
    val grid = new PlayingGrid(4, 2)
    assertEquals(4, grid.width)
    assertEquals(2, grid.height)
  }
  
  @Test def allGridpointsShouldBeEmpty() {
    val grid = new PlayingGrid(3, 3)
    grid.values.values.foreach( point => {
      assertEquals('0', point)
    })
  }
  
  @Test def retrieveGridPointByCoordinate() {
    val grid = new PlayingGrid(3, 3)
    assertEquals('0', grid.values(1, 1))
  }
  
  @Test def initializeGridWithString() {
    val gridString = "000\nFFF"
    val grid = new PlayingGrid(3, 2, gridString)    
    assertEquals('F', grid.values(1, 1))
  }
  
  @Test def putPieceOnGrid() {
    val grid = new PlayingGrid(5, 5)
    val piece = new Piece("AA\nAA")
    grid.place(2, 2, piece)
    assertEquals('A', grid.values(2, 2))
    assertEquals('A', grid.values(2, 3))
    assertEquals('A', grid.values(3, 2))
    assertEquals('A', grid.values(3, 3))
  }
  
  @Test def putAsymmetricPieceOnGrid() {
    val grid = new PlayingGrid(5, 5)
    val piece = new Piece("A0\nAA")
    grid.place(2, 2, piece)
    assertEquals('A', grid.values(2, 2))
    assertEquals('A', grid.values(2, 3))
    assertEquals('A', grid.values(3, 2))
    assertEquals('0', grid.values(3, 3))
  }
  
  @Test def piecePlacedOverlappingFails() {
	  val grid = new PlayingGrid(5, 5)
	  val occupyingPiece = new Piece("AA\nAA")
	  val toPlacePiece = new Piece("B")
	  grid.place(2, 2, occupyingPiece)

	  var success = false;
	  try {
		  grid.place(2, 2, toPlacePiece)
	  } catch {
	    case nee: NotEmptyException =>
	      success = true
	  }
	  assertTrue(success)
  }
  
  @Test def piecePlacedOverlappingNotPlacedAtAll() {
	  val grid = new PlayingGrid(5, 5)
	  val occupyingPiece = new Piece("AA\nAA")
	  val toPlacePiece = new Piece("BB")
	  grid.place(2, 2, occupyingPiece)

	  var success = false;
	  try {
		  grid.place(1, 2, toPlacePiece)
	  } catch {
	    case nee: NotEmptyException =>
	      success = true
	  }
	  assertTrue(success)
	  assertEquals(grid.EMPTY, grid.values(1, 2))
  }

  @Test def ensurePieceFits() {
	  val grid = new PlayingGrid(5, 5)
	  val occupyingPiece = new Piece("AA\nAA")
	  val toPlacePiece = new Piece("BB")
	  grid.place(2, 2, occupyingPiece)
	  assertFalse(grid.pieceFits(1, 2, toPlacePiece))
  }
  
  @Test def rotateSquarePiece() {
    val basePiece = new Piece("A0\nAA")
    var pieceRotations = basePiece.getRotations()
    assertEquals("A0\nAA\n", pieceRotations(0).toString())
    assertEquals("AA\nA0\n", pieceRotations(90).toString())
    assertEquals("AA\n0A\n", pieceRotations(180).toString())
    assertEquals("0A\nAA\n", pieceRotations(270).toString())
  }
  
  @Test def rotateRectangularPiece() {
    var pieceRotations = DefaultPieces.A.getRotations()
    assertEquals("AA\n0A\n0A\n", pieceRotations(0).toString())
    assertEquals("00A\nAAA\n", pieceRotations(90).toString())
    assertEquals("A0\nA0\nAA\n", pieceRotations(180).toString())
    assertEquals("AAA\nA00\n", pieceRotations(270).toString())
  }
  
  @Test def flipSquarePiece() {
    var flippedPiece = DefaultPieces.F.flip()
    assertEquals("0F\nFF\n", flippedPiece.toString)
  }

  @Test def flipRectangularPiece() {
    var flippedPiece = DefaultPieces.A.flip()
    assertEquals("0A\n0A\nAA\n", flippedPiece.toString)
    assertEquals(new Piece("0A\n0A\nAA"), flippedPiece)
  }
  
  @Test def testGettingAllOrientations() {
    var basePiece = DefaultPieces.A
    var perms = basePiece.orientations()
    assertEquals(8, perms.size)
  }
  
  @Test def testGettingUniqueOrientations() {
	  var perms = DefaultPieces.J.uniqueOrientations()
	  assertEquals(2, perms.size)
  }
  
  @Test def getAllPositionsWhenTheresOne() {
    var grid = new PlayingGrid(2, 2)
    var places = grid.findAllPlacesFor(DefaultPieces.K)
    assertEquals(1, places.size)
    var (x, y) = places(0);
    assertEquals(1, x)
    assertEquals(1, y)
  }
  
  @Test def getAllPositionsWhenThereAreMore() {
    var grid = new PlayingGrid(10, 10)
    var places = grid.findAllPlacesFor(DefaultPieces.A)
    assertEquals(72, places.size)
  }
  
  @Test def getAllPositionsForAllOrientations() {
	var grid = new PlayingGrid(10, 10)
    var places = grid.findAllPlacesFor(DefaultPieces.A.uniqueOrientations())
    var total = 0
    places.foreach( tuple => {
      var (piece, positions) = tuple
      total += positions.size
    })
    assertEquals(576, total)
  }
  
  @Test def findAllPossibleStartingMoves() {
    var grid = new PlayingGrid()
    var pieces = grid.generateAllPossibleOrientations(DefaultPieces.allPieces)
    var startingMoves = grid.findAllPlacesFor(pieces)
    assertEquals(60, startingMoves.size)
    var total = 0
    startingMoves.foreach( tuple => {
      var (piece, positions) = tuple
      total += positions.size
    })
    assertEquals(1789, total)
  }
  
  @Test def removePieceFromGrid() {
    var grid = new PlayingGrid()
    grid.place(1, 1, DefaultPieces.K)
    grid.remove(DefaultPieces.K)
    assertTrue(grid.isEmpty(1, 1))
    assertTrue(grid.isEmpty(2, 1))
    assertTrue(grid.isEmpty(1, 2))
    assertTrue(grid.isEmpty(2, 2))
  }
    
  @Test def getNumberOfConnectedCellsForBaseLocation() {
    var grid = new PlayingGrid(10, 10)
    assertEquals(4, grid.getNumberOfConnectedCells(1, 1, DefaultPieces.K))
  }
  
  @Test def getNumberOfConnectedCellsForNonConnectedLocation() {
    var grid = new PlayingGrid(10, 10)
    assertEquals(0, grid.getNumberOfConnectedCells(3, 4, DefaultPieces.K))
  }
  
  @Test def getNumberOfConnectedCellsForSinglyConnectedLocation() {
    var grid = new PlayingGrid(10, 10)
    grid.place(1, 1, DefaultPieces.K)
    assertEquals(1, grid.getNumberOfConnectedCells(2, 3, DefaultPieces.K))
  }
  
  @Test def getMaxNumberOfConnectedCellsOnEmptyGrid() {
    var grid = new PlayingGrid()
    var possibleMoves = grid.findAllPlacesFor(DefaultPieces.K.uniqueOrientations())
    var sortedMoves = grid.sortPositionOnMostSidesConnected(possibleMoves)
    var nr = sortedMoves.keys.last
    assertEquals(4, nr)
  }

  @Test def getMaxNumberOfConnectedCellsOnGridWithOnePiece() {
    var grid = new PlayingGrid()
    grid.place(2, 1, DefaultPieces.A)
    var possibleMoves = grid.findAllPlacesFor(DefaultPieces.K.uniqueOrientations())
    var sortedMoves = grid.sortPositionOnMostSidesConnected(possibleMoves)
    var nr = sortedMoves.keys.last
    assertEquals(7, nr)
  }
  
  @Test def findAllPlacesForSinglePiece() {
    var grid = new PlayingGrid(3, 3)
    var aRotations = DefaultPieces.A.getRotations()    
    grid.place(1, 1, aRotations(180))
    var foundPlaces = grid.findAllPlacesFor(DefaultPieces.B)
    assertEquals(1, foundPlaces.size)    
  }
  
  @Test def pieceFitsForSinglePossibleSpace() {
    var grid = new PlayingGrid(3, 3)
    var aRotations = DefaultPieces.A.getRotations()    
    grid.place(1, 1, aRotations(180))
    assertTrue(grid.pieceFits(2, 1, DefaultPieces.B))
  }
  
  @Test def positionIsOnGrid() {
    var grid = new PlayingGrid(2, 2)
    assertFalse(grid.positionIsOnGrid(0, 1))
    assertFalse(grid.positionIsOnGrid(1, -1))
    assertFalse(grid.positionIsOnGrid(3, 2))
    assertFalse(grid.positionIsOnGrid(1, 3))
    assertTrue(grid.positionIsOnGrid(2, 2))
  }

  @Test def getNumberOfEmptyCells() {
    var grid = new PlayingGrid(3, 3)
    assertEquals(9, grid.numberOfEmptyCells())
    
    var aRotations = DefaultPieces.A.getRotations()    
    grid.place(1, 1, aRotations(180))
    assertEquals(5, grid.numberOfEmptyCells())
    
    grid.place(2, 1, DefaultPieces.B)
    assertTrue(grid.solved)    
  }

  @Test def findSolutionWithTwoPieceSetOneMove() {
    var grid = new PlayingGrid(3, 3)
    var aRotations = DefaultPieces.A.getRotations()    
    grid.place(1, 1, aRotations(180))
    var (piece, (x, y)) = grid.getNextMove(DefaultPieces.B.uniqueOrientations())
    assertEquals(DefaultPieces.B, piece)
    assertEquals((2, 1), (x, y))
  }
  
  @Test def findSolutionWithTwoPieceSetNoMoves() {
    var grid = new PlayingGrid(3, 3)
    var pieces = Set(DefaultPieces.A, DefaultPieces.B)
    println(grid)
    //var solution = grid.solve(pieces, Stack.empty[(Int, Int, Piece)])
    var newGrid = grid.solve(pieces, grid)
    println("two:\n" + newGrid)
    assertTrue(newGrid.solved())
  }
  
  @Test def findSolutionWithThreePieceSetNoMoves() {
    var grid = new PlayingGrid(4, 4)
    var pieces = Set(DefaultPieces.A, DefaultPieces.F, DefaultPieces.H)
    println(grid)
    //var solution = grid.solve(pieces, Stack.empty[(Int, Int, Piece)])
    var newGrid = grid.solve(pieces, grid)
    println("three:\n" + newGrid)
    assertTrue(newGrid.solved())
  }
}