package org.geenz.puzzler

import org.junit._
import Assert._
import org.scalatest.Assertions
import scala.collection.immutable.Stack
import scala.collection.immutable.SortedSet
import java.util.Date


class GridTest extends Assertions {
  
  val nl = "\n"

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
	  assertFalse(grid.place(2, 2, toPlacePiece))
  }
  
  @Test def piecePlacedOverlappingNotPlacedAtAll() {
	  val grid = new PlayingGrid(5, 5)
	  val occupyingPiece = new Piece("AA\nAA")
	  val toPlacePiece = new Piece("BB")
	  grid.place(2, 2, occupyingPiece)
	  assertFalse(grid.place(1, 2, toPlacePiece))
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
    assertEquals("A0\nAA", pieceRotations(0).toString())
    assertEquals("AA\nA0", pieceRotations(90).toString())
    assertEquals("AA\n0A", pieceRotations(180).toString())
    assertEquals("0A\nAA", pieceRotations(270).toString())
  }
  
  @Test def rotateRectangularPiece() {
    var pieceRotations = DefaultPieces.A.getRotations()
    assertEquals("AA\n0A\n0A", pieceRotations(0).toString())
    assertEquals("00A\nAAA", pieceRotations(90).toString())
    assertEquals("A0\nA0\nAA", pieceRotations(180).toString())
    assertEquals("AAA\nA00", pieceRotations(270).toString())
  }
  
  @Test def flipSquarePiece() {
    var flippedPiece = DefaultPieces.F.flip()
    assertEquals("0F\nFF", flippedPiece.toString)
  }

  @Test def flipRectangularPiece() {
    var flippedPiece = DefaultPieces.A.flip()
    assertEquals("0A\n0A\nAA", flippedPiece.toString)
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
    assertEquals(4, grid.getNumberOfSurroundingFilledCells(1, 1, DefaultPieces.K))
  }
  
  @Test def getNumberOfConnectedCellsForNonConnectedLocation() {
    var grid = new PlayingGrid(10, 10)
    assertEquals(0, grid.getNumberOfSurroundingFilledCells(3, 4, DefaultPieces.K))
  }
  
  @Test def getNumberOfConnectedCellsForSinglyConnectedLocation() {
    var grid = new PlayingGrid(10, 10)
    grid.place(1, 1, DefaultPieces.K)
    assertEquals(1, grid.getNumberOfSurroundingFilledCells(2, 3, DefaultPieces.K))
  }
  
  @Test @Ignore def getMaxNumberOfConnectedCellsOnEmptyGrid() {
    var grid = new PlayingGrid()
    var possibleMoves = grid.findAllPlacesFor(DefaultPieces.K.uniqueOrientations())
    var sortedMoves = grid.sortPositionOnMostSidesConnected(possibleMoves)
    var nr = sortedMoves.last
    assertEquals(4, nr)
  }

  @Test @Ignore def getMaxNumberOfConnectedCellsOnGridWithOnePiece() {
    var grid = new PlayingGrid()
    grid.place(2, 1, DefaultPieces.A)
    var possibleMoves = grid.findAllPlacesFor(DefaultPieces.K.uniqueOrientations())
    var sortedMoves = grid.sortPositionOnMostSidesConnected(possibleMoves)
    var nr = sortedMoves.last
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
    assertFalse(grid.isOnGrid(0, 1))
    assertFalse(grid.isOnGrid(1, -1))
    assertFalse(grid.isOnGrid(3, 2))
    assertFalse(grid.isOnGrid(1, 3))
    assertTrue(grid.isOnGrid(2, 2))
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
    var move = grid.getNextMove(DefaultPieces.B.uniqueOrientations())
    assertEquals(DefaultPieces.B, move.piece)
    assertEquals((2, 1), (move.x, move.y))
  }
  
  @Test def findSolutionWithTwoPieceSetNoMoves() {
    var grid = new PlayingGrid(3, 3)
    var pieces = Set(DefaultPieces.A, DefaultPieces.B)
    println(grid)
    var newGrid = grid.solve(pieces)
    println("two:\n" + newGrid)
    assertTrue(newGrid.solved())
  }
  
  @Test def findSolutionWithThreePieceSetNoMoves() {
    var grid = new PlayingGrid(4, 4)
    var pieces = Set(DefaultPieces.A, DefaultPieces.F, DefaultPieces.H, DefaultPieces.J)
    println(grid)
    var newGrid = grid.solve(pieces)
    println("three:\n" + newGrid)
    assertTrue(newGrid.solved())
  }

  @Test @Ignore def findSolutionForMasterLevelNumber72() {
    var start = System.currentTimeMillis()
    System.out.println("Start Time: " + new Date())
    var grid = new PlayingGrid()
    grid.initGridWithString(11, 5, 
        "AAAJJJJDDDD" + nl +
        "00A000000D0" + nl +
        "00000000000" + nl +
        "00000000000" + nl +
        "00000000000");
    var pieces = DefaultPieces.allPieces - DefaultPieces.A - DefaultPieces.J - DefaultPieces.D
    println(grid)
    var newGrid = grid.solve(pieces)
    System.out.println("End Time: " + new Date())
    var millies = System.currentTimeMillis() - start
    System.out.println("Duration = " + millies / 1000)
    assertTrue(newGrid.solved())
  }

  @Test def testHeuristicNoSingleOpenSpaces() {
    var grid = new PlayingGrid(3, 3)
    grid.initGridWithString(3, 3, 
        "AAA" + nl +
        "B0B" + nl +
        "BBB")
    assertTrue(grid.hasSingleOpenSpace())
  }

  @Test def testHeuristicNoTwinOpenSpaces() {
    var grid = new PlayingGrid(3, 4)
    grid.initGridWithString(3, 4, 
        "AAA" + nl +
        "B0B" + nl +
        "B0B" + nl +
        "BBB")
    assertTrue(grid.hasTwinOpenSpace())
  }

}