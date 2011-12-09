package org.geenz.puzzler

object DefaultPieces {

  val A_STRING = 
    "AA\n" +
  	"0A\n" +
  	"0A"
  val B_STRING =
    "BB\n" +
    "BB\n" +
    "0B"
  val C_STRING = 
    "CC\n" +
    "0C\n" +
    "0C\n" +
    "0C"
  val D_STRING =
    "0D\n" +
    "DD\n" +
    "0D\n" +
    "0D"
  val E_STRING =
    "E0\n" +
    "EE\n" +
    "0E\n" +
    "0E"
  val F_STRING =
    "FF\n" +
    "0F"
  val G_STRING =
  	"GGG\n" +
  	"00G\n" +
  	"00G"
  val H_STRING =
    "HH0\n" +
    "0HH\n" +
    "00H"
  val I_STRING =
    "I0I\n" +
    "III"
  val J_STRING = 
    "J\n" +
    "J\n" +
    "J\n" +
    "J"
  val K_STRING =
    "KK\n" +
    "KK"
  val L_STRING =
    "0L0\n" +
    "LLL\n" +
    "0L0"

  val A = new Piece(A_STRING)
  val B = new Piece(B_STRING)
  val C = new Piece(C_STRING)
  val D = new Piece(D_STRING)
  val E = new Piece(E_STRING)
  val F = new Piece(F_STRING)
  val G = new Piece(G_STRING)
  val H = new Piece(H_STRING)
  val I = new Piece(I_STRING)
  val J = new Piece(J_STRING)
  val K = new Piece(K_STRING)
  val L = new Piece(L_STRING)
  
  val allPieces = Set(A, B, C, D, E, F, G, H, I, J, K, L)
  
  def allOrientations(): Set[Piece] = {
    allOrientationsFor(allPieces)
  }

  def allOrientationsFor(pieces: Set[Piece]): Set[Piece] = {
    var orientations = Set.empty[Piece] 
    for (piece <- pieces) orientations ++= piece.uniqueOrientations()
    orientations
  }

}