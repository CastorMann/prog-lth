object Chess {
    def main(args: Array[String]): Unit = {
        val version = "1.1"
        println(s"Chess Bot $version")

        // class Gameboard holds data and functions about the gameboard itself

        class Gameboard {

            // Instastiate variables
            var board = Array.ofDim[Char](8, 8)       // 2D array representing the gameboard
            var turn = 1                                // variable representing the player on turn

            // reset() resets the gameboard to it's initial state
            def reset() {
                // Initializing blank spaces
                for (row <- 2 to 5) for (column <- 0 to 7) {
                    board(row)(column) = '_'
                }
                // Initializing pawns
                for (column <- 0 to 7) {
                    board(1)(column) = 'p'
                    board(6)(column) = 'P'
                }
                // Initializing rooks
                board(0)(0) = 'r'; board(0)(7) = 'r'
                board(7)(0) = 'R'; board(7)(7) = 'R'

                // Initializing knights
                board(0)(1) = 'h'; board(0)(6) = 'h'
                board(7)(1) = 'H'; board(7)(6) = 'H'

                // Initializing bishops
                board(0)(2) = 'b'; board(0)(5) = 'b'
                board(7)(2) = 'B'; board(7)(5) = 'B'

                // Initializing kings and queens
                board(0)(3) = 'q'; board(0)(4) = 'k'
                board(7)(3) = 'Q'; board(7)(4) = 'K'
            }

            // prints the gameboard's current state
            def show() {
                println("    A   B   C   D   E   F   G   H   ")
                println("  ---------------------------------")
                for (x <- 0 to 7) {
                print(s"${8-x} | "); board(x).foreach(i => {print(i); print(" | ")})
                println(8-x)
                println("  ---------------------------------")
                }
                println("    A   B   C   D   E   F   G   H   ")
            }
        }

        var b = new Gameboard()
        b.reset()
        b.show()
    }
}