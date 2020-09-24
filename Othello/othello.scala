import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._
import scala.io.StdIn.readInt
import java.security.KeyStore.TrustedCertificateEntry
object Hello {
    def main(args: Array[String]) = {}
    class GameBoard {
        val tiles = Array.ofDim[Int](8, 8)
        var turn = 1
        var moves = ListBuffer[Tuple2[Int, Int]]()
        var played = ListBuffer[Tuple2[Int, Int]]()
        var stable = ListBuffer[Tuple2[Int, Int]]((0, 0), (0, 7), (7, 0), (7, 7))
        var unstable = ListBuffer[Tuple2[Int, Int]]((0, 1), (1, 0), (1, 1), (7, 6), (6, 7), (6, 6), 
                                                    (7, 1), (6, 0), (6, 1), (0, 6), (1, 7), (1, 6))
        var previous = ListBuffer[Tuple2[Int, Int]]()
        def reset() {
            for (r <- 0 to 7) {
                for (c <- 0 to 7) {
                    this.tiles(r)(c) = 0
                }
            }
            this.tiles(3)(3) = 1
            this.tiles(4)(4) = 1
            this.tiles(3)(4) = 2
            this.tiles(4)(3) = 2
            this.moves = ListBuffer[Tuple2[Int, Int]](
                (2, 2), (2, 3), (2, 4), (2, 5), 
                (3, 2), (3, 5), (4, 2), (4, 5), 
                (5, 2), (5, 3), (5, 4), (5, 5)
                )
            this.played = ListBuffer[Tuple2[Int, Int]](
                (3, 3), (3, 4), (4, 3), (4, 4)
                )
            var stable = ListBuffer[Tuple2[Int, Int]]((0, 0), (0, 7), (7, 0), (7, 7))
            var previous = ListBuffer[Tuple2[Int, Int]]()
        }

        def next_turn() {
            this.turn = if (this.turn == 1) 2 else 1
        }
        
        def show(){
            println(" --------------------------------")
            for (x <- 0 to 7) {
            print("| "); this.tiles(x).foreach(i => {print(i); print(" | ")})
            println("")
            println(" --------------------------------")
            }
        }
        def play(player: Int, row: Int,  column: Int) {
            if (row == -1) {this.pass(); return}
            assert(player == 1 || player == 2)
            assert(row <= 7 && row >= 0)
            assert(column <= 7 && column >= 0)
            assert(this.tiles(row)(column) == 0)
            val all_moves = this.generate_moves()
            assert(all_moves._1.contains((row, column)))
            this.tiles(row)(column) = player
            all_moves._2(all_moves._1.indexOf((row, column))).foreach(i => {this.tiles(i._1)(i._2) = this.turn})
            this.played.append((row, column))
            this.moves.remove(this.moves.indexOf((row, column)))
            for (i <- (-1 to 1)) { for (j <- (-1 to 1)) {
                if (0<=row+i && row+i<=7 && 0<=column+j && column+j<=7 && 
                !this.played.contains((row+i, column+j)) && 
                !this.moves.contains((row+i, column+j))) this.moves.append((row+i, column+j))
            }}
            this.previous.append((row, column))
            
            next_turn()
        }
        def pass(){
            this.previous.append((-1, -1))
            next_turn()
        }
        def generate_moves(): Tuple2[ListBuffer[Tuple2[Int, Int]], ListBuffer[ListBuffer[Tuple2[Int, Int]]]] =  {
            var available_moves = ListBuffer[Tuple2[Int, Int]]()
            var flips = ListBuffer[ListBuffer[Tuple2[Int, Int]]]()
            this.moves.foreach(move => {
                var flip = ListBuffer[Tuple2[Int, Int]]()
                var num = 0
                for (i <- -1 to 1) for (j <- -1 to 1) {
                    var res = 0
                    if (i == 0 && j == 0) {}
                    else if (i == 1 && j == 1) {
                        res = 7-math.max(move._1, move._2)
                    }
                    else if (i == 1 && j == -1) {
                        res = math.min(7-move._1, move._2)
                    }
                    else if (j == 1 && i == -1) {
                        res = math.min(7-move._2, move._1)
                    }
                    else if (i == -1 && j== -1) {
                        res = math.min(move._1, move._2)
                    }
                    else if (i == 1) {
                        res = 7-move._1
                    }
                    else if (i == -1) {
                        res = move._1
                    }
                    else if (j == 1) {
                        res = 7-move._2
                    }
                    else if (j == -1) {
                        res = move._2
                    }
                    if (res>0) {
                        val line = new Array[Int](res)
                        var temp = ListBuffer[Tuple2[Int, Int]]()
                        var current = 0
                        var count = 0
                        for (k <- 1 to res) {
                            line(k-1) = this.tiles(move._1+i*k)(move._2+j*k)
                        }
                        breakable{
                        line.foreach(l => {
                            count += 1
                            if (l != 0 && l != this.turn) {
                                current += 1
                                temp.append((move._1+count*i, move._2+count*j))
                            }
                            else if (l != 0 && l == this.turn) {
                                num += current
                                temp.foreach(tile => flip.append(tile))
                                break
                            }
                            else if (l == 0) {break}
                        })}

                    }
                }
                if (num != 0) {
                    available_moves.append((move._1, move._2))
                    flips.append(flip)
                }
            })
            return (available_moves, flips)
        }

        def static_eval(): Int = {
            var res = 0
            for (i <- 0 to 7) for (j <- 0 to 7) {
                if (this.tiles(i)(j)==this.turn) {
                    res -= 10
                    if (this.stable.contains((i, j))) {
                        res += 100000
                    }
                    if ((i==3||i==4)&&(j==3||j==4)) {
                        res += 350
                    }
                    if (((i==2||i==5)&&(j==2||j==3||j==4||j==5))||((i==3||i==4)&&(j==2||j==5))) {
                        res += 300
                    }
                    if (this.unstable.contains((i, j))) {
                        res -= 100
                    }
                }
                else if (this.tiles(i)(j)!=0) {
                    res+=10
                    if (this.stable.contains((i, j))) {
                        res -= 100000
                    }
                    if ((i==3||i==4)&&(j==3||j==4)) {
                        res -= 350
                    }
                    if (((i==2||i==5)&&(j==2||j==3||j==4||j==5))||((i==3||i==4)&&(j==2||j==5))) {
                        res -= 250
                    }
                    if (this.unstable.contains((i, j))) {
                        res += 100
                    }
                }

                
            }
            val moves = this.generate_moves()
            res += 450*moves._1.length
            var good = false
            moves._1.foreach(i => {if(!this.unstable.contains(i)){good=true}})
            if(!good){res-=100}
            return res
        }
        def AIplay() {
            val doable = this.generate_moves()._1
            if (doable.length==0) {this.pass(); return}

            var results = ListBuffer[Int]()
            doable.foreach(move => {
                var temp = new GameBoard()
                temp.reset()
                this.previous.foreach(prev => {temp.play(temp.turn, prev._1, prev._2)})
                temp.play(temp.turn, move._1, move._2)
                results.append(temp.static_eval()) 
            })
            
            var min = results.min
            val possible_moves = ListBuffer[Int]()
            for (i <- 0 to results.length - 1) {
                if (results(i) == min) {
                    possible_moves.append(i)
                }
            }
            var move_to_play = (-1, -1)
            val len = possible_moves.length
            if (len == 1) {move_to_play = doable(possible_moves(0))}
            else { 
                val r = scala.util.Random
                val n = r.nextInt(len-1)
                move_to_play = doable(possible_moves(n))
            }
            this.play(this.turn, move_to_play._1, move_to_play._2)
        }

        def playRandom() {
            val doable = this.generate_moves()._1
            val len = doable.length
            if (len == 0) {this.pass; return}
            if (len== 1) {this.play(this.turn, doable(0)._1, doable(0)._2); return}
            val r = scala.util.Random
            val n = r.nextInt(len-1)
            this.play(this.turn, doable(n)._1, doable(n)._2)
        }
    }
    var wins = 0
    var ties = 0
    var losses = 0
    for (iter <- 1 to 100) {
    var g = new GameBoard()
    g.reset()
    var run = true
    for (i <- 1 to 32) {
        /*
        val row = scala.io.StdIn.readInt(); val column = scala.io.StdIn.readInt()
        g.play(g.turn, row, column)
        g.show()
        println(g.moves)
        println(g.generate_moves()._1)
        */
        //g.play(g.turn, readInt(), readInt())
        g.show()
        println(g.generate_moves()._1)
        g.play(g.turn, readInt(), readInt())
        g.show()
        g.AIplay()
    }
    var counter = 0
    var counter2 = 0
    g.tiles.foreach(row => {row.foreach(col => {if(col==2){counter+=1}})})
    g.tiles.foreach(row => {row.foreach(col => {if(col==1){counter2+=1}})})
    print(counter); print("-"); println(counter2)
    if (counter > counter2) wins+=1
    else if (counter == counter2) ties += 1
    else {losses += 1}
    
    print("wins: "); print(wins); print(" - ties: "); print(ties); print(" - losses: "); println(losses)
    }
}

