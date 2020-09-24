package blockmole 
import java.awt.{Color => JColor}
object Color {
    val black   = new JColor(   0,   0,   0)
    val mole    = new JColor(  51,  51,   0)
    val soil    = new JColor( 153, 102,  51)
    val tunnel  = new JColor( 204, 153, 102)
    val grass   = new JColor(  25, 130,  35)
    val sky     = JColor.blue
    val candy   = JColor.pink
}
object BlockWindow {
    import introprog.PixelWindow
    val windowSize  = (150, 110)
    val blockSize   = 10
    val skyHeight = 20
    val grassHeight = 10
    val wx = windowSize._1
    val wy = windowSize._2
    val groundHeight = skyHeight + grassHeight
    val undergroundHeight = wy - groundHeight
    var candyPos: Pos = (-1, -1)

    val window = new PixelWindow(
        windowSize._1 * blockSize,
        windowSize._2 * blockSize,
        "Digging Blockmole")

    type Pos = (Int, Int)

    def block(pos: Pos, color: JColor = JColor.gray): Unit = {
        val x = pos._1 * blockSize
        val y = pos._2 * blockSize
        window.fill(x, y, blockSize, blockSize, color)
    }

    def rectangle(leftTop: Pos, size: Pos, color: JColor = JColor.gray) {
        for (y <- leftTop._2 to leftTop._2 + size._2) {
            for (x <- leftTop._1 to leftTop._1 + size._1) {
                block((x, y), color)
            }
        }
        /* Alternativ lösning
        window.fill(
            pos._1,             pos._2, 
            blockSize*size._1,  blockSize*size._2,
            color
            )
        */
    }

    def candy() {
        import scala.util.Random.nextInt
        val pos = (nextInt(wx), nextInt(undergroundHeight)+groundHeight)
        block(pos, Color.candy)
        candyPos = pos
    }

    val maxWaitMillis = 10

    def waitForKey(): String = {
        window.awaitEvent(maxWaitMillis)
        while (window.lastEventType != PixelWindow.Event.KeyPressed) {
            window.awaitEvent(maxWaitMillis)
        }
        println(s"KeyPressed: " + window.lastKey)
        window.lastKey
    }
    
}
object Mole {
    def dig(): Unit = {
        var x = BlockWindow.wx / 2
        var y = BlockWindow.wy / 2
        var quit = false
        var tail = Color.tunnel
        while (!quit) {
            if (y < BlockWindow.groundHeight) tail = Color.grass
            else tail = Color.tunnel
            BlockWindow.block((x, y), Color.mole)
            val key = BlockWindow.waitForKey()
            if (key == "w") {
                if (y > BlockWindow.skyHeight) {
                    BlockWindow.block((x, y), tail) 
                    y -= 1
                }
            } 
            else if (key == "a") {
                if (x > 0) {
                    BlockWindow.block((x, y), tail) 
                    x -= 1
                }
            } 
            else if (key == "s") {
                if (y < BlockWindow.wy -1) {
                    BlockWindow.block((x, y), tail)
                    y += 1
                }
            } 
            else if (key == "d") {
                if (x < BlockWindow.wx-1) {
                    BlockWindow.block((x, y), tail)
                    x += 1
                }
            } 
            else if (key == "q") {
                quit = true
            } 
        }
    }
}
object Worm {
    var score = 0
    def dig(): Unit = {
        var x = BlockWindow.wx / 2
        var y = BlockWindow.wy / 2
        var quit = false
        var tail = Color.tunnel
        while (!quit) {
            if (y < BlockWindow.groundHeight) tail = Color.grass
            else tail = Color.tunnel
            BlockWindow.block((x, y), Color.mole)
            val key = BlockWindow.waitForKey()
            if (key == "w") {
                if (y > BlockWindow.skyHeight) {
                    if ((x, y-1) == BlockWindow.candyPos) {
                        score += 1
                        BlockWindow.candy()
                    }
                    else BlockWindow.block((x, y), tail) 
                    y -= 1
                }
            } 
            else if (key == "a") {
                if (x > 0) {
                    if ((x-1, y) == BlockWindow.candyPos) {
                        score += 1
                        BlockWindow.candy()
                    }
                    else BlockWindow.block((x, y), tail) 
                    x -= 1
                }
            } 
            else if (key == "s") {
                if (y < BlockWindow.wy -1) {
                    if ((x, y+1) == BlockWindow.candyPos) {
                        score += 1
                        BlockWindow.candy()
                    }
                    else BlockWindow.block((x, y), tail)
                    y += 1
                }
            } 
            else if (key == "d") {
                if (x < BlockWindow.wx-1) {
                    if ((x+1, y) == BlockWindow.candyPos) {
                        score += 1
                        BlockWindow.candy()
                    }
                    else BlockWindow.block((x, y), tail)
                    x += 1
                }
            } 
            else if (key == "q") {
                quit = true
            }

        }
    }
}
object Main {
    def drawWorld(): Unit = {
        val skyHeight = BlockWindow.skyHeight
        val grassHeight = BlockWindow.grassHeight
        val wx = BlockWindow.wx
        val wy = BlockWindow.wy
        val groundHeight = BlockWindow.groundHeight
        val undergroundHeight = BlockWindow.undergroundHeight
        BlockWindow.rectangle((0, 0), (wx, skyHeight), Color.sky)
        BlockWindow.rectangle((0, skyHeight), (wx, grassHeight), Color.grass)
        BlockWindow.rectangle((0, groundHeight), (wx, undergroundHeight), Color.soil)
        BlockWindow.candy()
    }
    def main(args: Array[String]): Unit = {
        println("keep on digging")
        drawWorld()
        Worm.dig()
    }
}
