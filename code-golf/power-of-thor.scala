/**
The Goal - minimum ammount of code for: 
https://www.codingame.com/ide/puzzle/power-of-thor

Your program must allow Thor to reach the light of power. You need to have the shortest code possible.
 	Rules

Thor moves on a map which is 40 wide by 18 high. Note that the coordinates (X and Y) start at the top left! This means the most top left cell has the coordinates "X=0,Y=0" and the most bottom right one has the coordinates "X=39,Y=17".

Once the program starts you are given:
the variable lightX: the X position of the light of power that Thor must reach.
the variable lightY: the Y position of the light of power that Thor must reach.
the variable initialTX: the starting X position of Thor.
the variable initialTY: the starting Y position of Thor.
At the end of the game turn, you must output the direction in which you want Thor to go among:
	
N (North)
NE (North-East)
E (East)
SE (South-East)
S (South)
SW (South-West)
W (West)
NW (North-West)
Each movement makes Thor move by 1 cell in the chosen direction.
*/

object Player extends App {
val Array(x1, y1, x0, y0) = for (i <- readLine split " ") yield i.toInt
var x = x0
var y = y0
go(x0.compare(x1),y0.compare(y1)).map(println)
def go(dx: Int, dy: Int):List[String]=(dx,dy)match{
    case (0,0)=>Nil
    case (dx,dy)=>x-=dx
    y-=dy
    Seq("S","","W")(dy + 1)+Seq("E", "", "W")(dx+1)::go(x.compare(x1),y.compare(y1))}}
