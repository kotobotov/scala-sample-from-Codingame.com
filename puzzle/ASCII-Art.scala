 /**
 
 The Goal - just solve 
 https://www.codingame.com/ide/puzzle/ascii-art

In stations and airports you often see this type of screen:

Have you ever asked yourself how it might be possible to simulate this display on a good old terminal? We have: with ASCII art!
 	Rules

ASCII art allows you to represent forms by using characters. To be precise, in our case, these forms are words. For example, the word "MANHATTAN" could be displayed as follows in ASCII art:

 
# #  #  ### # #  #  ### ###  #  ###
### # # # # # # # #  #   #  # # # #
### ### # # ### ###  #   #  ### # #
# # # # # # # # # #  #   #  # # # #
# # # # # # # # # #  #   #  # # # #
 
​Your mission is to write a program that can display a line of text in ASCII art in a style you are given as input.

 	Game Input

Input
Line 1: the width L of a letter represented in ASCII art. All letters are the same width.

Line 2: the height H of a letter represented in ASCII art. All letters are the same height.

Line 3: The line of text T, composed of N ASCII characters.

Following lines: the string of characters ABCDEFGHIJKLMNOPQRSTUVWXYZ? Represented in ASCII art.

Output
The text T in ASCII art.
The characters a to z are shown in ASCII art by their equivalent in upper case.
The characters that are not in the intervals [a-z] or [A-Z] will be shown as a question mark in ASCII art.
Constraints
0 < L < 30
0 < H < 30
0 < N < 200
Example 1
Input
4
5 
E
 #  ##   ## ##  ### ###  ## # # ###  ## # # #   # # ###  #  ##   #  ##   ## ### # # # # # # # # # # ### ### 
# # # # #   # # #   #   #   # #  #    # # # #   ### # # # # # # # # # # #    #  # # # # # # # # # #   #   # 
### ##  #   # # ##  ##  # # ###  #    # ##  #   ### # # # # ##  # # ##   #   #  # # # # ###  #   #   #   ## 
# # # # #   # # #   #   # # # #  #  # # # # #   # # # # # # #    ## # #   #  #  # # # # ### # #  #  #       
# # ##   ## ##  ### #    ## # # ###  #  # # ### # # # #  #  #     # # # ##   #  ###  #  # # # #  #  ###  #  
Output
### 
#   
##  
#   
###

**/



object Solution extends App {
    val l = readInt
    val h = readInt
    val t = readLine
   val row = for(i <- 0 until h) yield readLine
    val sample = "ABCDEFGHIJKLMNOPQRSTUVWXYZ?"
def getChar(index:Int):IndexedSeq[String] = index match {
case x if x>= 0 => row.map(_.substring((index*l), (index*l + l)))
case _  => getChar(sample.indexOf("?"))
}
val text = t.map(char => getChar(sample.indexOf(char.toUpper)))
val answer = text.reduceLeft { (a, b) => a.zip(b).map(item => item._1 + item._2) }
answer.foreach(println(_))
}

