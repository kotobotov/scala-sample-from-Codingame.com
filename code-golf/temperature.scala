/**
The Goal - minimum ammount of code for solution:
https://www.codingame.com/ide/puzzle/temperature-code-golf

In this exercise, you have to analyze records of temperature to find the closest to zero.

You need to solve this puzzle using as little characters as possible.

	
Sample temperatures
Here, -1 is the closest to 0.
 	Rules

Write a program that prints the temperature closest to 0 among input data. If two numbers are equally close to zero, positive integer has to be considered closest to zero (for instance, if the temperatures are -5 and 5, then display 5).
 	Game Input

Your program must read the data from the standard input and write the result on the standard output.
Input
Line 1: N, the number of temperatures to analyze

Line 2: The N temperatures expressed as integers ranging from -273 to 5526

Output
Display 0 (zero) if no temperatures are provided. Otherwise, display the temperature closest to 0.
Constraints
0 ≤ N < 10000
Example
Input
5
1 -2 -8 4 5
Output
1
*/



object Solution extends App{print(if(readInt==0)0 else readLine.split(" ").map(_.toInt).minBy(t=>t.abs,t<0))}
