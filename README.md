hsQueens-Fun!


It currently understands the commands

Go N   -  chooses the option number N

Up     -  goes up

Auto N -  looks for a board with at least N queens below the tree
	  that is focused.

Top   - all the way to the top!


$ hsQueens 5
`_____
_____
_____
_____
_____

0:(3,3)  1:(2,2)  2:(2,3)  3:(2,4)  4:(3,2)  5:(3,4)  6:(4,2)  7:(4,3)  8:(4,4)  9:(1,1)  10:(1,2)  11:(1,3)  12:(1,4)  13:(1,5)  14:(2,1)  15:(2,5)  16:(3,1)  17:(3,5)  18:(4,1)  19:(4,5)  20:(5,1)  21:(5,2)  22:(5,3)  23:(5,4)  24:(5,5)  
14:53:47> Auto 4
___Q_
Q____
__Q__
_____
_Q___

0:(5,4)  
14:53:52> Go 0
___Q_
Q____
__Q__
____Q
_Q___


14:54:00> `

There! Solved the 5 Queens-Problem!

