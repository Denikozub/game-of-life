# game-of-life
Conway's Game of Life Simulation

### run
~~~
stack setup
stack run
~~~

### config
* First line (integer) - size of square board
* Next size^2 lines - 0 (Dead) / 1 (Alive)
* Indexing as follows:
~~~
0   1     2     ..  n-1  
n   n+1   n+2   ..  2n-1
........................
n(n-1)      ...     n^2-1
~~~
