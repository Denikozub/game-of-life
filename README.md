# Game of Life
Conway's game of life simulation + UI

### Run using Stack
~~~
stack setup
stack build
stack run
~~~

### Board configuration file
* First line (integer) - size of square board
* Next size^2 lines - 0 (Dead) / 1 (Alive)
* Indexing as follows:
~~~
0   1     2     ..  n-1  
n   n+1   n+2   ..  2n-1
........................
n(n-1)      ...     n^2-1
~~~

### Manage boards database
Boards can be managed via their unique IDs
~~~
stack run -- --upload 1 board.txt
stack run -- --delete 1
~~~
