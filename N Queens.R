## N-Queens Solver
## Author: James Walker
## Copyrighted 2017 under the MIT license:
##   http://www.opensource.org/licenses/mit-license.php
## 
## Purpose:
##   The N-Queens Solver finds solutions for the N-Queens problem. That is, how
##   many ways are there to place N chess queens on an NxN chess board such
##   that none of the queens can attack each other.
## Example Usage:
##   > setwd('C:/Users/SomeUser/Desktop')
##   > source('n_queens_solver.R')
##   > # Display solutions to the 4-queens problem in standard out:
##   > SolveNQueens(4)
##   The solver found 2 solutions for the 4-Queens problem  
##   Solution 1:     2 4 1 3
##   Solution 2:     3 1 4 2
##   > # Save solutions to the 4-queens problem in a plain-text file:
##   > SolveNQueens(4, 'FourQueensSolutions.txt')
## 
## This implementation is based off the algorithm provided at the bottom of this
## webpage: www.cs.utexas.edu/users/EWD/transcriptions/EWD03xx/EWD316.9.html

# Initializes starting values for the NxN chess board
InitializeBoard <- function(n.queens) {
  chess.board <- vector("list", 0)
  chess.board$queens <- vector(mode = "integer", length = n.queens)
  chess.board$column.j <- 0
  chess.board$column <- rep(TRUE, n.queens)
  chess.board$diagonal.up <- rep(TRUE, 2 * n.queens - 1)
  chess.board$diagonal.down <- rep(TRUE, 2 * n.queens - 1)
  return(chess.board)
}

# Check if a queen can be placed on the current square
SquareIsFree <- function(chess.board, n.queens, row.i) {
  return(chess.board$column[row.i + 1] &
           chess.board$diagonal.up[n.queens + chess.board$column.j - row.i] &
           chess.board$diagonal.down[chess.board$column.j + row.i + 1])
}

# Places a queen on the NxN chess board in the given column
SetQueen <- function(chess.board, n.queens, row.i) {
  chess.board$queens[chess.board$column.j + 1] <- row.i + 1
  chess.board$column[row.i + 1] <- FALSE
  chess.board$diagonal.up[n.queens + chess.board$column.j - row.i] <- FALSE
  chess.board$diagonal.down[chess.board$column.j + row.i + 1] <- FALSE
  chess.board$column.j <- chess.board$column.j + 1
  return(chess.board)
}

# Removes a queen from the NxN chess board in the given column to backtrack
RemoveQueen <- function(chess.board, n.queens, row.i) {
  chess.board$column.j <- chess.board$column.j - 1
  chess.board$column[row.i + 1] <- TRUE
  chess.board$diagonal.up[n.queens + chess.board$column.j - row.i] <- TRUE
  chess.board$diagonal.down[chess.board$column.j + row.i + 1] <- TRUE
  return(chess.board)
}

# Recursive function for finding valid queen placements on the chess board
PlaceNextQueen <- function(chess.board, n.queens, scope) {
  for (row.i in 0:(n.queens-1)) {
    if (SquareIsFree(chess.board, n.queens, row.i)) {
      chess.board <- SetQueen(chess.board, n.queens, row.i)
      if (chess.board$column.j == n.queens) {
        solved.list <- get('solutions', envir = scope)
        solved.list[length(solved.list) + 1] <- paste(chess.board$queens,
                                                      collapse = " ", sep = "")
        assign('solutions', solved.list, envir = scope)
      } else {
        PlaceNextQueen(chess.board, n.queens, scope)
      }
      chess.board <- RemoveQueen(chess.board, n.queens, row.i)
    }
  }
}

# Formats the output of the solutions
OutputSolutions <- function(solutions, n.queens, filename) {
  if (length(solutions) == 0) {
    cat(paste("No solutions were found for the ", n.queens,
              "-Queens problem:\n", collapse = "", sep = ""))
  } else {
    cat(paste("The solver found ", length(solutions), " solutions for the ",
              n.queens, "-Queens problem\n", collapse = "", sep = ""),
        file = filename, append = FALSE)
    solutions <- paste("Solution ", seq(1, length(solutions)), ":\t", solutions,
                       collapse = NULL, sep = "")
    cat(solutions, file = filename, sep = "\n", append = TRUE)
  }
}

# Starting point for the N-Queens solver
SolveNQueens <- function(n.queens, filename = "") {
  if (n.queens > 0) {
    solver.env <- new.env()
    solver.env$solutions <- character(0)
    PlaceNextQueen(InitializeBoard(n.queens), n.queens, solver.env)
    OutputSolutions(solver.env$solutions, n.queens, filename)
  }
}




# Brute force, see the "Permutations" page for the next.perm function
safe <- function(p) {
  n <- length(p)
  for (i in seq(1, n - 1)) {
    for (j in seq(i + 1, n)) {
      if (abs(p[j] - p[i]) == abs(j - i)) return(F)
    }
  }
  return(T)
}

queens <- function(n) {
  p <- 1:n
  k <- 0
  while (!is.null(p)) {
    if(safe(p)) {
      cat(p, "\n")
      k <- k + 1
    }
    p <- next.perm(p)
  }
  return(k)
}

next.perm <- function(p) {
  n <- length(p)
  i <- n - 1
  r = T
  for (i in seq(n - 1, 1)) {
    if (p[i] < p[i + 1]) {
      r = F
      break
    }
  }
  
  j <- i + 1
  k <- n
  while (j < k) {
    x <- p[j]
    p[j] <- p[k]
    p[k] <- x
    j <- j + 1
    k <- k - 1
  }
  
  if(r) return(NULL)
  
  j <- n
  while (p[j] > p[i]) j <- j - 1
  j <- j + 1
  
  x <- p[i]
  p[i] <- p[j]
  p[j] <- x
  return(p)
}

print.perms <- function(n) {
  p <- 1:n
  while (!is.null(p)) {
    cat(p, "\n")
    p <- next.perm(p)
  }
}
queens(8)
queens(10)
plot(queens(8))
print.perms(2)

queens(8)