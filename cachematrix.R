## Course 2: R Programming
## Week 3
## Peer-graded Assignment: Programming Assignment 2: Lexical Scoping

## In this second peer-graded assignment, we'll use the scoping rules
## of R to cache potentially time-consuming computations; in this way,
## we can look it up rather than recomputed.  



## This function creates a special "matrix" object that helps to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set,
       get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function calculates the inverse  of the sepecial "matrix"
## created with the above function. It first checks to see if the
## inverse has already been calculated. If so, it gets the result 
## and skips the computation. If not, it computes the inverse, 
## sets the value in the cache via setinverse function.
## It assumes that the matrix is always invertible. 

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setInverse(i)
  i
}

## Sample run

## x <- rbind(c(4,3), c(3,2))
## matrix <- makeCacheMatrix(x)
## matrix$get()

## [,1] [,2]
## [1,]    4    3
## [2,]    3    2

## cacheSolve(matrix)

## [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4

