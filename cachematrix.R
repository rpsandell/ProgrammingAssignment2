## The functions makeCacheMatrix and cacheSolve work in tandem to store a square matrix 
## and to calculate and cache the inverse of that square matrix.
## Because the function solve() is used throughout to calculate the inverse of the matrix
## only a square matrix should be passed to makeCacheMatrix.
## cacheSolve will return an error if the matrix passed to makeCacheMatrix is not square. 

## Usage: 
## pass a square matrix to makeCacheSolve:
    ## ex <- makeCacheMatrix(matrix(c(1, 17, 23, 42), 2)) -- sets the value of the "matrix" as a 2x2 matrix
      ## this can be confirmed by calling ex$get():
              ## [,1] [,2]
        ## [1,]    1   23
        ## [2,]   17   42
## pass the "matrix" to cacheSolve:
  ## cacheSolve(ex) calculates the inverse of the matrix and caches the result
        ## [,1]        [,2]
## [1,] -0.1203438  0.06590258
## [2,]  0.0487106 -0.00286533

## If cacheSolve(ex) is called again, the cached result will be pulled, rather than the inverse being calculated anew.

## makeCacheMatrix creates a pseudo-matrix that is in fact a list consisting of a function
## that performs the following operations:
  ## 1. set the value of the square matrix.
  ## 2. get the value of the square matrix.
  ## 3. set the value of the inverse of the square matrix.
  ## 4. get the value of the inverse of the square matrix.  

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve calculates the inverse of a square "matrix" stored using makeCacheMatrix
## and caches that inverse in setsolve.
## If cacheSolve is passed a "matrix" whose inverse has already been cached, then
## getsolve is pulled. 

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  return(m)
}

## Returns a matrix that is the inverse of "matrix" 'x'.