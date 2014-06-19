## David Keith Gonda
## 18-Jun-2014
## R Programming - Programming Assignment 2
##
## Solve inverse of matrix (with caching to improve performance)
## on subsequent calls using the following pair of functions:


## makeCacheMatrix:
##
## This function takes an (optional) invertible matrix as a parameter 
## and returns a list object that encapsulates the matrix (if provided) 
## and provides for caching of it's inverse when calculated
##
## "member functions" of the cacheMarix object:
## set - takes a matrix as a parameter and stores it; also
##       clears the inverse cache
## get - returns the original matrix
## setinv - takes a matrix (i.e. inverse) and caches it.
## getinv - returns the cached inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve:
##
## this function takes a matrix object as created by makeCacheMatrix above,
## and returns the inverse of the matrix as an R matrix.
## The first time cacheSolve is run on a matrix object, the inverse is calculated,
## cached in the matrix object, and then returned.
## In subsequent runs on the same matrix object, the cached copy of the inverse
## is returned, avoiding the overhead of (unnecessary) matrix inversions.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## get the cached inverse
  inv <- x$getinv()
  
  ## if the cached inverse is populated, return that copy
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## otherwise, solve the inverse, put it in the cache, and
  ## then return it
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
