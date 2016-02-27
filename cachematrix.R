## Assignment: 2
## Author: Alexander Wiebe
## Date: Feb 27, 2016

## The following two functions deal with caching the inverse of a matrix

## makeCacheMatrix creates an object and once the inverse of the matrix is 
## calculated, it caches the result
makeCacheMatrix <- function(x = matrix()) {
   myInv <- NULL
   set <- function(y) {
      x <<- y
      myInv <<- NULL
   }
   get <- function() x
   setInverse <- function(solve) myInv <<- solve
   getInverse <- function() myInv
   list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## cacheSolve calculates the inverse of the matrix object from the above 
## function.  If the matrix object already has calculated the inverse 
## cacheSolve will use the cached version.
cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   myInv <- x$getInverse()
   if(!is.null(myInv)) {
      message("getting cached data")
      return(myInv)
   }
   data <- x$get()
   myInv <- solve(data, ...)
   x$setInverse(myInv)
   myInv
}