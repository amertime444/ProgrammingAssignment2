## Pair of functions below aim to cache the inverse of a matrix so that it is
## not calculated multiple times

## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## first, we set then get the value of the matrix
## then, we set then get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y) {
      x <<- y           ## the <-- operator causes a search for variable in 
      inv <<- NULL      ## parent environment first (makeCacheMatrix) in this case
}
get <- function() x
setinverse <- function(inverse) inv <<- inverse
getinverse <- function() inv
list(set = set, get = get, 
     setinverse = setinverse, 
     getinverse = getinverse)
}


## cacheSOlve computes the inverse of the special matrix returned by makeCacheMatrix
## If the inverse has been calculated, cacheSolve retieves it from the cache

cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
      if(!is.null(inv))  {      ## if inverse is not NULL
            message("getting cached data")
            return(inv)
      }
      
      data <- x$get()
      inv <- inverse(data, ...)
      x$setinverse(inv)
      inv

      ## Return a matrix that is the inverse of 'x'
}
