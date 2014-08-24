## These functions illustrate the scoping rules of R by 
##  implementing a strategy to cache the inverse of the matrix

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  solvedCache <<- NULL
  set <- function(y) {
    x <<- y
    solvedCache <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) solvedCache <<- inverse
  getinverse <- function() solvedCache
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned 
## by `makeCacheMatrix` above. If the inverse has already been calculated 
## (and the matrix has not changed), then `cacheSolve` should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  result <- x$getinverse()
  ## Return a matrix that is the inverse of 'x'
  if( !is.null(result) ) {
    message("getting cached data")
    return(result)
  }
  
  result <- solve(x$get())
  x$setinverse(result)
  return(result)
}
