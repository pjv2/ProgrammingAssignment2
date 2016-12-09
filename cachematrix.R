
## The function makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ##initialize the inverse property
  inv_x <- NULL
  
  ##function to set the matrix
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  
  ##function to get the matrix
  get <- function() {
    x
  }
  
  ##method to set the inverse of the matrix
  setinverse<- function(inverse) {
    inv_x <<- inverse
  }
  
  ##method to get the inverse of the matrix
  getinverse <- function() {
    inv_x
  }
  
  ##return list of methods
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_x <- x$getinverse()
  if (!is.null(inv_x)) {
    message("getting cached inverse matrix")
    return(inv_x)
  } else {
    inv_x <- solve(x$get())
    x$setinverse(inv_x)
    return(inv_x)
  }
}

