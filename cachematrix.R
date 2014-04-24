# ================================================================================
# makeCacheMatrix: Creates a special object that can cache the inverse of a matrix
# ================================================================================
# Args:
#   x: Matrix whose inverse wil be cached
#
# Returns:
#   A special object that allows caching x's inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Sets a new value for the wrapped matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Returns the value of the wrapped matrix
  get <- function() {
    x
  }
  
  # Sets the inverse of the matrix to cache
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  
  # Gets the cached inverse of the matrix
  getinverse <- function() {
    inv
  }
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# =================================================================================================
# cacheSolve: Computes the inverse of a matrix m where m is wrapped by a special object returned by 
#             makeCacheMatrix(m)
# =================================================================================================
#
# Args:
#   x: The special object returned by makeCacheMatrix(m)
#
# Returns:
#   The inverse of the matrix m 

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  if (!is.null(inv)) {
    message("getting cached data")
  } else {
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
  }
  
  return (inv)
}
