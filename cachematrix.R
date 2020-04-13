# Matrix inversion is a time consuming computation and caching would be  
# helpful to inverse the matrixto caching the inverse of a matrix rather 
# than compute it repeatedly. 

#Following two functions are used to cache the inverse of a matrix.
# 1. makeCacheMatrix
# 2. cacheSolve

# makeCacheMatrix creates a following list containing a function 
# 1. Set the value of the matrix.
# 2. Get the value of the matrix.
# 3. Set the value of inverse of the matrix.
# 4. Get the value of inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  # Set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # Get the value of the matrix
  get <- function() x
  # Set the value of inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  # Get the value of inverse of the matrix
  getinverse <- function() inv
  # Generate the list
  list(set=set, 
       get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}


# The cacheSolve function returns the inverse of the matrix. 
# It first checks if the inverse has already been computed.
# - If yes, it gets the result and skips the computation. 
# - If no, it computes the inverse, 
# sets the value in the cache via # setinverse function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {

    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data.")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
    ## Return a matrix that is the inverse of 'x'
  }

