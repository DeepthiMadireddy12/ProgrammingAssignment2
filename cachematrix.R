## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Function to create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Clear the cached inverse when matrix changes
  }
  
  # Function to get the matrix
  get <- function() x
  
  # Function to set the inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Function to get the inverse
  getInverse <- function() inv
  
  # Return a list of functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Function to compute the inverse of the special matrix
cacheSolve <- function(x, ...) {
  # Try to get the cached inverse
  inv <- x$getInverse()
  
  # If the inverse is already calculated, return it with a message
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Otherwise, calculate the inverse
  data <- x$get()
  inv <- solve(data, ...)
  
  # Cache the inverse
  x$setInverse(inv)
  
  # Return the inverse
  inv
}
