## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function will create a special matrix object that will cache its inverse. Using the solve() function that gives 
##The inverse of a matrix
#And we have to assume the matrix is invertible

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inverse <<- solve
  getinv <- function() inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
#This will return the inverse of the special Matrix created above with makeMatrix function.
cacheSolve <- function(x, ...) {
  inverse <- x$getinv()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinv(inverse)
  inverse
}

##How to use example
##1. x <- matrix(1:4,2,2)
##2. cacheSolve(makeCacheMatrix(x))
##3. See if it is correct by solve(matrix(1:4,2,2))