## These two functions provide a cached version of a matrix inverse
## The technique used is function closure and a "super matrix"
## A "super matrix" object is a list containing the matrix and its inverse as
## parent level state variables that remain in memory.
## The object has 4 functions to set and get the matrix or its cached inverse

## The following function initiates a "super matrix" with a list of 4 functions
## It also includes 2 parent level states for the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
        x <<- y
        inv <<- NULL
      }
      get <- function() x
      setinv <- function(inverse_matrix) inv <<- inverse_matrix
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## This function returns the inverse of a matrix corresponding to a super matrix
## It first checks to see if the inverse already exists in the state variable 
##(through getinv())
## If it already exists is simply returns the inverse. Otherwise, it calculates
## and returns the inverse and updates the cached value with it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse_matrix <- x$getinv()
  if(!is.null(inverse_matrix)) {
    message("getting cached inverse")
    return(inverse_matrix)
  }
  data <- x$get()
  inverse_matrix <- solve(data, ...)
  x$setinv(inverse_matrix)
  inverse_matrix
}