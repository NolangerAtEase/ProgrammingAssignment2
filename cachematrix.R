## These two functions respctively create a list of a matrix and its inverse,
## and the second checks if the inverse has already been calculated, and if so,
## retrieves it without recalculation

## Similar to the MakeVector Function, makeCacheMatrix takes in a matrix and
## creates a list of the matrix's value and inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## cacheSolve returns the inverse of a matrix x. If the inverse has already
## been calculated, it returns the cached value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv  
}
