## makeCacheMatrix is a function that should return a special list vector
## ^ Takes 'x' as an argument; 'x' is a square matrix to be inverted
makeCacheMatrix <- function(x = matrix()) {
  cached.inverted.matrix <- NULL
  set <- function(y) {
    x <<- y
    cached.inverted.matrix<<- NULL
  }
## return the matrix
  get <- function() x
  set.inverted.matrix <- function(inverted.matrix) cached.inverted.matrix <<- inverted.matrix
  get.inverted.matrix <- function() cached.inverted.matrix
  list(set = set, get = get,
       set.inverted.matrix = set.inverted.matrix,
       get.inverted.matrix = get.inverted.matrix)

}
## Invert the matrix returned by the makeCacheMatrix function UNLESS
## cached.inverted.matrix exists; if it does then return cache
cacheSolve <- function(x, ...) {
  cached.inverted.matrix <- x$get.inverted.matrix()
  if(!is.null(cached.inverted.matrix)) {
    message("getting chached data")
    return(cached.inverted.matrix)
  }
## get matrix from original function
  data <- x$get()
  cached.inverted.matrix <- solve(data, ...)
  x$set.inverted.matrix(cached.inverted.matrix)
## Return a matrix that is the inverse of 'x' 
  cached.inverted.matrix

}
