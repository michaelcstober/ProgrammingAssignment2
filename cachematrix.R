## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  cachedinvertedmatrix <- NULL
  set <- function(y) {
    x <<- y
    cachedinvertedmatrix<<- NULL
  }
  get <- function() x
  setinvertedmatrix <- function(invertedmatrix) cachedinvertedmatrix <<- invertedmatrix
  getinvertedmatrix <- function() cachedinvertedmatrix
  list(set = set, get = get,
       setinvertedmatrix = setinvertedmatrix,
       getinvertedmatrix = getinvertedmatrix)

}
## m= cachedinvertedmatrix
## 

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  cachedinvertedmatrix <- x$getinvertedmatrix()
  if(!is.null(cachedinvertedmatrix)) {
    message("getting chached data")
    return(cachedinvertedmatrix)
  }
  data <- x$get()
  cachedinvertedmatrix <- solve(data, ...)
  x$setinvertedmatrix(cachedinvertedmatrix)
  cachedinvertedmatrix
        ## Return a matrix that is the inverse of 'x'
}
