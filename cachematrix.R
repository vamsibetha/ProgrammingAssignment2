## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(theMat = matrix()) {
  invMat <- NULL
  set <- function(passedMat) {
    theMat <<- passedMat
    invMat <<- NULL
  }
  get <- function() theMat
  setinv <- function(passedInvMat) invMat <<- passedInvMat
  getinv <- function() invMat
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(theMat, ...) {
  ## Return a matrix that is the inverse of 'x'
  invMat <- theMat$getinv()
  if(!is.null(invMat)) {
    message("getting cached data")
    return(invMat)
  }
  data <- theMat$get()
  invMat <- solve(data, ...)
  theMat$setinv(invMat)
  invMat
}
