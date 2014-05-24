## Matrix inversion is a lengthy and time consuming process. By caching 
## the inverse matrix when it was first calculated, we can speed the 
## computation of multiple inversion tasks. 

## The two functions given below are for the following purposes-

## makeCacheMatrix - It defines the 4 functions for setting and getting 
## the original matrix and its inverse. This function passes the handle
## to any variable which can be easily used for these 4 purposes.
## Here,
## theMat == x (from the example)
## passedMat == y
## invMat == m
## passedInvMat == mean
## setinv() == setmean()
## getinv() == getmean()

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

## cacheSolve - This function simply takes the handle variable created
## by the "makeCacheMatrix" function and then checks if the invMat is null.
## If so, it calculates the invMat using the setinv() function. On 
## subsequent calculations cacheSolve returns the already set value of 
## invMat without actual calculations.
## Here,
## theMat == x (from the example)
## invMat == m
## setinv() == setmean()
## getinv() == getmean()

cacheSolve <- function(theMat, ...) {
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