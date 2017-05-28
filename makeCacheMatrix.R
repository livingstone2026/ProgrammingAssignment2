
## makeCacheMatrix and cacheSolve are a combo to 
## 1) calculate the inverse of a matrix A of nxn dimension
## if it has not been calculated before
## 2) Keep the inverse of A in cache
## 3) retrieve the inverse of A on demand from cache if A remains the same 
## As calculating the inverse matrix could be time consuming, retrieving
## the inverse from cache is a more efficient approach.

## makeCacheMatrix will 
## 1. set the matrix input
## 2. get the matrix input
## 3. set the inversed matrix 
## 4. get the inversed matrix
## in the same environment of the function

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          print(y)
          m <<- NULL
     }
     get <- function() x  
     setinvMat <- function(invMat) m <<- invMat
     getinvMat <- function() m
     list(set = set, get = get,
          setinvMat = setinvMat,
          getinvMat = getinvMat)
     #print(y)     
}


## CacheSolve is to retrieve the saved inverse matrix of x if
## x has been saved before.
## If x is a new matrix, it will calculate and return the inverese of x

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     m <- x$getinvMat()
     if(!is.null(m)) {
          message("getting inverse matrix")
          return(m)
          #in this if.. loop, 
          #the function ends here and subsequent codes will not be run
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setinvMat(m)
     m
     
}