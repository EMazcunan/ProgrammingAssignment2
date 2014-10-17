## The pair of functions makeCacheMatrix and cacheSolve allow to determine the inverse of a given matrix,  
##  skiping the calculations in case they have been done previously.

## The function makeCacheMatix takes a matrix as input and constructs from it a special "matrix", which is really a list of four functions.
##    (1) set(matrix): sets the value of the matrix
##    (2) get(): gets the value of the matrix
##    (3) setinverse(inverse): sets the value of the inverse
##    (4) getinverse(): gets the value of the inverse
## These output functions allow to test if the inverse of the input matrix is already in the cache, and get it if so, or set it otherwise. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}


## The function cacheSolve takes as input the list created  by makeCacheMatrix associated to a matrix.
## First, it makes use of the getinverse function in the input list in order to know if the inverse is already in the cache.
## If so, the inverse in the cache is returned, skiping calculations
## Otherwise, it calculates the inverse and sets it in the cache, via the setinversefunction in the input list. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

