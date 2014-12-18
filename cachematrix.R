## Put comments here that give an overall description of what your
## functions do  - # c=rbind(c(1, -1/4), c(-1/4, 1)) - Matrix format

## Write a short comment describing this function

makeCacheMatrix <- function(mtx = matrix()) {
  inverse <- NULL
  set <- function(x) {
    mtx <<- x;
    inverse <<- NULL;
  }
  get <- function() return(mtx);
  setinverse <- function(inv) inverse <<- inv;
  getinverse <- function() return(inverse);
  return(list(set = set, get = get, setinverse = setinverse, getinverse = getinverse))
}

cacheSolve <- function(mtx, ...) {
  inverse <- mtx$getinverse()
  if(!is.null(inverse)) {
    message("Message: Getting cached data...")
    return(inverse)
  }
  data <- mtx$get()
  invserse <- solve(data)
  mtx$setinverse(inverse)
  return(inverse)
}
