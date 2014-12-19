## Functions will store a numeric / matrix vector and cache the 'Matrix Inverse' value

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

# Calculate the 'Inverse' value of Matrix and cache it for next ref.
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
