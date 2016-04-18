## Calculating the inverse of a matrix is a fast operation but for a very long matrix, 
## it may take too long to compute, especially if it is done repeatedly like in a loop. 
## If the contents of the matrix is not changing, it may make sense to cache the inverse 
## matrix so that when we need it again, it can be located in the cache instead of being
## recalculated again, saving time.
## With these two functions we first create a special object that stores a matrix and
## caches its inverse and the other function calculates the inverse of a matrix. 

## This function creates a special matrix object that stores a matrix and it inverse and 
## holds four fuctions to manage this content. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() 
    return(x)
  setinv <- function(solve) 
    m <<- solve
  getinv <- function()
    return(m) 
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function calculates the inverse a matrix created by the above function 
## makeCacheMatrix. If the inverse has already been calculated and the 
## matrix has not changed, then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  return(m)
}
