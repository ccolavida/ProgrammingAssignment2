## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function requires one argument of a matrix class to result into 
# creates a list of four elements that allow to get the matrix and its inverse
#stored as variable$getInverse 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- solve(x)
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
#This function returns the inverse from the cache if it is not null

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matr <- x$get()
  inv <- solve(matr, ...)
  x$setInverse(inv)
  inv
}

