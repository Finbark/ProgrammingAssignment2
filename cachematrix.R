## Put comments here that give an overall description of what your
## functions do

## Function creates special "matrix". which contains a list of functions that set
## and get the matrix value and also set and get the value of its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Function checks to see if inverse already exists, and if so it extracts it
## If no inverse exists, it calculates it and then caches it.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if (!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
}
