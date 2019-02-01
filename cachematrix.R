#The function makeCacheMatrix creates a special "matrix", which is a list containing a function to

#set the value of the matrix x
#get the value of the matrix x
#set the value of the inverse matrix of the matrix x
#get the value of the inverse matrix of the matrix x

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

# This function cacheSolve use the solve function to obtain an inverse of x matrix, but before doing that 
# the function verify if the inverse has already been calculated, if does the function gets the result from the cache

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
