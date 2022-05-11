# These functions run sequentially allow for a matrix to be created, its 
# inverse to be solved and cached in the global environment to save future 
# computation time. They are intended to work together and provide little 
# value independently


# This function creates the matrix (or assigns an existing one), 
# assigns/initializes the  necessary objects, and sets names to the list 
# objects.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


# Function which checks for cached inverse matrix and returns it if it exists # Otherwise it solves the new inverse matrix and stores it in cache.

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}
