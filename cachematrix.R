## Matrix inversion is usually a costly computation and their may be 
## some benefit to caching the inverse of a matrix rather than compute it repeatedly

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # clear old value by default
  s <- NULL
  # set new value by set function
  set <- function(y){
    x <<- y
    s <<- NULL
  }
  # get function
  get <- function()s
  # setters and getters function
  setsolve <- function(solve) s <<- solve
  getsolve <- function()s
  # return mapping of functions as output operation, just like functions class in OOP
  list(
  # Outter Name = Inner Name
    set = set,
    get = get,
    setsolve = setsolve,
    getsolve = getsolve
  )
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  # get solve value from x
  s <- x$getsolve()
  # check if s have a value then return it, otherwise get it
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  # get value of s
  data <- x$get()
  # calculate the solve
  s <- solve(data, ...)
  # save result into environment
  x$setsolve(s)
  ## Return a matrix that is the inverse of 'x'
  s
}
