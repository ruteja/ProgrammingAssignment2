## makeCacheMatrix and cacheSolve are two functions for effciently calculating a matrix inverse.
## Illustrates lexical scoping ie variable value is used where it is defined.

## makeCacheMatrix function creates a matrix. Also creates 4 "methods" or subfunctions
## namely set,get, setsolve and getsolve to access the matrix and the inverse values.

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL ## inital value of inverse is null.
  set <- function(y) {## takes input
    x <<- y ## saves  to x
    m <<- NULL ## sets inverse to null
  }
  get <- function() x ## function returns original value of x
  setsolve <- function(solve) m <<- solve ## superassigns inverse (solve) to m
  getsolve <- function() m ##  Called from cacheSolve if inverse is not already calculated
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve) ## function can return only one value so a list is returned
}

## function CacheSolve gives the inverse of the matrix created in makeCacheMatrix.
# if the inverse is already calculated, uses the cached value. If not calls the solve function and return the inverse value
cacheSolve <- function(x, ...) { ## takes matrix created by makeCacheMatrix
  m <- x$getsolve() ## access inverse obtained from makeCacheMAtrix
  if(!is.null(m)) { ## if inverse is not null, ie already calculated returns cached value
    message("getting cached data") ## prints message that cached data used
    return(m) ## returns the solution
  }
  data <- x$get() ## if inverse is null, gets the data by using the get subfunction
  m <- solve(data, ...) ## calculates inverse
  x$setsolve(m) ## sets the inverse to m 
  m ## returns inverse
}
