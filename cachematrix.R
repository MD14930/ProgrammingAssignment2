## Two functions are being used here. 1st function creates a matrix and has the ability to cache the output of the second function. 
## 2nd function calculates the inverse of the matrix from the 1st function. However, if the matrix remains same, instead of calculating, it will bring the cached data


##The function takes a matrix as an argument and contains a list of 4 more functions - set: sets a new matrix, get: returns the matrix, setinverse: assigns and returns the inverse that was calculated (cache)
##getinverse: calculates the inverse. x, y and inv are the variables used

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
      x <<- y ; inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

##below function calculates the inverse of the matrix if the variable inv is null. 
## if inv is not null, it returns the cached value of inv, which will happen if a new matrix was not created

cacheSolve <- function(x, ...) {
  
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("Cached data is available")
    return(inv)
  }
  mat <<- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}