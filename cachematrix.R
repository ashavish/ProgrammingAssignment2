## makeCacheMatrix and cacheSolve functions calculate the inverse of a
## matrix.The inverse is computed the first time for a given matrix and stored
## in a cache.Subsequent requests for inverse for the same matrix is retrieved 
## from the cache.


## The function makeCacheMatrix takes in a matrix arguement and contains 
## enclosed functions.The variable x is used to store the matrix and the variable
## minv is used to store the inverse of the matrix.The enclosed functions 
## either set or get these values.
## set function - Sets the value of x to the arguement passed and initializes the 
## variable minv
## get function - returns the value of x 
## setminv function - sets the value of minv to the arguement passed
## getminv function - returns the value of minv



makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  get <- function() x
  setminv <- function(inverse) minv <<- inverse
  getminv <- function() minv
  list(set = set, get = get,setminv= setminv,getminv = getminv)
}


## The function cacheSolve is run using the object created by makeCacheMatrix
## The function first calls getminv function to check if an inverse exists in the 
## cache.If it exists,then it returns the inverse from the cache else,it gets the 
## matrix using get function and calculates the inverse using the solve function.
## The inverse is then stored in the cache using the setminv function.

cacheSolve <- function(x, ...) {
  minv <- x$getminv()
  if(!is.null(minv)) {
    message("Getting cached data")
    return(minv)
  }
  data <- x$get()
  minv <- solve(data, ...)
  x$setminv(minv)
  minv
}
