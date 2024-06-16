## The following two functions work together to calculate the inverse of a matrix
## quickly by caching the result. It ensures that the inverse is 
## calculated only once (unless the matrix changes) which reduces computational time. 

## The first function creates a special "matrix" object that can cache its inverse.
## It returns a list containing functions to set and get the matrix and its inverse.
## First two objects are initialized - x and inv

makeCacheMatrix <- function(x = matrix()) {
  ## inv is set to NULL, initialized as an object 
  ## within makeCacheMatrix() environment to be used by later code in the function.
  inv <- NULL 
  
  ## Function to set the matrix value
   set <- function(y) {
    x <<- y
    
    ## Reset inverse cache when matrix changes
    inv <<- NULL
   }
  
  ## Function to get the matrix value
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  
  
  ## assigns each of these functions as an element within a list(), 
  ## and returns it to the parent environment.
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  
  ## if the value here is not equal to NULL, we have a valid, cached inverse and 
  ## can return it to the parent environment
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  
  # Only calculate inverse if not already cached
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
