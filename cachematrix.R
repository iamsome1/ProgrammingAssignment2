## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv_m <- NULL   ##Set inverse matrix as NULL
  ##Setting new argument to the object
  set <- function(y) {
    x <<- y
    inv_m <<- NULL
  }
  
  ##Returns the object
  get <- function() x
  
  ##Set inverse
  setInv_m <- function(inv) inv_m <<- inv
  ##get inverse
  getInv_m <- function() inv_m
  list(set = set, get = get,
       setInv_m = setInv_m,
       getInv_m = getInv_m)
}


## Write a short comment describing this function
##This function computes the inverse of the special "matrix" returned 
##by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve 
##the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInv_m()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data) ##Calculate the inverse
  x$setInv_m(i)
  i
}