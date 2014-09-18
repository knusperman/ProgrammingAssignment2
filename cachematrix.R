## The two function makeCacheMatrix and cacheSolve do matrix solving in an efficient
## way. The outcome is only computed if necessary. 

## Example:
## matrix <- matrix(sample(1:100,size=25, replace=TRUE), nrow=5)
## cachem <- makeCacheMatrix(matrix)
## cacheSolve(cachem) #doing the computation
## cacheSolve(cachem) #getting the cache

#this function takes a matrix and returns a list of 4 functions for getting/setting
#use the result in cacheSolve for efficient calculations.
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


#takes input from makeCacheMatrix and only computes the inverse if
#necessary. the cached version is taken otherwise
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}