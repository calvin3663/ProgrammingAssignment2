## ProgrammingAssignment2
## cachematrix.R
## Author: calvin3663@hotmail.com
## Description: The following are two functions to cache the inverse of a matrix.

## makeCacheMatrix() creates a list that has 4 member functions:
##   set(): set the orignal matrix
##   get(): returns the orignal matrix data
##   setinverse(): apply inverse of the orignal matrix
##   getinverse(): returns the inversed matrix data
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL  # m = inverse matrix result

  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setinverse <- function(inverse) {
    m <<- inverse
  }
  
  getinverse <- function() {
    m
  }
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

} # End makeCacheMatrix()


## cacheSolve() returns the inverse of input matrix 'x'
## If the inverse matrix already exists, returns directly from cache instead
## Else apply inverse of 'x', stores in a cache variable using setinverse(), and return the result 'inv_x'
cacheSolve <- function(x, ...) {

  m <- x$getinverse()
  
  
  # Check if inverse matrix already exists in cache
  if(!is.null(m)) {
    
    message("getting cached data")
    
    return(m) # Return inverse matrix from cache variable
  
  } else {

    data <- x$get()
    m <- solve(data, ...) # Calculate inverse matrix using solve()

    x$setinverse(m) # Store results in cache variable
    
    return(m) # Return inverse matrix
    
  } # End If

} # End cacheSolve() 
