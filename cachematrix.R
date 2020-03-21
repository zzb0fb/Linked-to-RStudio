## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Using the example provided for the vector mean, I ahve applied same approach
# to dor this assigment

makeCacheMatrix <- function(x = matrix()) {
  # the function receives a matrix as parameter
  m <- NULL     # clear the value
  set <- function(y) {
    x <<- y     # set the value
    m <<- NULL  # clear the cache
  }
  get <- function() x   #here get the value of the matrix
  setinverse <- function(inverse) m <<- inverse   
  #function to set the inverse of the matrix
  getinverse <- function() m
  #function to get the inverse of the matrix
  
  #And finally return a list with the four values
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {    # If cache is not empty then returns value and exit
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)  # Solve function calculates the inverse of the matrix
  x$setinverse(m)        # inverse matrix is cached
  m                      # inverse is returned
  
}
