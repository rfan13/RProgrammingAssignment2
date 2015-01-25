# The first function, makeCacheMatrix is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the matrix inverse
# get the value of the matrix inverse

## Write a short comment describing this function
# i is the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }

## This is the actual code setting the inverse. If solution is in the cache, it will 
## be retrieved. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()     # gets the whole matrix
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
