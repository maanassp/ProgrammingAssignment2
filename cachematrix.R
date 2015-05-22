## The makeCacheMatrix is used to take a matrix as an input and store it. The get() is used to retrieve the inputted matrix. The setinverse() is used to set the inverse matrix. The getinverse() is used to retrieve the matrix inverse from the cache if it exists. ##

## Example input: source("cachematrix.R"), x <- makeCacheMatrix(), samplematrix <- matrix(c(4,3,3,2), 2, 2, byrow = TRUE), x$set(samplematrix), x$get(), cacheSolve(x), cacheSolve(x) ##


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
	  x <<- y
  	  m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns a matrix inverse. It first checks if the inverse exists. If it does, it retrieves it from the cache. If not, it computes the inverse and returns it. ##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
