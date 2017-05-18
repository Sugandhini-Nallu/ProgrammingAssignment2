## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it repeatedly 


## `makeCacheMatrix`: This function creates a special "MATRIX" which is a list containing functions to:
## 	set the value of the Matrix
##   get the value of the Matrix
## 	set the value of the Inverse Matrix
## 	get the value of the Inverse Matrix

makeCacheMatrix <- function(x = matrix()) {
  #x is a input, default as empty matrix
  i <- NULL  #initialised as a null object
  set <- function(y) {
    x <<- y    # assign y to x
    i <<- NULL # reset i to NULL
  }
  get <- function() x  # returns matrix x
  setinverse <- function(solve)  # set the inverse matrix
    i <<- solve
  getinverse <- function() i # returns inverse matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## `cacheSolve`: This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
## If the inverse has already been calculated (and the matrix has not changed), then `cacheSolve` should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse() # returns a object from MakeCacheMatrix()
  if(!is.null(i)) { #Checks if i is not null i.e. the inverse is already calculate
    message("getting cached data")
    return(i)
  }
  # calculate and get the inverse of the matrix
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i 
}

