## This pair of functions works together to cache the value of a matrix 
## inversion after it has been calculated once, avoiding costly re-computation

## This first function creates a list.  The list contains four functions 
## that can be used to get and set the matrix and its inverse.  The four 
## functions are:
##       1) set, 
##       2) get, 
##       3) setinverse, and 
##       4  getinverse.
##
## How to use this function:
## 
## y <- makeCacheMatrix()

makeCacheMatrix <- function(x = matrix()) {

	i <- NULL
	set <- function(y) {

		x <<- y
		i <<- NULL

	}
	get <- function() xx
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## This second function in the pair calculates the inverse of a matrix.
## Before it does, it checks to see if the inverse has already been 
## calculated and, if it has, simply returns the cached result, rather than
## recalculating it.  This avoids repeating a computationally intensive task.
##
## Example:
##
## x <- makeCacheMatrix(rnorm(16),4,4) (NB: the matrix must be square)
## cacheSolve(x)
##
## Notice that if you then run cacheSolve again, you get "Getting cached..."


cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'

	i <- x$getinverse()
	if(!is.null(i)) {
		message("Getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data)
	x$setinverse(i)
	i
}
