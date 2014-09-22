## This pair of functions works together to cache the value of a matrix 
## inversion after it has been calculated once, avoiding costly computation

## This first function creates a list of four functions that set and get
## both the matrix itself and its inverse.  The four functions are set,
## get, setinverse and getinverse.

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


## This second function in the pair calculates the inverse of a matrix.
## Before it does, it checks to see if the inverse has already been 
## calculated and, if it has, simply returns the cached result, rather than
## recalculating it.  This avoids repeating a computationally intensive task.



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
