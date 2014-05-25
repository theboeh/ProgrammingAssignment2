## The functions in this library aid in working with the
## inverse of a matrix by caching it so that it does not
## need to be recalculated for each use.

## makeCacheMatrix returns a list of setter and getter functions
## for a matrix and the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## cacheSolve takes the result of makeCacheMatix and calculates the
## inverse of the matrix and returns the inverted matrix.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}
