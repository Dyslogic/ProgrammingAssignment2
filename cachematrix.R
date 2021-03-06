## These functions computes the inverse of a matrix or caches the same inverse if it has been already calculated

## makeCacheMatrix stores a martix, and its inverse in a cached value and sets or returns the inverse
## using the getInverse or setInverse methods.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInverse <- function(solve) inv <<- solve
	getInverse <- function() inv
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## cacheSolve calculates the inverse of matrix x, or returns the cached value of the same inverse

cacheSolve <- function(x, ...) {
	## return the inverse of the matrix 'x' if it was calculated by using the makeCachmeMatrix function 'getInverse'
	inv <- x$getInverse()
		if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	## otherwise calculate the inverse and cache it
	data <- x$get()
	inv <- solve(data)
	x$setInverse(inv)
	inv
}
