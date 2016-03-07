## 
## First we create a function makeCacheMatrix
## which creates a special "matrix" by applying the following functions: 

## set - sets the value of the matrix
## get - gets the value of the matrix
## setInv - sets the cached value (inverse of the matrix)
## getInv - gets the cached value (inverse of the matrix)


makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInv <- function(inverse) inv <<- inverse
	getInv <- function() inv
	list(set = set,
		get = get,
		setInv = setInv,
		getInv = getInv)
}


## The cacheSolve function returns the inverse of the "matrix" x
## created by makeCacheMatrix
## If the inverse was already calculated (and the matrix did not change) 
## then cacheSolve will retrieve the inverse from the cache 

cacheSolve <- function(x, ...) {
        inv <- x$getInv()
	if (!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	mat <- x$get()
	inv <- solve(mat, ...)
	x$setInv(inv)
	inv
}