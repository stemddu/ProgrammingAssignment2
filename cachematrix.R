##makeCacheMatrix creates a list containing the functions to:
##- set the value of a matrix
##- get the value of the matrix
##- cache the value of inverse
##- get the cache value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) m <<- solve
	getsolve <- function() m
	list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

##The cacheSolve function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated (and the matrix has 
##not changed), then the cacheSolve should retrieve the inverse from the cache and skips 
##the computation. Otherwise, it calculates the inverse of the data and sets the value 
##in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	
	m <- x$getsolve()
	if (!is.null(m)) {
		message("getting cached matrix")
		return(m)
	}
	mat <- x$get()
	m <- solve(mat, ...)
	x$setsolve(m)
	m
}
