## The following two functions act hand-in-hand to utilize cache to 
## store an inverse of a matrix that can be called many times over while
## the matrix has not changed. 

## makeCacheMatrix function creates a special "matrix" object that has 
## the ability to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) {
		inv <<- inverse
	}
	getinverse <- function() inv
	list(set = set, get = get, 
		setinverse = setinverse, 
		getinverse = getinverse)
}


## cacheSolve function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

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
