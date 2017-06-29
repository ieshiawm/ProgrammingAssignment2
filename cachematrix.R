## The function below creates a matrix
## The makeCacheMatrix function creates a special "matrix", 
## which is really a list containing a function to:

    ## 1. set the value of the matrix
    ## 2. get the value of the matrix
    ## 3. set the value of the inverse matrix
    ## 4. get the value of the inverse matrix	

makeCacheMatrix <- function(x = matrix()) {

	inv <- NULL
	set <- function(y) {
			x <<- y
			inv <<- NULL
	}
	get <- function() x
	setinversse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get,
		 setinversse = setinversse,
		 getinverse = getinverse)
}

## The cacheSolve function computes the inverse of the special "matrix"
## returned by makeCacheMatrix function. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve will 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if(!is.null(inv)) {
			message("getting cached data")
			return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setinversse(inv)
	inv
		
}
