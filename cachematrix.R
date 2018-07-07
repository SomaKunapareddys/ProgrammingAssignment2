## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates a list of function with set,get setinverse and getinverse
makeCacheMatrix <- function(x = matrix()) {

	invrs <- NULL
   	set <- function(y) {
        	x <<- y
       		invrs <<- NULL 
    	}

   	get <- function() x
    	setInverse <- function(inverse) invrs <<- inverse
    	getInverse <- function() invrs
    	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## CacheSolve checks for matrix is inverse or not of created matrix using function 'makeCacheMatrix'
## if inversed retunrs the inversed matrixed otherwise inverse the matrix
cacheSolve <- function(x, ...) {
        
	invrs <- x$getInverse()
    	if (!is.null(invrs)) {
        	message("Getting cached data")
    	    	## Return a matrix that is the inverse of 'x' with cache
        	return(invrs)
    	}
    	
	gdata <- x$get()
    	invrs <- solve(gdata, ...)
    	x$setInverse(invrs)
    	## Return a matrix that is the inverse of 'x' without Cache
    	invrs
}
