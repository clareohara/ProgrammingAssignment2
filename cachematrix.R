## These functions allow matrix inverses (which can be time-consuming to 
## compute) to be cached so that they can be looked up after the first 
## computation rather than being repeatedly calculated.

## This function stores a matrix and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {

	m <- NULL

	## first function sets the value of the matrix
	set <- function(q) {
		p <<- q
		m <<- NULL
	}
	## second function gets the value of the matrix
	get <- function() p

	##third function sets the value of the inverse
	setinverse <- function(solve) m <<- solve

	##fourth function gets the value of the inverse
	getinverse <- function() m

	## output a list of the four functions
	list(set = set, get = get, setinverse = setinverse,
		getinverse = getinverse)
}



## This function checks if the matrix inverse already exists. If so, 
## it gets it from the cache. If not, it calculates the inverse and 
## stores it in the cache.

cacheSolve <- function(x, ...) {

	## check, using getinverse, if matrix inverse has already
	## been computed and cached - if so, return matrix inverse.
	m <- p$getinverse()
		if(!is.null(m)){
			message("getting cached data")
			
			## Return a matrix that is the inverse of 'x'
			return(m)
		}

		## if matrix inverse does not yet exist, calculate it 
		## and cache it using the setinverse function.
		data <- p$get()
		m <- solve(data, ...)
		p$setinverse(m)
		
		## Return a matrix that is the inverse of 'x'
		m
}      
