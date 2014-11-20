## Overall, these two functions below provide the ability to cache the inverse of a matrix, the purpose being to
## prevent a matrix inversion which is performed directly, a computation which is usually costly.

## makeCacheMatrix: This function creates a 'matrix' object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { # input x will be a matrix
	m <- NULL 	# m will be our 'inverse' matrix and it is reset to NULL every
				# time MakeCacheVector is called
	set <- function(y) {
	x <<- y
	m <<- NULL
	}
				
	get <- function() x  # this function returns the original matrix
	
	setinverse <- function(solve) m <<- solve 
				# this is called by cacheSolve during the first cacheSolve()
				# access and it will store the value using superassignment
				
	getinverse <- function() m 	# this will return the cached value to cacheSolve() on
								# the subsequent access(es)
	
	list(set = set, get = get,	# Each time makeCacheMatrix is called
	     setinverse = setinverse,	# we create new objects as defined in the functions
	     getinverse = getinverse)	# above in the code.	 
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve() should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { 	# the input x is an object created by makeCacheMatrix
	m <- x$getinverse() 				# accesses the object 'x' and gets the value of its inverse
	if(!is.null(m)) {		  		# if the matrix was already cached (not NULL) then
		message("getting cached data - inverse matrix")	#... provide this message to the console
		return(m)										#... return the value of the cached inverse matrix
	}
	matrix <- x$get()			# This code only executes if x$getinverse returned NULL
	m <- solve(matrix, ...)		# if m was NULL then we have to calculate the inverse value
	x$setinverse(m)				# store the calculated inverse matrix in x
	m							# return the inverse of the matrix
}