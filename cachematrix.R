## This is the 2nd assignment for R Programming.
## The following two functions provide solutions for cashing matirx inversion,
## which saves time when calculating the inversion repeatedly.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	#inversion of the matrix for caching
	inv <- NULL

	#set the original matrix
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}

	#get the original matrix
	get <- function() x

	#set the inversion of the matrix
	setInverse <- function(inverse) inv <<- inverse

	#get the inversion of the matrix
	getInverse <- function() inv

	list(set = set,
		get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
       inv <- x$getInverse()

       #the inversion was already cached
       if(!is.null(inv)) {
       		message("Get the cached matrix inversion")
       		return(inv)
       }

       #generate matrix inversion for caching
       data <- x$get()
       if(is.na(data)) {
       		message("Data is NA!")	#the original matrix is NA
       }
       inv <- solve(datda, ...)

       #set the inversion inside the x for caching
       x$setInverse(inv)
       inv
}