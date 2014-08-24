## A matrix is created that is stored in the cache, when the inversion of the matrix is not calculated it calculates it and returns it.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL #m will store the matrix 
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x # this function returns the value of the original matrix
        setinversion <- function(inversion) m <<- inversion
        getinversion <- function() m
        list(set = set, get = get,
             setinversion = setinversion,
             getinversion = getinversion)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	m<-x$getinversion()

	if(!is.null(m)){

		message("getting cached data")
		return (m)
	}
	data<-x$get()
	m<-solve(data,...)
	x$getinversion(m)
	m

}