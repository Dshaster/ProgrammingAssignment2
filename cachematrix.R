## The function will by design cache the inverse of a matrix.
## It will create a matrix and cache its inverse.

## This function creates a special “matrix” object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- matrix(NULL*length(x), nrow = nrow(x), ncol = ncol(x))
	set <- function(y) {
		x <<- y
	        inv <<- matrix(NULL*length(x), nrow = nrow(x), ncol = ncol(x))
		}
	get <- function() x
	setinvs <- function(invs) {
		for (i in seq_len(nrow(invs)))
			for (j in seq_len(ncol(invs)))
				inv[i, j] <<- invs[i,j]
				}
	getinvs <- function() inv
	list(set = set, get = get,
	setinvs = setinvs,
	getinvs = getinvs)
}


## calculates the inverse of the special “matrix” created with the above function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinvs()
        if(!is.na(inv[1,1])) {
        message("getting cached matrix")
        return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinvs(inv)
        inv

}
