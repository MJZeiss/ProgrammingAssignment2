## Put comments here that give an overall description of what your
## functions do

#Create matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
    i <- NULL
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
	}

       get <- function() {m}

    setInverse <- function(inverse) {
        i <<- inverse
    }

    ## Method to get the inverse of the matrix
    getInverse <- function() {i}

    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


# Compute inverse of matrix returned by "makeCacheMatrix"
cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }
    data <- x$get()
    m <- solve(data) %*% data
    x$setInverse(m)
    m
}
