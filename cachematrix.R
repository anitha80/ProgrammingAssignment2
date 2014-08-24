## A pair of functions that cache the inverse of a matrix

## Matrix object that can cache its inverse

makeCacheMatrix <- function( m = matrix() ) {
	## Initialize the inverse property

    i <- NULL

    ## Set the matrix
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }
    ## Get the matrix
    get <- function() {
    	## Return matrix
    	m
    }
    ## Set the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }
    ## Get the inverse of the matrix
    getInverse <- function() {
        ## Return inverse property
        i
    }
    ## Return list of methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Compute the inverse of  matrix returned by "makeCacheMatrix"
## If the inverse has already been calculated and matrix has not changed
## then  "cachesolve" retrieves inverse from the cache.
cacheSolve <- function(x, ...) {

    ## Return inverse of 'x'
    m <- x$getInverse()

    ## return inverse if its already set
    if( !is.null(m) ) {
            message("Cached Data")
            return(m)
    }

    ## Get the matrix from our object
    data <- x$get()

    ## Inverse using matrix multiplication
    m <- solve(data) %*% data

    ## Set the inverse
    x$setInverse(m)

    ## Return matrix
    m
}