## 2 Functions makeCacheMatrix and cacheSolve to 
## cache the inverse of matrix

## I am creating here a special matrix object that will be used to cache the inverse

makeCacheMatrix <- function(x = matrix()) {

        ## Initializing the inverse property
    i <- NULL

    ## Method used to set the matrix
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    ## Method used to get the matrix
        get <- function() {
    	m
    }

    ## Method used to set the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }

    ## Method used to get the inverse of the matrix
    getInverse <- function() {
        ## Returning the inverse property
        i
    }

    ## Return the methods in the list
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## Computing here the inverse of the special matrix returned by "makeCacheMatrix".
## If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        ## Reading getInverse of the above function and storing it in m
        m <- x$getInverse()

    ## Return the inverse if it is already set
    if( !is.null(m) ) {
            message("getting/reading cached data")
            return(m)
    }

    ## Get the matrix from our object
    data <- x$get()

    ## Calculating the inverse by using matrix multiplication
    m <- solve(data) %*% data

    ## Set the inverse to the object by setInverse
    x$setInverse(m)

    ## Returning the matrix
    m
}
