## To Cache the inverse of a Matrix
## Inversion of matrix is a considered to be a costly computation
## Its better to cache the inverse of a matrix rather than computing it in repetitions
## Below given are a pair of functions that are useful to create 
## Inverse of  matrix as well as Caching it

## This function helps to create a spl "matrix" then cache its inverse

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function does the vice versa of the special matrix  created above
## If the inverse has already been calculated and there is no change to the matrix
## Then it should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		 inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
