## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## this function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        ## set up the matrix
        set <- function(matrix) {
                x <<- matrix
                i <<- NULL
        }
        
        ## get the matrix
        get <- function() x
        
        ## sets the inverse of the matrix
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        ## Return a list
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## Get the matrix
        data <- x$get()
        ## Calculate the inverse
        i <- solve(data, ...)
         ## Set the inverse
        x$setInverse(i)
        ## Return the matrix
        i
}
