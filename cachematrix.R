## The pair of functions are used to invert a matrix , and store
## the matrix in cache. The cached matrix will be used for retrive if 
## the matrix has not be changed


## Function: Create a special "matrix" object that can cache its inverse.
## Input: x is a optional parameter for initial matrix
makeCacheMatrix <- function(x = matrix()) {

        inversed_m <- NULL
        
        ## Set a new matrix value to the special "matrix"object, and 
        ## the inversed_m is set to NULL
        set <- function(y) {
                x <<- y
                inversed_m <<- NULL
        }
        
        ## Get the current matrix value
        get <- function() x
        ## Set the inversed_m
        setim <- function(im) inversed_m <<- im
        ## Get the inversed_m
        getim <- function() inversed_m
        list(set = set, get = get,
             setim = setim,
             getim = getim)
}

## Function: Computes the inverse of the special "matrix" returned by 
##   makeCacheMatrix  If the inverse has already been calculated 
##   (and the matrix has not changed), then the cachesolve should
##   retrieve the inverse from the cache.
## Input: x is the special "matrix" object 
cacheSolve <- function(x, ...) {

        
        im <- x$getim()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setim(im)
        im
        ## Return a matrix that is the inverse of 'x'
}