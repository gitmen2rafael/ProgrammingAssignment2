## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix creates a special "matrix", a list containing a function to
##    a.set the value of the matrix
##    b.get the value of the matrix
##    c.set the value of the inverse of the matrix
##    d.get the value of the inverse of the matrix
##
## cacheSolve creates the inverse of the special "matrix" created with
## the above function. It first checks to see if the inverse has already
## been created. If so, it gets the inverse from the cache and skips the
## computation. Otherwise, it creates the inverse of the data and sets the
## value of the inverse in the cache via the setsolve function.
##


## makeCacheMatrix returns a list of functions to set and get the matrix
## and its inverse
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) i <<- solve
        getsolve <- function() i
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve creates the inverse of the matrix created by makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getsolve()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setsolve(i)
        i
}
