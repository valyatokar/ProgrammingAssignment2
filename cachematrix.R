## makeCacheMatrix and cacheSolve are a pair of functions. makeCacheMatrix returnes a list
## that caches the inverse of an input matrix. cacheSolve takes the result of makeCacheMatrix as an input.
## If the inverse matrix has been calculated and cached it is returned. If the inverse matrix hasn't been
## calculated and cached it will be calculated

## makeCacheMatrix is a function that creates an object (list) that can cache its inverse.
## Input - matrix, output - list of functions
        
makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setinv <- function(solve) I <<- solve
        getinv <- function() I
        list(set = set, get = get,
                setinv = setinv,
                getinv = getinv)
}

## cacheSolve is a function that computes the inverse matrix for the object returned by makeCacheMatrix.
## input - list of functions, output - matrix

cacheSolve <- function(x, ...) {
        I <- x$getinv()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- x$get()
        I <- solve(data, ...)
        x$setinv(I)
        I
        ## Return a matrix that is the inverse of input matrix for makeCacheMatrix
}