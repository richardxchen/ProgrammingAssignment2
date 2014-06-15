## Coursera R Programming
## Assignment: Caching the Inverse of a Matrix

## The "makeCacheMatrix" function creates a special "matrix" object
## that can cache its inverse.
## The input is an inversable matrix.
## The output is a list of four functions that store and retrieve the matrix
## and its inverse cache. 

makeCacheMatrix <- function(x = matrix()) {
    mCache <- NULL
    set <- function(y) {
        x <<- y
        mCache <<- NULL
    }
    get <- function() x
    setmCache <- function(minverse) mCache <<- minverse
    getmCache <- function() mCache
    list(set = set, get = get,
         setmCache = setmCache,
         getmCache = getmCache)
}

## The "cacheSolve" function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then the cacheSolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

    minverse <- x$getmCache()
    if(!is.null(minverse)) {
        message("getting cached data")
        return(minverse)
    }
    data <- x$get()
    minverse <- solve(data, ...)
    x$setmCache(minverse)
    minverse
}
