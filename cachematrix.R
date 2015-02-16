## These two functions create a special object 
## that stores a matrix and caches its inverse.

## The first function creates a list
## that contains a function to
## set and get the value of the matrix
## set and get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inverse <<- solve
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The second function first checks 
## if the inverse matrix is in cache
## if not it calculates the inverse
## if yes it gets the value from cache

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("Getting the inverse matrix from cache")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    ## Return a matrix that is the inverse of 'x'
    inverse
}
