## vgonzalez: This function is my solution for
## the Programming assignement for week 2, 'Lexical Scoping'

## vgonzalez: This function creates a special "matrix",
## which is really a list containing a function to:
##      set the value of the matrix
##      get the value of the matrix
##      set the value of the inverse matrix
##      get the value of the inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## vgonzalez: This function checks if we already have the inverted matrix
## cached and returns it. Otherwise, it calculates it, caches it on the
## special object and returns the calculated inverted matrix

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}