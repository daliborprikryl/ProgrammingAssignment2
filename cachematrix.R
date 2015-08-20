## Below are two functions "makeCacheMatrix" and "cacheSolve" that are used to create
## a special object that stores a matrix and cache's its inversion.

## "makeCacheMatrix "function creates a special "matrix", which is really a list
## of functions (set, get, setinverse, getinverse)

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## "cacheSolve" function calculates the inverse of the special "matrix"
## returned by makeCacheMatrix above using the solve function. It returns the inversion
## from cache if already calculated,calculates inversion otherwise.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$getinverse()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
