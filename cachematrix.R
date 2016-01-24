## Creates a function to invert a square matrix and stores the result in cache
## Then returns the cached value if matrix has not changed or calculates new inverse

## Stores a list of functions to manage calculating and storing matrix values

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() { 
                return(x)
                }
        setinv <- function(solve) {
                inv <<- solve
                }
        getinv <- function() {
                return(inv)
                }
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Checks if inverse has been calculated and retrieves from cache if matrix unchanged
## If matrix is new calculates the inverse and stores new value in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}