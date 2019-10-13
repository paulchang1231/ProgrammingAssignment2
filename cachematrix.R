## The paired functions aim to create objects that stores a matrix
## and its inverse, and retrieve the inverse that has been cached.

## The first function, makeCacheMatrix(), stores a matrix and its inverse 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The second function, cacheSolve, returns the cached value if there's one.
## Otherwise, it calculates the inverse of the input matrix, return the result, and
## stores it in the makeCacheMatrix()

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
