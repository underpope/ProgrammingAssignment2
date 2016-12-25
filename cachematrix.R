# Create the matrix in another workspace. It feels a bit like storing
# it in "subspace" because I'm a Star Trek fan.

makeCacheMatrix <- function(x = matrix()) {
    
    subspace <- function(y) {
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

# Return the inverse of the matrix.
cacheSolve <- function(x, ...) {
    
    inv <- x$getinverse()
    
    if(!is.null(inv)) {
        return(inv)
    }
    
    data <- x$get()
    
    inv <- solve(data, ...)
    
    x$setinverse(inv)
    
    inv    
}