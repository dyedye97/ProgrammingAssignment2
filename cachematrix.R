## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize the inverse as NULL

    # Function to set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL  # Reset the inverse cache when the matrix changes
    }

    # Function to get the matrix
    get <- function() x

    # Function to set the inverse
    setInverse <- function(inverse) inv <<- inverse

    # Function to get the cached inverse
    getInverse <- function() inv

    # Return a list of the above functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()  # Get the cached inverse

    # If the inverse is already cached, return it
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    # Otherwise, compute the inverse
    data <- x$get()  # Get the matrix
    inv <- solve(data, ...)  # Compute the inverse
    x$setInverse(inv)  # Cache the inverse

    inv  # Return the inverse
}
