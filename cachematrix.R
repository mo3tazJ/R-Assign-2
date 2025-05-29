## Below are two functions that are used to create a special object that stores a matrix and caches its inverse.


## The First Function "makeCacheMatrex" creates a special matrix object that can cache its inverse
## which contain these methods:

## 1. set the value of the matrix

## 2. get the value of the matrix

## 3. set the value of the inverse

## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL # Initialize inverse cache
    
    # Setter used to update the matrix and invalidate inverse:
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # Getter to get the matrix:
    get <- function() x
    
    # Setter for inverse: Stores the computed inverse in cache (inv):
    setInverse <- function(inverse) inv <<- inverse
    
    # Getter to get the inverse:
    getInverse <- function() inv
    
    # Return a list of previuos methods:
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The Second Function "cacheSolve" will return the inverse of x from cache if it was calculated before
## else it will calculate it, save it to cache and return it

cacheSolve <- function(x, ...) {
    # Trying to get inverse from cache
    inv <- x$getInverse()
    # Checking the value of retrived inverse
    if(!is.null(inv)) {
        message("Getting cached Data")
        return(inv) # Return cached inverse if it is valid (Not Null)
    }
    # if inv is Null we need to compute it
    # Get the matrix
    data <- x$get()
    # compute the inverse of it
    inv <- solve(data, ...)
    # cache the result
    x$setInverse(inv)
    # Return computed inverse of matrix "x"
    inv
}
