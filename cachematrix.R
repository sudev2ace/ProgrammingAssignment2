## Put comments here that give an overall description of what your
## functions do

## This function is used to cache the matrix and also its inverse, so that when we call the function during the
## during the second time, we will get the response quicker.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function()
                x
        setInv <- function(inverse)
                inv <<- inverse
        getInv <- function()
                inv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## This function is used to first check if there is a cached data present in the memory. If there is a cached
## data, then its printed and then we exit from the function.
## Otherwise we find the inverse of the matrix and cache the same into the memory.

cacheSolve <- function(x, ...) {
        inv <- x$getInv()
        if(!is.null(inv)){
                print("Getting cached data ...")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInv(inv)
        inv
}
