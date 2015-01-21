## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## `makeCacheMatrix` can create a special matrix which is a list with 
## four functions :
## 1) `set()` to set the value of the matrix
## 2) `get()` to get the current value
## 3) `setinv()` to set the inverse of the matrix
## 4) `getinv()` to get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
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


## Write a short comment describing this function
## `cacheSolve()` will calculate the inverse of the matrix created by above function.
## Actually it will first check whether the matrix's inverse exits or not. 
## If it already exists just use the matrix's `getinv()` to get inverse from cache
## otherwise calculates it and sets the inverse in th cache via the `setmean()` function

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
