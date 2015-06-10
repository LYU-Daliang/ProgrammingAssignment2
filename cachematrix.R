## "makeCacheMatrix" and "cacheSolve" are a pair of functions which create a
## special object that stores a matrix and caches its inverse

## "makeCacheMatrix" contains 4 functions: set the matrix, return the matrix, 
## set the inverse of matrix, return the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        get <- function() x
        
        set_inverse <- function(inv) inverse <<- inv
        
        get_inverse <- function() inverse
        
        list(set = set, get = get, set_inverse = set_inverse, 
             get_inverse = get_inverse)
}


## "cacheSolve" tries to get inverse from "makeCacheMatrix", in case of NUll,
## calculates the inverse and set it with "makeCacheMatrix"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$get_inverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)                
        }
        
        data <- x$get()
        inverse <- solve(data, ...)
        x$set_inverse(inverse)
        inverse
}
