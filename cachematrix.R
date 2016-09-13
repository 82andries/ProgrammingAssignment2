## These function takes a matrix and cache the inverse and checks if it is cached
## This function creates an object(matrix) that can cache its inverse using 
##the solve() function.


makeCacheMatrix <- function(x = matrix()){
        minv <- NULL
        set <- function(y){
                x <<- y
                minv <<- NULL
        }
        get <- function() x
        setmatrixinv <- function(solve) minv <<- solve
        getmatrixinv <- function() minv
        list(get = get, set = set, setmatrixinv = setmatrixinv,
             getmatrixinv = getmatrixinv)
}

## This function calculates the inverse of the matrix created by makeCacheMatrix.
## If there is already a value in the cache memmory and the matrix has not
## changed then it returns the cached data.

cacheSolve <- function(x, ...){
        minv <- x$getmatrixinv()
        if(!is.null(minv)){
                message("Getting Cache Data!")
                return(minv)
        }
        data <- x$get()
        minv <- solve(data, ...)
        x$setmatrixinv(minv)
        minv
}