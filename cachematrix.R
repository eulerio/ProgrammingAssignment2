## Put comments here that give an overall description of what your
## functions do

## This function returns a list of 4 functions,
## set sets the matrix value, get returns the matrix, setinverse sets the inverse matrix and getinverse returns it

makeCacheMatrix <- function(x = matrix()) {

        invr <- NULL
        set <- function(y) {
                x <<- y
                invr <<- NULL
        }
        get <- function() x
        setinverse <- function(givinv) invr <<- givinv
        getinverse <- function() invr
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


##this function gets the output of above function as input
##it returns the inverse from cache is already calcualted

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invr <- x$getinverse()
        if(!is.null(invr)) {
                message("getting cached data")
                return(invr)
        }
        data <- x$get()
        invr <- solve(data)
        x$setinverse(invr)
        invr
}
