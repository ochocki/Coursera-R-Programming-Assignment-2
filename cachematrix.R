## This function will return the inverse of a matrix. It will first check to see if it has previously
## performed the operation. If so, it will return the cached value to save time, if not, it will 
## perform the calculation and return the result.

##The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to:
## set the value of the matrix
## get the value ofgit the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(y) {
                x <<- y
                inver <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inver <<- inverse
        getinverse <- function() inver
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)         
}

## cacheSolve will return the inverse of a matrix. First, it will check to see if this has already
## been calculated. If yes, then it will just display the cached value. If not, it will perform
## the computation.

cacheSolve <- function(x, ...) {
        ## Check to see if 'inver' has been previously calculated and cached
        inver <- x$getinverse()
        if(!is.null(inver)) {
                message("Already done this. Getting cached data, please wait.")
                return(inver)
        } 
        ## If it wasn't previously calculated, it is done here using the solve() function
        data <- x$get()
        inver <- solve(data)
        x$setinverse(inver)
        inver
}
