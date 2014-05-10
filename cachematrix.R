## These functions work together to cache a copy of an inverse of matrix 'x'
## Matrix inversion can be a slow function, especially if called in a loop.
## Returning a cached copy of the inverted matrix will be much faster than
## recalculating it every time.
## 

## creates a special "matrix", which is really a list containing functions used by the cacheSolve
## function to detect if the incoming matrix has been cached yet, 
##  if not calls the real solve function to invert the matrix
## else it returns the previously cached copy. 
##
## set the value of the matrix
## get the value of the matrix
## setinverse the value of the inverted matrix
## getinverse the value of the inverted matrix
##
## How to use:
##		Call makeCacheMatrix passing in the original matrix
##		This returns a 'pseudo' matrix (actually a list of functions tied to your input matrix),
##		you then call the cacheSolve function passing in the returned 'pseudo' matrix.
##		cacheSolve returns the inverted matrix of that passed to makeCacheMatrix, either
##		by calling the real solve function if not previously called, or by returning
##		the cached version if it has been previously called. 
##
##		example:
##		mymatrix <- (build an invertable matrix using R functions)
##		mypseudomatrix <- makeCacheMatrix(mymatrix)
##		myinvertedmatrix <- solveCache(mypseudomatrix)				## returns output from solve
##		myinvertedmatrix2 <- solveCache(mypseudomatrix) 			## returns cached matrix
##


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function first checks to see if there is a cached version of the inverted matrix
## if so, its returned, otherwise we call the solve function to create one.
## For this assignment, assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
