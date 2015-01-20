## These Functions provide a means to cache the R solve() function for a matrix. 

## solve() is an expensive function to identify the inverse of a matrix, therefore 
## the aim of this function is to increase efficiency by returning a cached version 
## of the solve() function if it has been run before in this session

## The first function creates a special matrix which is the set of functions that
## undertake the foillowing functions against the matrix:
## 	Set and get the value of the matrix
##	Set and get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
 	m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## The second function calculates the matrix inverse solution to a special matrix
## created from the above function. If the inverse has already been caluclated it 
## returns the existing value, otherwise it calculates the inverse and stores into cache

cacheSolve <- function(x, ...) {
             m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
