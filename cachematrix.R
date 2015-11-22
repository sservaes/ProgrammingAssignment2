'''
The function makeCacheMatrix generates a special matrix object
consisting out of a list of functions that allowing caching of the inverse
'''

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setsolve <- function(inverse) inv <<- inverse
        getsolve <- function() inv
        
        list(set = set,
             get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


'''
The function cacheSolve returns the inverse of the matrix
that served as an input for makeCacheMatrix. It first checked
if the inverse is cached.
If yes - it returns the cached inverse
If no - it calculates, caches and then returns the inverse
'''

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getsolve()
        
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        data <- x$get()
        inv <- solve(data, ...)
        x$setsolve(inv)
        
        inv
}