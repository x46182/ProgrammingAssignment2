#Write an R function to "cache" time-consuming operations
#If items aren't changing, then cache the value so it can be looked up vs. computed
#Take advantage of the scoping rules



#write a pair of functions that cache the inverse of a matrix
#The two functions below were modified from the given code
#The function names were changed to appropriately reflect the 
#inverse.  The solve() function was used instead of mean()
#to calculate a matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(invert) m <<- invert
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached inverse")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        
        ## Return a matrix that is the inverse of 'x'
}
