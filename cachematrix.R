## The function inside the makeCacheMatrix do not calculate the inverse. They simply assign the values (the matrix that we want its inverse calculate, and the calculated inverse).
makeCacheMatrix <- function(x = matrix()) {
	   inv <- NULL
	   set <- function(y)
	   	   x <<- y
		   inv <<- NULL
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## The functions inside the cachSolve, check if the inverse has been calculated and stored in the cache, and if not, it claculates the inverse and stores it to cache. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	   #Checking if the inverse has been calculated
	   # If yes, then return it from cache
        inv <- x$getinv()
        if(!is.null(inv)) {
                return(inv)
        }
	   #If no, then calculate the inverse of the matrix
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
