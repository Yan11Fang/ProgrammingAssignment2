## This function creates a special "matrix" object that can cache its inverse.
## 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
                Inver <- NULL
                set <- function(y) {
                        x <<- y
                        Inver <<- NULL
                }
                get <- function() x
                setinver <- function(solve) Inver <<- solve
                getinver <- function() Inver
                list(set = set, get = get,
                     setinver = setinver,
                     getinver = getinver)

}


##This function computes the inverse of the special
##"matrix" returned by `makeCacheMatrix` above. If the inverse has
##already been calculated (and the matrix has not changed), then
##`cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        Inver <- x$getinver()
        if(!is.null(Inver)) {
                message("getting cached data")
        return(Inver)
        }
        data <- x$get()
        Inver <- solve(data, ...)
        x$setinver(Inver)
        Inver
}
