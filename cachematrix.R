## This function reduces the computational "cost" of inverting a matrix and computing it repeatedly 
## by instead caching the inverse of the matrix computation. 
## The two functions below create a special object that stores the matrix caches the computation of it's inverse. 


makeCacheMatrix <- function(x = matrix()) {
        ## here "x" is a square invertible matrix
        ## returns a list containing the inverse of the matrix
         inv <- NULL
         set <- function(y) {
                # use '<<-' to assign a value from a different environment
                x <<- y
                m <<- NULL
         }
         get <- function() x
         setinv <- function(inverse) inv <<- inverse
         getinv <- function() inv
         list(set = set, get = get,
              setinv = setinv,
              getinv = getinv)
}


## This function computes the inverse of the matrix created above in the makeCatcheMatrix function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## x is output of makeCacheMatrix()
        ## return: inverse of the original matrix input to makeCacheMatrix()
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat.data <- x$get()
        inv <- solve(mat.data, ...)
        x$setinv(inv)
        inv
}
