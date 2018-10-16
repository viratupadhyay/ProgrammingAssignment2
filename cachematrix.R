## x: A NxN "nonsingular" matrix
## First function "makeCacheMatrix" returns a list containing functions to:
##      1. set the matrix
##      2. get the matrix
##      3. set the inverse
##      4. get the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv = NULL
    set = function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    return(list(set=set, get=get, setinv=setinv, getinv=getinv))
}

## The list output from ""makeCacheMatrix is used as input to cacheSolve()
## in order to compute inverse of matrix 'x'

## It checks if the matrix inverse has been already computed.
## If so, "cacheSolve" will return the cached data and skips computation.
## Else, it will compute the inverse and keep the return invisible.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    
    # If the inverse has already been calculated
    # get it from the cache and skips the computation.
    if (!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    
    # Else, computes the inverse
    else{
        message("computing the inverse")
        mat.data <- x$get()
        inv <- solve(mat.data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinv(inv)
    }
    # Invisible return to avoid printing cache value on console
    return(invisible(inv))
}


#*************** OUTPUT ***************#
#   > x = rbind(c(3:1), c(2,1,3),c(1:3))
#   > x
#   [,1] [,2] [,3]
#   [1,]    3    2    1
#   [2,]    2    1    3
#   [3,]    1    2    3
#   > m = makeCacheMatrix(x)
#   > cacheSolve(m)
#   computing the inverse
#   > cacheSolve(m)
#   getting cached data
#   [,1]       [,2]        [,3]
#   [1,]  0.25  0.3333333 -0.41666667
#   [2,]  0.25 -0.6666667  0.58333333
#   [3,] -0.25  0.3333333  0.08333333


