## makeCacheMatrix 

# creates a global variable which is list containing the following three functions

# get()
# setmat()
# getmat()

# input: a matrix which is assume to be invertable

# example usage:

# > x <- matrix(rnorm(9, 5, 1), 3, 3)
# > x
# [,1]     [,2]     [,3]
# [1,] 4.418861 5.342808 5.248087
# [2,] 4.571040 6.190559 4.754583
# [3,] 5.013326 7.368715 3.697024

# > a <- makeCacheMatrix(x)

# > cacheSolve(a)
# [,1]      [,2]      [,3]
# [1,]  4.4571695 -6.941252  2.599698
# [2,] -2.5451255  3.659263 -1.093104
# [3,] -0.9713053  2.119188 -1.076098

makeCacheMatrix <- function(x = matrix()) {
    
    mat <- NULL
    get <- function() x
    setmat <- function(solve) mat <<- solve
    getmat <- function() mat
    list(get = get,
         setmat = setmat,
         getmat = getmat)
}

## CacheSolve

# returns a cached inverted matrix if it exists, otherwise inverts the user supplied matrix and stores in the # global environment

# input: the list object returned by makeCachedMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mat <- x$getmat()
    if(!is.null(mat)) {
        message("getting cached data")
        return(mat)
    }
    data <- x$get()
    mat <- solve(data, ...)
    x$setmat(mat)
    mat
}
