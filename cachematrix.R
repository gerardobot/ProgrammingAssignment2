## A couple of functions that allow to cache and compute the inverse of a given matrix,
## as long as said matrix is squared and non singular

# This first function caches a matrix taking advantage of the <<- operator and the
# scoping rules of R. It also stores the inverted function and caches it, in order to
# avoid redundant computations from further calls of the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
    inv  <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverted) inv <<- inverted
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


# The second function uses solve() to compute the inverse of the cached matrix. It
# first checks wether an inverse for said matrix has already been computed, in order
# to avoid reundancies. If it is not the case, it computes and stores in the cache
# the invers, which is given as a result

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
