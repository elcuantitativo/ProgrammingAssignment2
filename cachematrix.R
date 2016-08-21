# Assignment: Caching the Inverse of a Matrix
# WRITE A PAIR OF FUNCTIONS THAT CACHE THE INVERSE OF A MATRIX
# ============================================================

# Note: assume that the matrix supplied is always invertible.


# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get, setmean = setmean, getmean = getmean)
}

makeCacheMatrix <- function(x = matrix()) {
    m_inv <- NULL
    set <- function(y) {
        x <<- y
        m_inv <<- NULL
    }
    get <- function() x
    set_inv <- function(solve) m_inv <<- solve
    get_inv <- function() m_inv
    list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}


# cacheSolve: This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix above. If the inverse has already been calculated
# (and the matrix has not changed), then cacheSolve should retrieve the inverse
# from the cache.

cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}

# Testing
x <- 1:20
z <- makeVector(x)
cachemean(z)
cachemean(z)

# Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    m_inv <- x$get_inv()
    if(!is.null(m_inv)) {
        message("getting cached data")
        return(m_inv)
    }
    data <- x$get()
    m_inv <- solve(data, ...)
    x$set_inv(m_inv)
    m_inv
}

# Testing
x <- matrix(1:4, nrow=2)
z <- makeCacheMatrix(x)
cacheSolve(z)
cacheSolve(z)
