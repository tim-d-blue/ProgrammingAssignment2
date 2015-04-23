## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## 
## This module provides a matrix wrapper to cache the inverse and a function
## that utilizes the matrix cache to calculate a matrix inversion and store
## the result in the cache.

## Matrix wrapper. Given a matrix (default to empty matrix) return a list
## of functions to get/set the matrix and its inverse.
makeCacheMatrix <- function(m = matrix()) {

    m_inv <- NULL

    # define the function to set the matrix, ensure the inverse is now NULL
    set <- function(matrix) {
        m <<- matrix
        m_inv <<- NULL
    }

    # define the function to get the matrix
    get <- function() {
        m
    }

    #define the function to set the inverse matrix
    set_inverse <- function(inverse_matrix) {
        m_inv <<- inverse_matrix
    }

    # define the function to get the inverse matrix
    get_inverse <- function() {
        m_inv
    }

    # return the list of functions
    list(set = set, 
         get = get, 
         set_inverse = set_inverse, 
         get_inverse = get_inverse)
}


## Return a matrix that is the inverse of the cacheMatrix and store the
## result in the cacheMatrix, just return the inverse matrix already stored
## in the cacheMatrix if it is there.
cacheSolve <- function(cache_matrix, ...) {

    # if cache_matrix already has an inverse matrix cached then return that
    m_inv <- cache_matrix$get_inverse()
    if (!is.null(m_inv)) {
        return(m_inv)
    }

    # cache_matrix does not have its inverse so calculate it and cache it
    m <- cache_matrix$get()
    m_inv <- solve(m, ...)
    cache_matrix$set_inverse(m_inv)

    m_inv
}
