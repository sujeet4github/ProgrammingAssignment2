## Matrix inversion is usually a costly computation
## the two methods here cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse
##
## Returns an vector of following functions
## 1. setMatrix - set the matrix value
## 2. getMatrix - get the matrix value
## 3. setInverseMatrix - set the inverse value
## 4. getInverseMatrix - get the inverse value
##
makeCacheMatrix <- function(inputMatrix = matrix()) {

    if ( is.null(inputMatrix) || ! is.matrix(inputMatrix) ) {

        stop("makeCacheMatrix must be supplied a valid matrix")
        
    }
    
    inverseMatrix <- NULL
    
    setMatrix <- function (inp) {
        
        inputMatrix <<- inp
        
        inverseMatrix <<- NULL
    }
    
    getMatrix <- function() inputMatrix
    
    setInverseMatrix <- function (inverse) inverseMatrix <<- inverse
    
    getInverseMatrix <- function() inverseMatrix
    
    list(setMatrix = setMatrix,
         getMatrix = getMatrix,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)

}


## Return a matrix that is the inverse of 'x'
##
## If the inverse has already been calculated (and the matrix
## has not changed),
## Then the cachesolve should retrieve the inverse from the cache.
## Else it uses the computeInverse to compute the inverse and
## cache the results
##
cacheSolve <- function(x) {
    
    inverse <- x$getInverseMatrix()
    
    if (is.null(inverse)) {
        
        matrix <- x$getMatrix()
        
        inverse <- computeInverse(matrix)
        
        x$setInverseMatrix(inverse)
    
    } else {
        
        message("getting cached data")
    }
    
    inverse
}

## This function computes the inverse of the special "matrix"
## 
computeInverse <- function (matrix) {

    ## assume that the matrix supplied is always invertible.
    solve(matrix)

}
