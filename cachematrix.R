## Programming in R, Assignment #2 - July 22, 2014
## These functions can be used to cache the inverse of a square matrix X.
## Note that the functions assume that any matrix supplied is square and invertible. 

##********************************************************************
## makeCacheMatrix takes advantage of the <<- operator and lexical scoping rules
##   to enable 'caching' of an inverted matrix. It returns a list with four elements,
##   containing special functions to:
##      1. Set the value of the supplied matrix
##      2. Get the value of the supplied matrix
##      3. Set the value of the inverted matrix (calcualed in cacheSolve)
##      4. Get the value of the invertex matrix 

makeCacheMatrix <- function(x = matrix()) {
  ##establish empty square matrix of same dimensions as x to receive/hold inverse
  m <- matrix(,nrow(x),nrow(x))
  set <- function(y=matrix()) {
    ## use special operator to force assignment in parent frame
    x <<- y
    m <<- matrix(,nrow(y),nrow(y))
  }
  get <- function() x
  ## create a function to pass an inverted matrix and cache it in m for later use
  setinverse <- function (inverse) m <<- inverse
  getinverse <- function () m
  ## Create special list of functions just created for use in cacheSolve
  list (set = set, get = get, setinverse=setinverse, getinverse=getinverse) 
}


## This function will calculate the inverse of the special matrix created by makeCacheMatrix.
## Because matrix inversion is computationally intensive, it will first check to see if it has
## already been created. If so, it retrieves the cached version already created and skips the 
## calculation. If not, it will calculate the inverse of the matrix and set the value via the
## setinverse function created in makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
        ## Start by getting the current inverse value in special cache matrix 
        ## created by makeCacheMatrix
        m <- x$getinverse()
        ## if this matrix is not empty, it was already calculated
        if (!is.na(m[1,1])) {
            message ("Getting cached inverse")
            ## get cached value and return immed, skipping further functions
            return (m)
        }
        ## inverse has not yet been calculated, so get value of source matrix
        data <-x$get()
        ## calculate inverse using solve function
        m <- solve(data)
        ## record inverse in special cache matrix
        x$setinverse(m)
        # return inverse matrix
        m    
}
