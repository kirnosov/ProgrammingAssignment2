## R Programming Assignment 2

## Matrix inverses are widely used in science, although
## finding the inverse is a computationally expensive 
## operation (http://en.wikipedia.org/wiki/Invertible_matrix).
## In order to avoid multiple evaluations of the same matrix
## inverse, one can simply cache the result and use it
## if the initial matrix has remained unchanged.
## The goal of this project is to create a function (cacheSolve),
## which would be computing the inverse if it was not yet
## computed and caching it or retrieveing the inverse matrix
## from the cache.

## In order to achieve this goal, a helper function needed:
##                       makeCacheMatrix
## will create a special "matrix" which is really a list 
## containing a function to:
## set the value of the matrix
## get the value of the matrix
## set the value of inverse
## get the value of inverse
## (just like in the example problem)

makeCacheMatrix <- function(x = matrix()) {

        # set to NULL when makeMatrix is called
        inv <- NULL
        
        # set function: sets the matrix
        set <- function(y) {
                x <<- y 
                inv <<- NULL # check out the assignment operator here:
                # http://stat.ethz.ch/R-manual/R-patched/library/base/html/assignOps.html
        }
        
        # get function: gets the matrix
        get <- function() x
        
        # setinverse function: sets the inverse 'by hand'
        setinverse <- function(inverse) inv <<- inverse
        
        # getinverse function: gets the inverse
        getinverse <- function() inv
        
        # Return these as a list
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)	
}

##        cacheSolve
## function follows the example format as well.
## It will return an inverse of the input matrix

cacheSolve <- function(x, ...) {
        
        # Was the inverse computed already?
        inv <- x$getinverse()  
        
        # If the inverse matrix is not all-zeroes matrix,
        # the answer is yes!
        if(!is.null(inv)) {
                # In that case we just return cached inverse		
                message("Getting cached matrix")
                return(inv)
        }
        
        # If the answer is 'no',       
        data <- x$get()         # we should retrieve the initial matrix,
        inv <- solve(data, ...) # invert it
        x$setinverse(inv)       # and cache the outcome.
        # Note that this will change the list created in makeMatrix,
        # so the next time x$getinverse() will not be a null matrix.
        
        inv # Return the inverse
}

##                Let us TEST the code.

## In order to avoid possible 'good numbers', we will generate an initial
## matrix as a matrix of random elements:

## > set.seed(1)
## > n=3
## > x<-matrix(runif(n*n), ncol=n) 
## > x
##           [,1]      [,2]      [,3]
## [1,] 0.2655087 0.9082078 0.9446753
## [2,] 0.3721239 0.2016819 0.6607978
## [3,] 0.5728534 0.8983897 0.6291140

## (In other cases the elements might be different)

## Next, let us prepare a list of functions M using makeMatrix()
## > M <- makeCacheMatrix(x)

## And calculate the inverse of the initial matrix:
## > invM <- cacheSolve(M)
## > invM
##             [,1]      [,2]      [,3]
## [1,]  -2.1820373  1.296397  1.914852
## [2,]   0.6751799 -1.748934  0.823167
## [3,]   1.0227284  1.317057 -1.329575

## Is this the correct answer?
## > invM == solve(x)
##      [,1] [,2] [,3]
## [1,] TRUE TRUE TRUE
## [2,] TRUE TRUE TRUE
## [3,] TRUE TRUE TRUE

## Now let us compute the inverse again:
## > invM <- cacheSolve(M)
## Getting cached matrix
## > invM == solve(x)
##      [,1] [,2] [,3]
## [1,] TRUE TRUE TRUE
## [2,] TRUE TRUE TRUE
## [3,] TRUE TRUE TRUE

## As we can see, the programm has simply fetched
## the cached matrix.
##    TESTING is SUCCESSFULL
