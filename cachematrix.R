## The string is written thanks to the explanations of Gregory D. Horne and
## Bill Hilton in the Discussion Forums and Dr. Peng.
## One may run the script in R Studio.
## First one should remeber that only square and non-singular matrices could be inverted.
## If he/she ignores these conditions the cacheSolve function reminds the user that:
## Reminder No.1: The matrix must be square!
## Reminder No.2: The matrix must be nonsingular!
## In the beginning we load teh matrixcalc package in order to use the in.non.singular.matrix() function
## it is used in the cacheSolve function in order to check wheather a particular matrix is singular

library(matrixcalc)

## The mackeCacheMatrix is a storage of inverted matrices

makeCacheMatrix <- function(x=matrix()) {
       invertedmtrx <- NULL
       
       ## set takes, saves the inverted matrix, and resets the it to NULL, 
       ## that happens when a new matrix is delived in the storage (generated with makeCacheMatrix)
       set <- function(y) {               
              x <<- y
              invertedmtrx <<- NULL
       }
       
       ## get returns the value of the original matrix
       get <- function() x
       
       ## setinverted is called by cacheSolve() during the first cacheSolve access and 
       ## it will store the value using superassignment
       setinverted <- function(solve) invertedmtrx <<- solve
       
       ## getinverted will return the cached value to cachSolve on subsequent accesses
       getinverted <- function() invertedmtrx
       
       ## This list is returned with the newly created matrix. It lists all functions that are part of
       ## the object.  If a function is not on the list then it cannot be accessed externally, i.e. by the 
       ## cacheSolve function.
       ## In  other words the list contains all functions responsible for the management of the storage with inverted matrices
       list(set = set, get = get,
            setinverted = setinverted,
            getinverted = getinverted)
}

## When one comes to the storage with a raw matrix and wishes an inverted one
## the cacheSolve is called. it will see if the inverted matrix has been stored.  
## If not it will calculate the inverse matrix, store it and then return it, but cacheSolve should be sure 
## that the raw matrix is square and non-singular. If not it wont the raw matrix has no place in the storage. :)
## If the inverse matrix for this raw matrix has been calculated and stored earlier it will fetch the inverse and return it.

cacheSolve <- function(x, ...) {
  
  ## accesses the matrix'x' and gets the inverse of 'x'
  ## and if the inverse of the matrix is already in the store cachSolve get it.
  invertedmtrx <- x$getinverted()
       if(!is.null(invertedmtrx)) {
              message("Getting cached data.")
              return(invertedmtrx)
       } 
  ## else the matrix 'x' is new one, then the inverse of the new 'x' will be calculated
       data <- x$get()
  
  ## First the new matrix should be cheked, Is it square and singular? If no, "No party!". If yes, cacheSolve
  ## will calculate the inverse of the new matrix and will store it.
       if(nrow(data)!=ncol(data)) {
              stop("Reminder No.1: The matrix must be square!")
       }else if(is.non.singular.matrix(data)==FALSE){
                     stop("Reminder No.2: The matrix must be nonsingular!")
       } else
       
         invertedmtrx <- solve(data, ...)
       x$setinverted(invertedmtrx)
      invertedmtrx
}

## An arbitrary matrix
y = makeCacheMatrix(matrix(1:4,2,2));
cacheSolve(y);
cacheSolve(y);

## A variance covariance matrix of 2 stocks
z = makeCacheMatrix(matrix(c(0.001842224,0.001910612,0.001910612,0.0034429),2,2));
cacheSolve(z);
cacheSolve(z);

## Singular matrix
p = makeCacheMatrix(matrix(1:16,4,4));
cacheSolve(p);
