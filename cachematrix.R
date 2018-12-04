## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#This is the function that returns a list of 4 helper functions 
#      set : set the value of the matrix
#      get : get the value of the matrix
#      setInverse : set the value of the inverse of the matrix
#      getInverse : get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
       inverseMatrix <- NULL
       set <- function(y)
       {
              x <<- y
              inverseMatrix <<- NULL
       }
       
       get <- function() x
       
       setInverse <- function(inverse)
       {
              inverseMatrix <<- inverse
       }
       
       getInverse <- function()
       {
              inverseMatrix
       }
       
       list(set = set, get = get, setInverse = setInverse , getInverse = getInverse)
}


## This function is used to retrieve the value of the inverse of the matrix 
## If the inverse has already been calculated previously, the inverse is returned
## Otherwise, the inverse is calculated and stored so that it can be retreived later 
## directly without having to recompute the inverse 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       inverseMatrix <- x$getInverse()
       if(!is.null(inverseMatrix))
       {
              message("Gettting cached data")
              return(inverseMatrix)
       }
       
       matrix <- x$get()
       inverseMatrix <- solve(matrix)
       x$setInverse(inverseMatrix)
       inverseMatrix
}
