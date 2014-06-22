################################################################################
## cachematrix.R
## Determines the inverse of a matrix and caches the solution in an object
## containing the matrix and its inverse along with get and set functions.
################################################################################

#' makeCacheMatrix(x)
#' Arguments: x - matrix
#' Returns: object (list) containing:
#'          get() - Returns object matrix
#'          set(x) - Takes a matrix as argument and sets it to be object matrix,
#'                   and clears cached inverse if any.
#'          getinverse() - Returns cached inverse if calculated
#'          setinverse(x) - Takes matrix as argument and sets it to be object
#'                          inverse          
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                     #Inverse not calculated yet
  set <- function(y) {
    x <<- y                       #Set object matrix to matrix provided
    inv <<- NULL                  #Reset inverse, matrix changed
  }
  get <- function() x             #Return object matrix
  setinverse <- function(inverse) inv <<- inverse     
  getinverse <- function() inv    #Return object inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


#' cacheSolve(x, ..)
#' Arguments: x - makeCacheMatrix object
#'            ... - solve{base} arguments
#' Returns: Matrix that is the inverse of the matrix contained in x.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()             #get inverse from object
  if(!is.null(inv)) {               #If inverse cached, return it
    message("getting cached data")
    return(inv)
  }                                 #Else...
  matrix <- x$get()                 # Get matrix from object...
  inv <- solve(matrix, ...)         # Solve inverse...
  x$setinverse(inv)                 # Cache the solution...
  inv                               # Return the solution
}
