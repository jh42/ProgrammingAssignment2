## "R Programming" Assignment 2: Creating and caching the inverse of a matrix

## The "makeCacheMatrix" function creates a special matrix, which is really a 
## list containing a function to
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the mean
## 4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  # initialize variable for inverse to NULL
  i <- NULL
  
  #anonymous function for setting x (and re-initializing i)
  set <- function(y) {
    x <<- y
    i <<- NULL
  }

  #anonymous function for getting matrix x
  get <- function() x
  
  #anonymous function for computing the inverse i of x
  setinverse <- function(inverse) i <<- inverse
  
  #anonymous function for returning the inverse i of x
  getinverse <- function() i

  #return list of all four elements
  list(
    set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse
    )
}


## The "cacheSolve" function computes and caches the inverse of a matrix x

cacheSolve <- function(x, ...) {
  #get inverse from "special" matrix x (NULL if not computed yet)
  i <- x$getinverse()

  #if i has been computed already (!=NULL), return i and exit function
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  #otherwise, continue...
  #get matrix x, inverse and store in variable i
  data <- x$get()
  i <- solve(data, ...)
  
  #set inverse of x to (local) variable i
  x$setinverse(i)
  
  #return i and exit
  i
}
