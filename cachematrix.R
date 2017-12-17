  ##MakeCacheMatrix and cacheSolve functions
  ##Programming Assignment 2

  ## This function creates a list of functions(set, get, setInverse, and getInverse)
  makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL     
    ##assigns the value to the matrix
    set <- function(y) {  
      x <<- y
      inv <<- NULL
    }
    ## read and returns the value of the matrix, x
    get <- function() 
       x 
    ## assigns the inverse of the matrix to variable, inv          
    setInverse <- function(inverse) 
      inv <<- inverse
          
    ## read and returns the inverse of the matrix, inv
    getInverse <- function()   
      inv
    list(set=set, get=get, setInverse=setInverse, getInverse = getInverse)
  }
  
  ## Checks first if the the inverse of the supplied matrix is already computed,
  ## returning the cached inverse matrix. Else, it will compute the inverse
  ## of the matrix anew and returns it
  cacheSolve <- function(x, ...) {
    inv <- x$getInverse() 
    if(!is.null(inv)) {
      return(inv)
    }
    else {
      new <- x$get()
      inv <- solve(new, ...)
      x$setInverse(inv)
      inv
    }
  }

## test 
A= matrix(c(1,0,0,0,1,0,0,0,1),nrow=3, ncol=3 )
print(A)
B <- makeCacheMatrix(A)
print(cacheSolve(B))
