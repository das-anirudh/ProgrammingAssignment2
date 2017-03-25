## This program intends to cache the invesre of a matrix, which is a time consuming computation. 
## It is stored in the cache and is accessed everytime it is needed, instead of computing it again.
## The first function is used to create the matrix. And the second computes its inverse.

## This function has four 4 nested functions, to assign and get the matrix, and its inverse.

makeCacheMatrix <- function(source_matrix = matrix()) 
{
      matrix_inverse <- NULL
      set <- function(y)
      {
          source_matrix <<- y
          matrix_inverse <<- NULL
      }
      get <- function() source_matrix
      setinverse <- function(inverse) matrix_inverse <<- inverse
      getinverse <- function() matrix_inverse
  
      ##List containing all the nested functions
      return(list(set = set , get = get, setinverse = setinverse , getinverse = getinverse))
}


## Checks if inverse is already calculated. If yes, obtains from cache. If no, uses solve() function.

cacheSolve <- function(x, ...)
{
      inverse <- x$getinverse()
      if(!is.null(inverse))
      {
          message("Obtaining inverse from Cache.")
          return(inverse)
      }
      data <- x$get()
      inverse <- solve(data, ...)
      x$setinverse(inverse)
      return(inverse)      
}
