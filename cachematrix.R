## This program intends to cache the invesre of a matrix, which is a time consuming computation. 
## It is stored in the cache and is accessed everytime it is needed, instead of computing it again.
## The first function is used to create the matrix. And the second computes its inverse.

## 4 functions, to assign and get the matrix, and its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
  i <- NULL
  set <- function(y)
  {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Checks if inverse is already calculated. If yes, obtains from cache. If no, uses solve() function.

cacheSolve <- function(x, ...)
{
  i <- x$getinverse()
  if(!is.null(i))
  {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i      
}
