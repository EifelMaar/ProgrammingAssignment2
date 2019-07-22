## Put comments here that give an overall description of what your
## functions do

## creates a cached matrix with the ability 
## to cache once calculated inverse matrix for further use
makeCacheMatrix <- function(x = matrix()) 
{

  inv <- NULL     ## local
  set <- function(y) 
  {
    x <<- y       ## override
    inv <<- NULL  ## reset
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse  ## overwrite
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## returns the inverse matrix, 
## if already calculated before it returns the cache inverse matrix

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  m_inv <- x$getinverse()
  if(!is.null(m_inv)) ## if already set, return the got value
  {
    # message("the cached data")
    return(m_inv)
  }
  ## calculate the inverse and cache it
  data <- x$get()
  m_inv <- solve(data)
  x$setinverse(m_inv)    ## cache it
  # message("the calculated data")
  m_inv
}
