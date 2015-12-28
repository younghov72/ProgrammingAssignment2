
## this function will create a matrix object that can cache its inverse
## object is a list containing a function to 
## set the value of the matrix
## get the value of the matrix
##set the value of the inverse
##get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

      m <- NULL # initiates 
      set <- function(y) { ## sets values to cache
            x <<- y
            m <<- NULL
      }
      get <- function() x ## calls function 
      setinv = function(inverse) m <<- inverse ## set inverse function
      getinv = function() m ## get inverse function
      list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function
## checks if the inverse is stored in cache if it is it returns
## if not it calculates the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinv() ## calls get inverse function
      if(!is.null(m)) { ## checks if its stored in cache it returns the value
            message("getting cached data")
            return(m)
      }
      ## if not calculates the inverse of the matrix
      data <- x$get() 
      m <- inv(data, ...) ## calls function
      x$setinv(m)
      return(m) #returns inverse
}
