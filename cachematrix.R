# Creates a list of functions (set, get, setminv, getminv) that:
      #     - sets the value of a matrix
      #     - gets the value of the matrix
      #     - sets the value of the inverse matrix
      #     - gets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL               #initialize m as NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setminv <- function(minv) {
            m <<- minv
      }
      getminv <- function() m
      list(set = set, get = get,
           setminv = setminv, getminv = getminv)
}


#Creates a fucntion that calculates the inverse of the "matrix"
# returned by the makeCacheMatrix function above. Retrieves
# cached value if the inverse matrix has already been calculated.

cacheSolve <- function(x, ...) {
      m <- x$getminv()        #retrieve cached matrix (or NULL)
      if(!is.null(m)) {       #check if inverse matrix exists
            message("getting cached data")
            return(m)                           #return cached value
      }
      data <-x$get()
      m <- solve(data,diag(dim(data)[1]),...) #calculate inverse matrix
                               #assumes matrix is square and invertible!
      x$setminv(m)
      m
}