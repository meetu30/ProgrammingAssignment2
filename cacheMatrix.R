#Function to create a matrix and caching its inverse

makeCacheMatrix <- function(x = matrix()) {
#initiating matrix m
m <- NULL

   set <- function(y) {
           x <<- y
           m <<- NULL
   }
   get <- function() x             #this will get matrix
   setinverse <- function(inverse) m <<- inverse         #this will inverse the matrix   
   getinverse <- function() m
   list(set = set, get = get,
         setinverse = setinverse,
        getinverse = getinverse)
}

#function to return cached matrix, or if its not cached, then inversing the matrix
cacheSolve <- function(x, ...) {
   m <- x$getinverse()
   if(!is.null(m)) {
           message("getting cached matrix")   #if matrix is already cached, then it is returned
           return(m)
   }
   data <- x$get()   #if matrix is not cached, then its inversed and returned
   m <- solve(data, ...)
   x$setinverse(m)
   m
}
