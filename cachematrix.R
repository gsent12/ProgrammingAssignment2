
makeCacheMatirx <- function(m = matrix()){
  
  n <- NULL
  set <- function(y){
    m <<- y
    n <<- NULL
  }
  get <- function() m
  
  setinverse <- function(inverse) n <<- inverse
  getinverse <- function() n
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


cacheSolve <- function(x, ...){
  
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

#Test To Ensure It Works
a <- matrix(c(4:7),2,2)
b <- makeCacheMatirx(a)
cacheSolve(b)
