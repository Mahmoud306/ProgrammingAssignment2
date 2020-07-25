## Here is two functions that caches the invers of a matrix

##this function creates a special matrix that can cache its inverse
makeCacheMatrix<-function(x=matrix()){
  inv <- NULL
  get <-function() x
  set <-function(y){
    x<<-y
    inv<<-NULL
  }
  getinv<-function() inv
  setinv <- function(inverse) inv<<-inverse
  list(get=get,set=set,getinv=getinv,setinv=setinv)
}


##this function computes the inverse of the special matrix returned by makeCacheMatrix above
cacheSolve<-function(x,...){
  inv<-x$getinv()
  if(!is.null(inv)){
    message("inverse is cached")
    return(inv)
  }
  m<-x$get()
  inv<-solve(m,...)
  x$setinv(inv)
  return(inv)
}
