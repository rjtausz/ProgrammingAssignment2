#This function creates the matrix and the functions get and set matrix

makeCacheMatrix <-function(x=matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL}
  get<-function()x
  setmatrix<-function(solve) m<<-solve
  getmatrix<-function()m
  list(set=set,get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)}

#This function does takes the inverse of the matrix from cache if it is there

cachesolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  mat.data <- x$get()
  m <- solve(mat.data, ...)
  x$setmatrix(m)
  return(m)
}