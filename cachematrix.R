##there are two functions makeCacheMatrix,cachesolve
##makeCacheMatrix consistes of set,get,setInverse,getInverse functions
makeCacheMatrix<-function(x=matrix())
  {
    inv<-NULL #initilizing inverse as NULL
    set<-function(y){
    x<<-y
    inv<<-NULL #initilizing inverse as NULL
  }
  get<-function(){x} #function to get matrix x

  setInverse<-function(inverse)
  {
    inv<<-inverse
  }
      getInverse<-function(){inv}
     list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
    
}
cachesolve<-function(x,..) ##function to get cache data
{
    inv<-x$getInverse()
    if(!is.null(inv)) ##checking inverse is NULL or not
      {
    message("getting cached data")
    return(inv) ## returning inverse value
      }
    data <- x$get() 
    inv <- solve(data, ...)#calculates inverse
    x$setInverse(inv)
    inv  ##return a matrix which is inverse of x
  }
