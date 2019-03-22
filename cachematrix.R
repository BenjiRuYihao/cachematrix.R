## Create a function called 'makeCacheMatrix', which create a list of functions with a numeric and matrix object 
## in its own environment



## initiate 'x' in argument
## set variable 'inverse' to NULL
## 'set' function does: with input 'aa', set the variable x in its parent environment to aa, 
##                                 , set the variable inverse in its parent environment to NULL
## 'get' function does: return the variable x in its parent environment
## 'setinverse' function does: with input 'inver_', set the variable inverse in its parent environment to 'inver_'
## 'getinverse' function does: return the variable 'inverse' in its parent environment

makeCacheMatrix <- function(x = matrix()) {
  
  inverse<-NULL
  set<-function(aa){
    x<<-aa
    inverse<<-NULL
  }
  get<-function() x
  setinverse<-function(inver_) inverse<-inver_
  getinverse<-function() inverse
  list(set=set,
       get=get,
       setinverse=setinverse,
       getinverse=getinverse
    
  )

}


## cacheSolve takes variable as the return value of a 'makeCacheMatrix' function
## return the inverse of the 'x' in the environment of input 'x'  ****note that the two 'x' are not of the same class
## initiate 'inv' as the return value of the 'getinverse' function given the environment of 'x'

## 'inv' not being NULL implies that the 'inverse' in the environment of 'x' is not NULL, or in another word, exists.
## so cacheSolve simply returns that 'inverse' value in the environment of 'x' 

## set 'data' as the return value of the 'get' function given the environment of 'x', which is the 'x' in 'x' environment
## set 'inverse' in 'x' as  the value of the inverse of 'x' in 'x'
## return it 
cacheSolve <- function(x, ...) {
        inv<-x$getinverse()
        if(!is.null(inv)){
          message('getting cached inverse')
          return(inv)
        }
        data<-x$get()
        x$setinverse(solve(x$get()))
        solve(x$get())
}
