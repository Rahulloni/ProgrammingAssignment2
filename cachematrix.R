## This code will create two functions, makeCacheMatrix and cacheSolve

## This function creates a special "matrix" object that can cache its inverse.
## This object will have four functions and two matrix data objects

makeCacheMatrix <- function(m_matrix = matrix()) {
        m_inverse <- NULL    
        set <- function(b) {        
                m_matrix <<- b        
                m_inverse <<- NULL    
        }
        get <- function() m_matrix    
        setsolve <- function(solve) m_inverse <<- solve    
        getsolve <- function() m_inverse    
        list(set = set, get = get,         
             setsolve = setsolve,         
             getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(m_matrix, ...) {
        ## Return a matrix that is the inverse of 'x'
        m_inverse <- m_matrix$getsolve()    
        if(!is.null(m_inverse)) {        
                message("getting cached data")        
                return(m_inverse)    
        }    
        data1 <- m_matrix$get()
        
        # Outer if condition checks for Square Matrix    
        if(dim(data1)[1]==dim(data1)[2]){        
                #Inner if condition checks for singular matrix        
                if(det(data1)==0){            
                        print("Matrix is Singular")        
                }        
                else{            
                        #Smaller tolerance else error,"system is computationally singular"           
                        m_inverse <- solve(data1, tol=1e-20)            
                        m_matrix$setsolve(m_inverse)        
                }    
        }    
        else{        
                print("Please input Square Matrix")    
        }       
        m_inverse
}
