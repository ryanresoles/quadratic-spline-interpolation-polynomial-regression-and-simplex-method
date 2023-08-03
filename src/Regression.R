PolynomialRegression <- function(number, items){

  x = items[[1]]
  y = items[[2]]
  rhs <- c()  # column to be attatched
  unknowns <- c()
  
  
  # uses result previously created AugCoeffMatrix function

  augcoeffmatrix = matrix(nrow = number+1, ncol = number+1)
  
  print(x)
  print(y)
  
  # creates new augcoeffmatrix
  # plugs in values in the matrix
  for(i in 0:number){
    power = i
    for(j in 1:ncol(augcoeffmatrix)){
      total = 0
      for(k in 1:length(x)){
        total = total + (x[k]**power)
      }
      power = power+1
      augcoeffmatrix[i+1,j] = total
      
    }
  }
  
  # solves for RHS
  for(i in 0:number){
    total = 0
    for(j in 1:length(x)){
      total = total + ((x[j]**i) * y[j])
    }
    rhs[i+1] <- total
  }
  
  augcoeffmatrix = cbind(augcoeffmatrix, rhs)
  old = augcoeffmatrix
  
  # gets the row with max value of the matrix then swaps it with the index of the current iteration number
  
  for(i in 1:nrow(augcoeffmatrix)){
    if(i != nrow(augcoeffmatrix)){
      pivot_row = augcoeffmatrix[,i]
      if(i != 1) pivot_row = pivot_row[-1:(-1*(i-1))]
      maxval = max(abs(augcoeffmatrix[i:nrow(augcoeffmatrix),i]))
      
      #solution unavailable
      if(maxval==0) return(NA)
      
      # gets pivot row
      for(j in i:nrow(augcoeffmatrix)){
        if(maxval == abs(augcoeffmatrix[j,i])){
          pivrow <- augcoeffmatrix[j,]
          break
        }
      }
      # swapping of rows
      augcoeffmatrix[j,] <- augcoeffmatrix[i,] 
      augcoeffmatrix[i,] <- pivrow
    }
    # normalizes current row
    augcoeffmatrix[i,] = augcoeffmatrix[i,]/augcoeffmatrix[i,i]
    
    for(k in 1:nrow(augcoeffmatrix)){
      if(i==k){
        next
      } 
      normalized_row = augcoeffmatrix[k,i]*augcoeffmatrix[i,]
      augcoeffmatrix[k,] <- augcoeffmatrix[k,] - normalized_row
    }
  }
  
  polynomial_string = "function(x)"
  
  for(i in 0:number){
    unknowns[i+1] <- augcoeffmatrix[i+1,number+2]
    if(i==0) polynomial_string = paste(polynomial_string, unknowns[i+1], "+")
    
    
    if(i != 0 && i != number) polynomial_string = paste(polynomial_string, unknowns[i+1],"*x^",i, "+")
    if(i==number) polynomial_string = paste(polynomial_string, unknowns[i+1],"*x^",i)
    }
  
  polynomial_function = c(eval(parse(text=polynomial_string)))
  
  return(list(unknowns = unknowns, polynomial_string = polynomial_string, polynomial_function = polynomial_function, augcoeffmatrix = augcoeffmatrix, old=old))
  
}

