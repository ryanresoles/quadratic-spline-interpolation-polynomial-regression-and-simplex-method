QuadraticSpline <- function(number, items){
  
  x = items[[1]]
  y = items[[2]]
  n = length(x)-1
  newx = length(x)
  
  augcoeffmatrix = matrix(nrow = (3*n)-1, ncol = 3*n)
  # initialization of fields needed
  set = c()
  check = 0
  count = 0
  
  # gets first set of equations (interior points)
  for(i in 2:n){
    power = 2
    for(j in 0:2){
      augcoeffmatrix[((i-1)*2)-1,j+(3*check)] <- x[i]^(power-j)
      augcoeffmatrix[(i-1)*2, j+3+(3*check)] <- x[i]^(power-j)
      augcoeffmatrix[(i-1)*2, ncol(augcoeffmatrix)] <- y[i]
      augcoeffmatrix[(i-1)*2-1, ncol(augcoeffmatrix)] <- y[i]
      count = 1+(i-1)*2
    }
    check=check+1
  }
  
  # gets second set of equations (exterior points)
  power = 2
  for(i in 0:2){
    augcoeffmatrix[count,i] <- x[1]^power
    augcoeffmatrix[count+1,ncol(augcoeffmatrix)-(power+1)] <- x[length(x)]^power
    power = power-1
    augcoeffmatrix[count, ncol(augcoeffmatrix)] <- y[1]
    augcoeffmatrix[count+1, ncol(augcoeffmatrix)] <- y[length(y)]
  }
  
  count = count+2
  
  #gets final set of equations
  for(i in 2:n){
    for(j in 0:1){
      augcoeffmatrix[count, j+(2*j)+(3*(i-2))] <- 2*x[i]*((-1)^j)
      augcoeffmatrix[count, j+1+(2*j)+(3*(i-2))] <- 1*((-1)^j)
    }
    augcoeffmatrix[count, ncol(augcoeffmatrix)] <- 0
    count = count+1
  }
  
  for(i in 1:nrow(augcoeffmatrix)){
    for(j in 1:ncol(augcoeffmatrix)){
      if(is.na(augcoeffmatrix[i,j])) augcoeffmatrix[i,j] = 0
    }
  }
  
  old = augcoeffmatrix
  
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
  
  # polynomial strings
  
  strings = c()
  functions = c()
  co = 0
  for(i in 1:length(x)-1) {
    strings[i] = "a*x^2 + b*x + c"
  }
  
  co = 1
  for(j in seq(0, nrow(augcoeffmatrix), 3)){
    if(j==0) strings[co] = sub("a", "0", sub("b", toString(augcoeffmatrix[j+1,ncol(augcoeffmatrix)]), sub("c", toString(augcoeffmatrix[j+2,ncol(augcoeffmatrix)]), strings[i])))
    else strings[co] = sub("a", toString(augcoeffmatrix[j,ncol(augcoeffmatrix)]),sub("b", toString(augcoeffmatrix[j+1,ncol(augcoeffmatrix)]), sub("c", toString(augcoeffmatrix[j+2,ncol(augcoeffmatrix)]), strings[i])))
    co = co+1
    }
  for(i in 1:length(x)-1) {
    strings[i] = paste("function(x)", strings[i])
  }
  formula = c()
  print(strings[1])
  print(strings[2])
  
  for(i in 1:length(x)-1){
    if(number >= x[i] && number <= x[i+1]){
      formula = strings[i]
      break
    }
  }
  
  return(list(strings = strings, formula = formula, augcoeffmatrix = augcoeffmatrix, old=old))
  
}