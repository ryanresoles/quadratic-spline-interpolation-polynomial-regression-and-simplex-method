# checks if there's still a negative number in the last row
countNegative <- function(m){
  x = 0
  for(i in 1:23){
    if(m[16,i]<0) x = x+1
  }
  return(x)
}


Simplex <- function(simp){
  
  # stores the values
  simplexMatrix = matrix(nrow = 4, ncol = 6)
  colN = c("supply", "w1", "w2", "w3", "w4", "w5")
  rowN = c("p1", "p2", "p3", "demand")
  colnames(simplexMatrix) <- colN
  rownames(simplexMatrix) <- rowN
  simplexMatrix[,1] = simp[[1]]
  simplexMatrix[,2] = simp[[2]]
  simplexMatrix[,3] = simp[[3]]
  simplexMatrix[,4] = simp[[4]]
  simplexMatrix[,5] = simp[[5]]
  simplexMatrix[,6] = simp[[6]]


  
  # sets up initial tableau
  solution <- matrix(0, nrow = 9, ncol = 16)
  init = 1
  end = 5
  
  # adds values from user input to the second matrix
  solution[1:3,16] <- simplexMatrix[1:3,6]
  solution[4:8,16] <- simplexMatrix[4,1:5]*-1
  solution[9,1:5] <- simplexMatrix[1,1:5]
  solution[9,6:10] <- simplexMatrix[2,1:5]
  solution[9,11:15] <- simplexMatrix[3,1:5]
  solution[1,1:5] <- -1 
  solution[2,6:10] <- -1  
  solution[3,11:15] <- -1
  number=0
  
  
  for(it in 1:3){
    for(i in 1:5){
      solution[i+3,i+(5*number)] <- 1
    }
    number = number+1
  }
  
  # transposes initial matrix
  initialTableau = t(solution)
  # sets up initial tableau and adds 15 slack variables
  for(i in 1:15){
    initialTableau = cbind(initialTableau, 0)
  }
  initialTableau[,24] <- initialTableau[,9]
  initialTableau[,9] <- 0
  
  for(i in 9:23){
    for(j in 1:15){
      if(i == j+8) initialTableau[j,i] = 1
    }
  }
  
  old = initialTableau
  
  
  tables = list()
  counter = 1
  
  # executes if a negative number is in the final row
  while(countNegative(initialTableau) != 0){
    testRatio = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    # gets pivot column
    temp = min(initialTableau[16,])
    COLUMN = 0
    for(i in 1:23){
      if(temp == initialTableau[16,i]){
        # minimum number
        COLUMN = i
        break
      }
    }
    
    # looks for pivot element
    for(j in 1:15){
      if(initialTableau[j,COLUMN] > 0){
        testRatio[j] = initialTableau[j,24]/initialTableau[j,COLUMN]
      }
      else testRatio[j] = 999
    }
    
    # gets position of minimum test ratio
    COL2 = min(testRatio)
    subs=0
    for(i in 1:15){
      if(testRatio[i]==COL2){
        subs = i
        break
      }
    }
  

    PivotElement = initialTableau[subs,COLUMN]

    # normalizes the pivot row
    initialTableau[subs,] =  initialTableau[subs,]/PivotElement
    for(i in 1:nrow(initialTableau)){
      # GaussJordan does not execute on the pivot row
      if(i != subs){
        initialTableau[i,] = initialTableau[i,] - (initialTableau[i,COLUMN]*initialTableau[subs,])
      }
    }
    tables[[counter]] <- initialTableau
    counter = counter+1
  }
  
  final_matrix = matrix(0, nrow=4,ncol=5)
  colnames(final_matrix) <- c("Warehouse 1", "Warehouse 2", "Warehouse 3", "Warehouse 4", "Warehouse 5")
  rownames(final_matrix) <- c("Plant 1", "Plant 2", "Plant 3", "Minimum Cost")
  final_matrix[1,1:5] <- initialTableau[16,9:13]
  final_matrix[2,1:5] <- initialTableau[16,14:18]
  final_matrix[3,1:5] <- initialTableau[16,19:23]
  final_matrix[4,5] <- initialTableau[16,24]

  
  
  return(list(initialTableau=old, final_matrix=final_matrix, finalTableau = initialTableau, counter=counter, tables=tables))
}