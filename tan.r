source("include.r")
require(ape)

## TODO
## - m-estimation !
## - classification

test_row = function(x, conditions) {
  retVal = 1;
  lapply(conditions, function(cond){
    if(cond$val == x[cond$pos] ){
      retval <<- retVal & 1
    } else {
      retVal <<- retVal & 0
    }
  })
  return(retVal);
}

## x is training dataset. Assumption was made that class attribute is the last attribute in this dataset.
tan = function(x, ...)
  UseMethod("tan")

tan.default = function(x, ...) {
  conditionalMutualInformation = function( a_i, pos_i, a_j, pos_j, a_k, pos_k) {
    retVal = 0;
    partial = 0;
    for(i in 1:length(a_i)){
      a_i_condition = list(pos = pos_i, val = a_i[i])
      for(j in 1:length(a_j)){
        a_j_condition = list(pos = pos_j, val = a_j[j])
        for(k in 1:length(a_k)) {
          a_k_condition = list(pos = pos_k, val = a_k[k])
          
          a_i_j_k_condition = list( a_i_condition, a_j_condition, a_k_condition)
          a_k_cons_condition = list(a_k_condition)
          a_i_ant_condition = list(a_i_condition, a_k_condition)
          a_j_ant_condition = list(a_j_condition, a_k_condition)
          
          a_i_j_k_number = sum( apply(data, 1, test_row, conditions = a_i_j_k_condition) )
          a_k_cons_number = sum( apply(data, 1, test_row, conditions = a_k_cons_condition) )
          a_i_ant_prob = sum( apply(data, 1, test_row, conditions = a_i_ant_condition) )
          a_j_ant_prob = sum( apply(data, 1, test_row, conditions = a_j_ant_condition) )
          
          a_i_ant_prob = a_i_ant_prob / a_k_cons_number
          a_j_ant_prob = a_j_ant_prob / a_k_cons_number
          a_i_j_ant_prob = a_i_j_k_number / a_k_cons_number
          sum_prob = a_i_j_k_number / nrow(data)

          partial = sum_prob * log2( a_i_j_ant_prob / ( a_i_ant_prob * a_j_ant_prob ) )          
          retVal = retVal + partial
        }
      }
    }
    return(retVal);
  }
  getParents = function(maximalSpanningTree) {
    ## always root node is attr1
    listOfParents = c()
    
    for(i in 1:ncol(maximalSpanningTree) ) {
      parents = c( (ncol(maximalSpanningTree) + 1) ) ## class attribute as a parent every other attribute
      for(j in 1:i ) {
        if(maximalSpanningTree[j,i] == 1) {
          parents = c(parents, j)      ## parent node
          break;
        }
      }
      listOfParents = c(listOfParents, list(parents))
    }
    
    listOfParents = c(listOfParents, list() ) # class desn't have any parents
    
    return(listOfParents)
  }
  
  
  data[] = lapply(data, factor)
  attributes = lapply(data, levels)
  att_number = length(attributes)
  mutualInformationMatrix = matrix(data = 0, nrow = att_number - 1, ncol = att_number - 1)
  
  for(i in 1:(att_number - 1) ) {
    for( j in i:(att_number - 1) ) {
      if( i != j ) {
        mutualInformationMatrix[i,j] = 
          mutualInformationMatrix[j,i] = conditionalMutualInformation( attributes[[i]], i,
                                                                       attributes[[j]], j,
                                                                       attributes[[att_number]], att_number) # class attribute
      }
    }
  }
   
  maximalSpanningTree = mst(apply(mutualInformationMatrix, 1:2, function(x) {
    if(x>=0)
      return(1/(1+x))
    else
      return(1+abs(x))
  }))
  
  for(i in 1:nrow(maximalSpanningTree)) {
    for(j in 1:i) {
        maximalSpanningTree[i,j] = 0
    }
  }
  
  parents = getParents(maximalSpanningTree)
  model = list(att = attributes, par = parents)
  return(model)
}

tan.predict = function(model, vector, ...) {
  print("tan.predict")
}

data = read.csv("test_data.csv",header=TRUE,sep=";")
model = tan(data)