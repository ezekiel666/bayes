#rm(list=ls())
source("include.r")
require(ape)

## TODO
## - classification

## x is training dataset. Assumption was made that class attribute is the last attribute in this dataset.
tan = function(x, ...)
  UseMethod("tan")

tan.default = function(data, ...) {
  conditionalMutualInformation = function( a_i, pos_i, a_j, pos_j, a_k, pos_k) {
    retVal = 0;
    partial = 0;
    for(i in 1:length(a_i)){
      a_i_condition = list(pos = pos_i, val = a_i[i])
      for(j in 1:length(a_j)){
        a_j_condition = list(pos = pos_j, val = a_j[j])
        for(k in 1:length(a_k)) {
          a_k_condition = list(pos = pos_k, val = a_k[k])
          
          a_i_j_k_condition   = list( a_i_condition, a_j_condition, a_k_condition)
          a_i_ant_condition   = list( a_i_condition, a_k_condition)
          a_j_ant_condition   = list( a_j_condition, a_k_condition)
          a_k_cons_condition  = list( a_k_condition)
          
          a_i_j_k_number  = sum( apply(data, 1, test_row, conditions = a_i_j_k_condition) )
          a_i_j_m_est     = length( a_i) * length( a_j)
          a_i_j_k_m_est   = a_i_j_m_est * length( a_k)
          a_i_ant_number  = sum( apply(data, 1, test_row, conditions = a_i_ant_condition) )
          a_i_ant_m_est   = length( a_i)
          a_j_ant_number  = sum( apply(data, 1, test_row, conditions = a_j_ant_condition) )
          a_j_ant_m_est   = length( a_j)
          a_k_cons_number = sum( apply(data, 1, test_row, conditions = a_k_cons_condition) )
          
          a_i_ant_prob    = ( a_i_ant_number + 1 ) / ( a_k_cons_number + a_i_ant_m_est )
          a_j_ant_prob    = ( a_j_ant_number + 1 ) / ( a_k_cons_number + a_j_ant_m_est )
          a_i_j_ant_prob  = ( a_i_j_k_number + 1 ) / ( a_k_cons_number + a_i_j_m_est )
          sum_prob        = ( a_i_j_k_number + 1 ) / ( nrow(data)      + a_i_j_k_m_est )

          partial = sum_prob * log2( a_i_j_ant_prob / ( a_i_ant_prob * a_j_ant_prob ) )          
          retVal = retVal + partial
        }
      }
    }
    return(retVal);
  }
  getParents = function(maximalSpanningTree) {
    ## first attr is always node
    listOfParents = c()
    parents = c( (ncol(maximalSpanningTree) + 1) )
    for(i in 1:ncol(maximalSpanningTree) ) {
      listOfParents = c(listOfParents, list(parents))
    }
    listOfParents = updateChildrens(maximalSpanningTree, listOfParents, 1)$l
    return(listOfParents)
  }
  updateChildrens = function(maximalSpanningTree, listOfParents, parent) {
    for(i in 1:ncol(maximalSpanningTree) ) {
      if( maximalSpanningTree[parent,i] == 1 ) {
        listOfParents[[i]] = c(listOfParents[[i]], parent)
        maximalSpanningTree[parent,i] = maximalSpanningTree[i,parent] = 0
        update = updateChildrens(maximalSpanningTree, listOfParents, i)
        maximalSpanningTree = update$mat
        listOfParents = update$l
      }
    }
    return(list(l = listOfParents, mat = maximalSpanningTree))
  }
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
  computeConditionalProbabilities = function(data, parents, len) {
    conditionalProbabilities = c()
    
    for(i in 1:( length(attributes) - 1) ) {
      parent    = parents[[i]]
      ndim = c(length(attributes[[i]]))
      
      for( p in 1:length(parent) )
        ndim = c( ndim , length( attributes[[ parent[p] ]] ) )
      
      probabilities = array(0, dim = ndim)
      
      for(a_i in 1:length( attributes[[i]] ) ) {
        a_i_condition = list(pos = i, val = attributes[[i]][a_i] )
        
        for(c in 1:length(attributes[[ parent[1] ]]) ) { ## class is always first on parent list
          c_condition = list(pos = parent[1], val = attributes[[ parent[1] ]][c] )
          
          if( i > 1 ) {
            for(a_j in 1:length(attributes[[ parent[2] ]]) ) { ## another att is always second on parent list
              a_j_condition = list(pos = parent[2], val = attributes[[ parent[2] ]][a_j] )
              
              a_i_c_a_j_condition = list( a_i_condition, c_condition, a_j_condition )
              c_a_j_condition     = list( c_condition, a_j_condition )
              
              a_i_c_a_j_number    = sum( apply(data, 1, test_row, conditions = a_i_c_a_j_condition) )
              c_a_j_number        = sum( apply(data, 1, test_row, conditions = c_a_j_condition) )
              
              m = length(attributes[[i]])
              
              probabilities[a_i,c,a_j] = (a_i_c_a_j_number + 1) / (c_a_j_number + m)
            }
          } else {
            
            a_i_c_condition       = list( a_i_condition, c_condition)
            c_condition           = list( c_condition)
            
            a_i_c_number          = sum( apply(data, 1, test_row, conditions = a_i_c_condition) )
            c_number              = sum( apply(data, 1, test_row, conditions = c_condition) )
            
            m = length(attributes[[i]])
            
            probabilities[a_i, c] = ( a_i_c_number + 1) / (c_number + m)
            
          }
        }
      }
      conditionalProbabilities = c(conditionalProbabilities, list(probabilities))
    }
    
    ## class probabilities
    probabilities = array(0, dim = c(length(attributes[[length(attributes)]])) )
    for( c in 1:length(attributes[[length(attributes)]]) ) {
      c_condition = list( pos = length(attributes), val = attributes[[ parent[1] ]][c] )
      c_condition = list( c_condition)
      c_number    = sum( apply(data, 1, test_row, conditions = c_condition) )
      c_prob      = c_number / nrow(data)
      probabilities[c] = c_prob
    }
    
    return(c(conditionalProbabilities, list(probabilities))) 
  }
  
  data[] = lapply(data, factor)
  attributes = lapply(data, levels)
  attributes[] = lapply(attributes, factor)
  att_number = length(attributes)
  mutualInformationMatrix = matrix(data = 0, nrow = att_number - 1, ncol = att_number - 1)
  
  for(i in 1:(att_number - 1) ) {
    for( j in i:(att_number - 1) ) {
      if( i != j ) {
        mutualInformationMatrix[i,j] = 
          mutualInformationMatrix[j,i] = conditionalMutualInformation( attributes[[i]], i,
                                                                       attributes[[j]], j,
                                                                       attributes[[att_number]], att_number)
      }
    }
  }
  
  maximalSpanningTree = mst(apply(mutualInformationMatrix, 1:2, function(x) {
    if(x>=0)
      return(1/(1+x))
    else
      return(1+abs(x))
  }))
  parents = getParents(maximalSpanningTree)
  conditionalProbabilities = computeConditionalProbabilities(data, parents, attributes)
  
  structure(list(attributes = attributes, 
                 parents = parents,
                 conditionalProbabilities = conditionalProbabilities), 
            class="tan")
}

predict.tan = function(object, newdata, type = c("class","raw"), ...) {
  attValIdx = function(attVal, attArray) {
    idx = 0
    for(i in 1:length(attArray) ) {
      if(attVal == attArray[i])
        idx = i
    }
    cond = (idx > 0)
    stopifnot( cond )
    return(idx)
  }
  
  print("predict.tan")
  type <- match.arg(type)
  newdata[] = lapply(newdata, factor)
  
  outputVector = apply(newdata, 1, function(x, type, object) {
    classProb = c()
    for(cls in 1:length(object$attributes[[length(object$attributes)]]) ) { ## for each class value
      
      probability = 1;
      for(argAttr in 1:(length(x) - 1) ) {
        parents = object$parents[[argAttr]]
        att1ValIdx = attValIdx(x[[argAttr]], object$attributes[[argAttr]] )
        
        if(length(parents) == 1) {
          probability = probability * object$conditionalProbabilities[[argAttr]][att1ValIdx,cls]
        } else if(length(parents) == 2) {
          att2ValIdx = attValIdx( x[[ parents[2] ]], object$attributes[[ parents[2] ]] )
          probability = probability * object$conditionalProbabilities[[argAttr]][att1ValIdx,cls,att2ValIdx]
        } else {
          stop("ERROR")
        }
        
      }
      print(probability)
      classProb = c(classProb, probability)
    }
    return(classProb)
  }, type = type, object = object )
  
  return(outputVector)
}

set.seed(1235)
sam <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3) )
trainData <- iris[sam==1,]
testData <- iris[sam==2,]

test_data = read.csv("test_data.csv",header=TRUE,sep=";")
#model = tan(testData)
predicted = predict(model, testData, type = "class")

library(e1071)
nb = naiveBayes(Species ~ . , data = testData)
cla = predict(nb, testData, type="class")
raw = predict(nb, testData, type="raw")
