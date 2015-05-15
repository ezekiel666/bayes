source("include.r")
require(ape)

tan = function(x, ...)
  UseMethod("tan")

tan.default = function(data, ...) {
  conditionalMutualInformation = function( a_i, pos_i, a_j, pos_j, a_k, pos_k) {
    retVal = 0;
    partial = 0;
    for(i in 1:length(a_i)){
      a_i_condition = list(pos = pos_i, val = a_i[i], fun = function(x,y){ !is.na(x) && x == y })
      a_i_exist = list(pos = pos_i, val = NA, fun = function(x,y){ !is.na(x) })
      for(j in 1:length(a_j)){
        a_j_condition = list(pos = pos_j, val = a_j[j], fun = function(x,y){ !is.na(x) && x == y })
        a_j_exist = list(pos = pos_j, val = NA, fun = function(x,y){ !is.na(x) })
        for(k in 1:length(a_k)) {
          a_k_condition = list(pos = pos_k, val = a_k[k], fun = function(x,y){ !is.na(x) && x == y })
          a_k_exist = list(pos = pos_k, val = NA, fun = function(x,y){ !is.na(x) })
          
          # Probabilities computing
          
          # sum_prob = P(A_i, A_j, A_k) = |T a_i, a_j, a_k | / |T a_i=?, a_j=?, a_k=?|
          a_i_j_k_cond        = sum( apply(data, 1, test_row, conditions = list( a_i_condition, a_j_condition, a_k_condition)) )
          a_i_j_k_exist       = sum( apply(data, 1, test_row, conditions = list( a_i_exist, a_j_exist, a_k_exist)))
          sum_prob            = a_i_j_k_cond / a_i_j_k_exist
          
          # a_i_j_prob = P(A_i, A_j | A_k) = |T a_i, a_j, a_k | / |T a_i=?, a_j=?, a_k|
          #a_i_j_k_cond      = sum( apply(data, 1, test_row, conditions = list( a_i_condition, a_j_condition, a_k_condition)) )
          a_i_j_exist_a_k_cond= sum( apply(data, 1, test_row, conditions = list(a_i_exist, a_j_exist, a_k_condition)))
          a_i_j_m_est         = length( a_i) * length( a_j)
          a_i_j_prob          = ( a_i_j_k_cond + 1 ) / ( a_i_j_exist_a_k_cond + a_i_j_m_est )
          
          # a_i_prob = P(A_i | A_k) = |T a_i, a_k | / |T a_i=?, a_k|
          a_i_k_cond          = sum( apply(data, 1, test_row, conditions = list( a_i_condition, a_k_condition)) )
          a_i_m_est           = length( a_i)
          a_i_exist_a_k_cond  = sum( apply(data, 1, test_row, conditions = list(a_i_exist, a_k_condition)))
          a_i_prob            = ( a_i_k_cond + 1 ) / ( a_i_exist_a_k_cond + a_i_m_est )
          
          # a_j_prob = P(A_j | A_k) = |T a_j, a_k | / |T a_j=?, a_k|
          a_j_k_cond          = sum( apply(data, 1, test_row, conditions = list( a_j_condition, a_k_condition)) )
          a_j_m_est           = length( a_j)
          a_j_exist_a_k_cond  = sum( apply(data, 1, test_row, conditions = list(a_j_exist, a_k_condition)))
          a_j_prob            = ( a_j_k_cond + 1 ) / ( a_j_exist_a_k_cond + a_j_m_est)

          partial = sum_prob * log2( a_i_j_prob / ( a_i_prob * a_j_prob ) )          
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
      if(cond$fun(x[cond$pos], cond$val) ){
        retval <<- retVal & 1
      } else {
        retVal <<- retVal & 0
      }
    })
    return(retVal);
  }
  computeConditionalProbabilities = function(data, parents, attributes) {
    conditionalProbabilities = c()
    
    for(i in 1:( length(attributes) - 1) ) {
      parent    = parents[[i]]
      ndim = c(length(attributes[[i]]))
      
      for( p in 1:length(parent) )
        ndim = c( ndim , length( attributes[[ parent[p] ]] ) )
      
      probabilities = array(0, dim = ndim)
      
      for(a_i in 1:length( attributes[[i]] ) ) {
        a_i_condition = list(pos = i, val = attributes[[i]][a_i], fun = function(x,y){ !is.na(x) && x == y })
        a_i_exist     = list(pos = i, val = NA, fun = function(x,y){ !is.na(x) })
        
        for(c in 1:length(attributes[[ parent[1] ]]) ) { ## class is always first on parent list
          c_condition = list(pos = parent[1], val = attributes[[ parent[1] ]][c], fun = function(x,y){ !is.na(x) && x == y })
          
          if( i > 1 ) {
            for(a_j in 1:length(attributes[[ parent[2] ]]) ) { ## another att is always second on parent list
              a_j_condition = list(pos = parent[2], val = attributes[[ parent[2] ]][a_j], fun = function(x,y){ !is.na(x) && x == y })

              a_i_c_a_j_cond      = sum( apply(data, 1, test_row, conditions = list( a_i_condition, c_condition, a_j_condition )))
              a_i_exist_c_a_j_cond= sum( apply(data, 1, test_row, conditions = list( a_i_exist, c_condition, a_j_condition)))
              m = length(attributes[[i]])
              
              probabilities[a_i,c,a_j] = (a_i_c_a_j_cond + 1) / (a_i_exist_c_a_j_cond + m)
            }
          } else {
            a_i_c_cond            = sum( apply(data, 1, test_row, conditions = list( a_i_condition, c_condition)))
            a_i_exist_c_cond      = sum( apply(data, 1, test_row, conditions = list( a_i_exist, c_condition)))                       
            m = length(attributes[[i]])
            
            probabilities[a_i, c] = ( a_i_c_cond + 1) / (a_i_exist_c_cond + m)
          }
        }
      }
      conditionalProbabilities = c(conditionalProbabilities, list(probabilities))
    }
    
    ## class probabilities
    probabilities = array(0, dim = c(length(attributes[[length(attributes)]])) )
    
    c_exist = list(pos = length(attributes), val = NA, fun = function(x,y){ !is.na(x) })
    c_exist = list( c_exist )
    c_exist_num = sum( apply(data, 1, test_row, conditions = c_exist) )
    
    for( c in 1:length( attributes[[length(attributes)]] ) ) {
      c_condition = list(pos = length(attributes), val = attributes[[length(attributes)]][c], fun = function(x,y){ !is.na(x) && x == y })
      c_condition = list( c_condition)
      c_cond_num  = sum( apply(data, 1, test_row, conditions = c_condition))
      c_prob      = c_cond_num / c_exist_num
      probabilities[c] = c_prob
    }
    
    return(c(conditionalProbabilities, list(probabilities))) 
  }
  getModes = function(data) {
    modes = lapply(data, function(x) {
      newData = na.omit(x)
      Mode(newData)
    })
    return(modes)
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
  modes = getModes(data)
  
  structure(list(attributes = attributes, 
                 parents = parents,
                 conditionalProbabilities = conditionalProbabilities,
                 modes = modes),
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

  type = match.arg(type)
  newdata[] = lapply(newdata, factor)
  outputVector = apply(newdata, 1, function(x, type, object) {
    classProb = c()
    for(cls in 1:length(object$attributes[[length(object$attributes)]]) ) { ## for each class value
      
      probability = object$conditionalProbabilities[[length(object$attributes)]][cls];
      for(argAttr in 1:(length(x) - 1) ) {
        if( !is.na(x[[argAttr]]) ) {
          parents = object$parents[[argAttr]]
          att1ValIdx = attValIdx(x[[argAttr]], object$attributes[[argAttr]] )
          
          if(length(parents) == 1) {
            probability = probability * object$conditionalProbabilities[[argAttr]][att1ValIdx,cls]
          } else if(length(parents) == 2) {
            if( !is.na(x[[ parents[2] ]]) ) {
              att2ValIdx = attValIdx( x[[ parents[2] ]], object$attributes[[ parents[2] ]] )
            } else {
              att2ValIdx = attValIdx( object$modes[[ parents[2] ]], object$attributes[[ parents[2] ]] )
            }
            probability = probability * object$conditionalProbabilities[[argAttr]][att1ValIdx,cls,att2ValIdx]
          } else {
            stop("ERROR")
          }
        }
      }
      classProb = c(classProb, probability)
    }
    return(classProb)
  }, type = type, object = object )
  
  classes  = object$attributes[[ length(object$attributes) ]]
  lvlNames = levels(classes)
  
  if( type == "class" ) { # return array of factors
    retVal = c()
    for( i in 1:ncol(outputVector) ) {
      idx = which.max(outputVector[,i])
      retVal = c(retVal, classes[idx])
    }
    retVal = factor(retVal, levels = 1:length(classes), labels = lvlNames)
  } else { # return matrix which col names coresponds to class names
    retVal = matrix(0, nrow = ncol(outputVector), ncol = nrow(outputVector),
                    dimnames = list(NULL, lvlNames))
    for( i in 1:ncol(outputVector) ) {
      for( j in 1:nrow(outputVector) ) {
        retVal[i,j] = outputVector[j,i]
      }
    }
    
  }
  
  return(retVal)
}

## example usage
set.seed(1235)
sam <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3) )
trainData <- iris[sam==1,]
testData <- iris[sam==2,]

model = tan(testData)
predicted_class = predict(model, testData, type = "class")
predicted_raw = predict(model, testData, type = "raw")
