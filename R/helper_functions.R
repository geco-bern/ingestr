
extrapolate_missing_headtail <- function(
  ddf
){
  
  ## extrapolate to missing values at head and tail using mean seasonal cycle
  ##--------------------------------------
  
  ## new: fill gaps at head
  idxs <- findna_head( ddf$var )
  if (length(idxs)>0){
    rlang::warn("Filling values with last available data point at head")
  }
  ddf$var[idxs] <- ddf$var[max(idxs)+1]
  
  ## new: fill gaps at tail
  idxs <- findna_tail( ddf$var )
  if (length(idxs)>0){
    rlang::warn("Filling values with last available data point at tail.")
  }
  ddf$var[idxs] <- ddf$var[min(idxs)-1]
  
  vec <- ddf %>%
    dplyr::pull(var)
  
  return(vec)
}

findna_headtail <- function(vec) {
  
  ## Remove (cut) NAs from the head and tail of a vector.
  ## Returns the indexes to be dropped from a vector
  
  idxs <- c(findna_head(vec), findna_tail(vec))
  
  return(idxs)
  
}

findna_head <- function(vec) {
  
  ## Remove (cut) NAs from the head and tail of a vector.
  ## Returns the indexes to be dropped from a vector
  
  ## Get indeces of consecutive NAs at head
  if (is.na(vec[1])){
    idx <- 0
    cuthead <- 1
    while ( idx < length(vec) ){
      idx <- idx + 1
      test <- utils::head( vec, idx )
      if (any(!is.na(test))){
        ## first non-NA found at position idx
        cuthead <- idx - 1
        break
      }
    }
    idxs_head <- 1:cuthead
  } else {
    idxs_head <- c()
  }
  
  return(idxs_head)
}

findna_tail <- function( vec ){
  
  ## Remove (cut) NAs from the head and tail of a vector.
  ## Returns the indexes to be dropped from a vector
  
  ## Get indeces of consecutive NAs at tail
  if (is.na(vec[length(vec)])){
    idx <- 0
    cuttail <- 1
    while ( idx < length(vec) ){
      idx <- idx + 1
      test <- utils::tail( vec, idx )
      if (any(!is.na(test))){
        ## first non-NA found at position idx, counting from tail
        cuttail <- idx - 1
        break
      }
    }
    idxs_tail <- (length(vec)-cuttail+1):length(vec)
  } else {
    idxs_tail <- c()
  }
  
  return(idxs_tail)
  
}

na.omit.list <- function(y) { return(y[!sapply(y, function(x) all(is.na(x)))]) }