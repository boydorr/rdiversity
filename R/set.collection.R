#' Set Collection
#' 
#' This object contains 
#' 
#' @param data Object of class \code{matrix}
#' @param similarity Object of class \code{character}
#' @param zmatrix Object of class \code{matrix}
#' @param lookup Object of class \code{data.frame}
#' @return Object of class \code{initDiv}
#' 
#' @include calculate.zmatrix.R class-initDiv.R
#' @export
#' 
set.collection <- function(data, similarity = NA, zmatrix = NA, lookup = NA) {
  
  # Check pmatrix
  if(is.data.frame(data)) data <- as.matrix(data)
  if(sum(data)!=1) data <- data / sum(data)
  if(is.null(row.names(data))) row.names(data) <- paste('type', 1:nrow(data))
  if(is.null(colnames(data))) colnames(data) <- paste('subcommunity', 1:ncol(data))
  
  if(is.matrix(zmatrix)) {
    # If the zmatrix is provided, then the similarity argument is ignored
    new('initDiv', data, zmatrix = zmatrix)
    
  }else if(is.na(zmatrix) & !is.na(similarity)) {
    # If the similarity is provided, then calculate the zmatrix
    zmatrix <- calculate.zmatrix(similarity, data, lookup)
    new('initDiv', data, zmatrix = zmatrix)
    
  }else if(is.na(zmatrix) & is.na(similarity)) {
    # If neither similarity nor zmatrix arguments are provided, assume a 
    # naive-type case
    zmatrix <- calculate.zmatrix(data)
    new('initDiv', data, zmatrix = zmatrix)
  }else {
    stop('Something is wrong')
  }
}

