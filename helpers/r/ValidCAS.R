#' @title ValidCAS
#' @name ValidCAS
#' @author jaap slootweg
#' @description Checks the validity of a CAS code based on the checksum of CAS numbers.
#' Explanation CAS checksum: https://www.wikidoc.org/index.php/CAS_registry_number#:~:text=The%20checksum%20is%20calculated%20by,%3B%20105%20mod%2010%20%3D%205.
#' @param CAScode the CAS code or a vector of CAS codes
#' @return Vector of boolean
#' @export
ValidCAS <- function(CAScode){
  #remove or ignore -
  CascodeNodash <- gsub("-", "", as.character(CAScode))
  suppressWarnings(Screen1 <- !is.na(as.numeric(CascodeNodash)))
  CasCodeSingleChar <- sapply(CascodeNodash[Screen1], strsplit, "")

  suppressWarnings( CasCodeLastNumber <- sapply(CasCodeSingleChar, function(x){
    as.numeric(x[length(x)])
  })) #can generate NA's
  #avoid error; ie if length of vector < 5
  lengten <- sapply(CasCodeSingleChar, length)
  Screen1[Screen1] <- lengten > 4
  suppressWarnings( CasCodeChecksum <- sapply(CasCodeSingleChar[lengten > 4], function(x){
    revx <- rev(x)
    sum(sapply(1:(length(x)-1), function(y)
       as.numeric(y*as.numeric(revx[y+1]))))
  })) #dito
  Screen1[Screen1] <- (CasCodeChecksum %% 10) == CasCodeLastNumber
  Screen1
}
