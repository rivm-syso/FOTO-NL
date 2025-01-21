#' @title CheckMapConc
#' @name CheckMapConc
#' @author jaap slootweg
#' @description in prep of combining prep concentrations, and msPAFfen
#' NB hardwired! aquocode "OS" and "Corg" are converted to  "TSS" resp. "DOC"
#' @param x dataframe? with concentrations, substances(aquo and/or CAS) and SampleID, like from the access tool
#' do not forget stringsAsFactors = F for proper matching
#' @param DataSource who provided the data (for example Dutch waterschap)
#' @param SampleID name to be used as SampleID: unique place and time unit
#' @param ChemIDnames to be used to match with aquocode/CAS
#' @param MeasuredValue name to be used as concentration
#' @param Unit = "Unit"
#' @param LimitIndicator = "LimitIndicator",
#' @param PreTreatment = "hoedanigheid",
#' @param SampleDate = "SampleDate",
#' @param X = "xcoordinaat",
#' @param Y = "ycoordinaat",
#' @param fac2ugL = 1 multiplyer for proper unit
#' @param ... additional column names, as given in x
#' @param DateAsYearMonthDay boolean indicating separate columns, or character indicating format (see as.Date())
#' @return dataframe with concentrations, checked with names mapped to standard
#' @export
CheckMapConc <- function(x, DataSource = NA,
                         SampleID = "SampleID", 
                         Meetpuntcode = "Meetpuntcode",
                         ChemIDnames = c("AquoCode", "CAS"), 
                         MeasuredValue = "MeasuredValue",
                         Unit = "Unit",
                         LimitIndicator = "LimitIndicator",
                         PreTreatment = "hoedanigheid",
                         SampleDate = "SampleDate",
                         X = "xcoordinaat",
                         Y = "ycoordinaat",
                         DateAsYearMonthDay = T,
                         fac2ugL = 1,  ...){
  addedColomns <- list(...)
  stopifnot(length(ChemIDnames) > 0)
  if(is.na(SampleID)) {
    SampleID = "SampleID"
    x$SampleID <- NA
  } 
  if(is.na(PreTreatment)) {
    PreTreatment <- "PreTreatment"
    x$PreTreatment <- NA
  }
  if(is.na(LimitIndicator)) {
    LimitIndicator = "LimitIndicator"
    x$LimitIndicator <- NA
  } 
  if(is.na(Meetpuntcode)) {
    Meetpuntcode = "Meetpuntcode"
    x$Meetpuntcode <- NA
  } 
  if(is.na(X)) {
    Meetpuntcode = "X"
    x$Meetpuntcode <- NA
  } 
  if(is.na(Y)) {
    Meetpuntcode = "Y"
    x$Meetpuntcode <- NA
  } 
  
  keyNames <- match(tolower(c(SampleID, ChemIDnames, MeasuredValue, Unit, Meetpuntcode, 
                              PreTreatment, LimitIndicator, X, Y)),
                    tolower(names(x)))
  #keyNames <- keyNames[!is.na(keyNames)]
  stopifnot(!anyNA(keyNames))
  ret <- x[,keyNames] #and make sure of the up/low case
  names(ret) <- c("SampleID", ChemIDnames, "MeasuredValue", "Unit", "Meetpuntcode", "PreTreatment", "LimitIndicator", "X", "Y")
  Adding <- do.call(cbind, lapply(addedColomns, function(addCol){
    ToBind <- data.frame(x[,addCol])
    names(ToBind) <- names(addCol)
    ToBind
  }))
  if(length(addedColomns)>0) ret <- cbind(ret,Adding)
  if (fac2ugL != 1)  ret$MeasuredValue <- ret$MeasuredValue * fac2ugL else
    ret$MeasuredValue <- as.numeric(ret$MeasuredValue)
  ret$DataSource <- DataSource
  if (DateAsYearMonthDay == T) { #== T, it can be a string, avoids error
    stopifnot("Year" %in% names(x), "Month" %in% names(x) , "Day" %in% names(x))
    ret$SampleDate <- as.Date(paste(x$Year, x$Month , x$Day, sep = "-"))
  } else if (is.character(DateAsYearMonthDay)) {
    ret$SampleDate <- as.Date(x[,SampleDate], DateAsYearMonthDay)
  } else {#it's an excel-date trick!
    stopifnot(SampleDate %in% names(x))
    ret$SampleDate <- excelDate2Date(x[,SampleDate])
  }
  #NB fixed substitutions !!
  for (nm in ChemIDnames){
    ColNr <- match(nm, names(ret))
    ToChange <- ret[,ColNr]=="OS" & !is.na(ret[,ColNr])
    ret[ToChange,ColNr] <- "TSS"
    ToChange <- ret[,ColNr]=="Corg" & !is.na(ret[,ColNr])
    ret[ToChange,ColNr] <- "DOC"
  }
  ret
}
