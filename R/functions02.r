
#' Code > Insert ... Skeletor
#'
#' @param chla 
#'
#' @return
#' @export
#'
#' @examples
fixCholorophyllData <- function(chla){
  
  actualYear <- 0
  actualMonth <- 0
  chla$Date <- ymd("1900-01-01")
  fechaTemp <- dmy("01-01-1900")
  
  for(i in 1:nrow(chla)){
    
    #if (is.na(chla$Year[i]))
    if(is.na(chla[i,1])) 
      chla$Year[i] <- actualYear

        else
      actualYear <- chla$Year[i]
    
    chla$Date <- ymd(paste(chla$Year, chla$Month,1))
    
    if(is.na(chla[i,6]))
      fechaTemp <- dmy(chla$Month[i])
      chla$Date[i] <- fechaTemp
#    else
#      chla$Date <- fechaTemp

  }
  
#  chla$Date <- ymd(paste(chla$Year, chla$Month,1))


  chla$IntegE1 <- abs(chla$IntegE1)
  
  chla$IntegE2 <- abs(as.numeric(chla$IntegE2))
  
  
  return(chla) 
}
