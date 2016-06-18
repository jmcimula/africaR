
#some libraries

library(stringr)
library(stringi)

#Sub functions

#(1) Number of pages to explore
getPageNumber <- function(x){
    
	#Pattern
	inX <- c("[:alpha:]","[:punct:]",">","<","=")
	rcX <- x #Assignment
	
	for (j in 1 : length(inX)){
	    #replace pattern to null
  	    rcX <- str_replace_all(rcX,inX[j],"") 
		
    }#End for
	
	rcX <- as.integer(str_trim(rcX))#conversion to integer
	
	y <- as.integer(rcX/20) #Diving by the max value on the screen
	
	#Getting number the sub pages to explore
	if (rcX %% 20 > 0) {	  y <- y + 1
	}else{ y <- y}#if-condition
	
	return (y)

}#End function getPageNumber


#(2) Getting the status of the project

getStatus <- function (x){

return (str_trim(tolower(substr(x, 1, str_length(x) - 4))))

}#End function getStatus

#(3) Getting the name of the country

getCountry <- function (x){
#Getting the line of the title of project which contains the name of the host country
	e <- unlist(gregexpr(pattern = ",", x)) - 1	  
	x <- substr(x, 1, e)
	
	e <- unlist(gregexpr(pattern = ":", x)) + 1
	y <- str_trim(substr(x, e, str_length(x)))
	
	if (str_length(y) >= 30){
	        
			#Text processing
	    e <- unlist(gregexpr(pattern = ":", y)) + 1 
			y <- str_trim(substr(y, e, str_length(y)))
			y <- stri_extract_last_boundaries(y)
	}
    #Special character for Ivory Coast and Sao Tome
	if (str_detect (tolower(y), "ivoire") == TRUE) { y <- "Cote d'Ivoire"}
	else if (str_detect (tolower(y), "ncipe") == TRUE) {y <- "Sao Tome and Principe"}
	else { y <- y }
	
    return (y) 
}

#(4) Getting the link of the project
getLink <- function (x){

return (str_trim(tolower(substr(x, 1, str_length(x)))))

}

