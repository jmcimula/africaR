
#some libraries

library(stringr)
library(stringi)

#Text processing

#Sub functions
#List of the sponsors
adf <- "African Development Bank"

#(0) Getting the title of the project 
getTitle <- function (x){

    y  <- read_html(x) %>%
          html_nodes("title") %>%
          html_text()		  
        
		y <- str_replace_all(y, adf, "")
		y <- substr(y, 1, str_length(y) - 2)
	
	#return the title
	return (str_trim(y))
}

#(1) Number of pages to explore
getPageNumber <- function(x){
    
	#Pattern
	inX <- c("[:alpha:]","[:punct:]",">","<","=")
	rcX <- x #assignment
	
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

#(5) Function which checks the url of all the project and their status
getSearchReference <- function (urlR){

		urlR <- urlR
		urlR <- read_html(urlR)
			
		urlR <- urlR %>%
                html_nodes("tr td")%>%
			    html_text()
        
		UI <- urlR
		UI <- as.data.frame(UI)
		nbrow <- nrow(UI)
				
        #print(UI)
		valR <- data.frame()
		for (j in 1 : nbrow){

            if ( j %% 3 == 0 ){
	
		            status  <-  getStatus (UI[j,]) #Getting the status 
					country <-  getCountry (UI[j-1,]) #Getting country
					link    <-  paste(httProj, getLink(UI[j-2, ]), sep = "" )#Creating the link
					
					projectID <- getLink(UI[j-2, ]) #Project id
					amount  <- getAmount (link, status) #Estimated cost of the project
					title   <- getTitle(link) #Getting the title of the project
                    
					#title <- str_conv(title, "UTF-8")
					
					vloop <- data.frame(country, projectID, title, status, amount)
					valR  <- rbind(valR, vloop)
		            #setStatusRedict(link,pStatus)

	        }#if-condition

        }#End for
	
    return (valR)
}#End function

# (6) Browsing all the subpages related to the choice of the user from the interface
getData <- function (x){

    valR <- data.frame ()
	
	for (j in 1 : x - 1){
		
	   #List of project and creation of link + status
		    
	     if (j > 0) {
                urlR  <- paste(httpR, j, sep = "")
		vloop <- getSearchReference(urlR) #Call the function to collect all the project i.e link and status
            }else{ 
		vloop <- getSearchReference(httpR)
	    }#if-condition 
    
        valR <- rbind(valR, vloop)	
    }#End for
    #return
    return (valR)

}#End-function
