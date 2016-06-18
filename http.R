#http link and reference

library(httr)

url <- "http://www.afdb.org/en/projects-and-operations/project-portfolio/"

r <- !http_error(url)

if (r == TRUE) 
            {  
                http <- url 
				httProj <- paste (http, "project/", sep = "")
			}
#End-if
