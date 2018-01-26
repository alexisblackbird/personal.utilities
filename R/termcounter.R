#' termcount
#'
#' Counts the number of distinct terms used in a a string.

#' Counts the number of terms used in a string that are separated by semicolon or comma (if semicolons were not used). 
#' @param x A vector of strings to be parsed.
#' @return A corresponding numeric vector of number of terms in the string
#' @export

termcount <- function(x){
  countlist <- c()
  for(i in x){
    count <- NA
    if(!is.na(i)){ #only work with those who actually entered something
      if(str_detect(i, ";")){ #use a ; count for those that used ;
        if(!str_sub(str_trim(i), -1) == ";"){ #if the last non-whitespace character isn't ; add one
          i <- paste0(str_trim(i), ";")
        }

        count <- str_count(i, ";")

        
      } else { #use a , count if they didn't use ;
        if(!str_sub(str_trim(i), -1) == ","){ #if the last non-whitespace character isn't , add one
          i <- paste0(str_trim(i), ",")
        }

        count <- str_count(i, ",")
      }
    }
    countlist <- c(countlist, count)
  }
  
  return(countlist)
}