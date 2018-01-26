#intended to work like varname_label, but by pulling the text from the data in a choice text output format from Qualtrics. The actual data is too messy for this to work.

varname_data <- function(vargrp, label, key = FALSE){
  for(i in names(data.choicetext)){
    if(str_detect(i, vargrp) && !str_detect(i, "TEXT")){
      variable <- i
      for(i in get(paste0(variable), data.choicetext)){
        if(str_length(i) > 1){
          print(i)
          extracted <- i
          newname <- paste0(label, ".", tolower(extracted))
          newname <- str_replace_all(newname, " ", "_")
          names(data)[names(data) == variable] <- newname
          names(datakey)[names(datakey) == variable] <- newname
        }
      }
    }
    if(str_detect(i, vargrp) && str_detect(i, "TEXT")){
      variable <- i
      newname <- paste0(label, ".custom")
      names(data)[names(data) == variable] <- newname
      names(datakey)[names(datakey) == variable] <- newname
    }
  }
  if(key == TRUE){
    return(datakey)
  } else {
    return(data)
  }
}



