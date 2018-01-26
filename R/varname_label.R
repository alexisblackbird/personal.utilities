#' varname_label
#'  
#'  Renames variables based on text parsed from a data key.
#'  
#'  Renames variables by parsing text from descriptions of the variable. 
#'  Must be in the format of a data frame (named datakey) with variables matching the data set (named data), and containing only a single string.
#'  Intended to work with Qualtrics' output of question text. 
#'  
#'  @param vargrp String pattern to rename, renames all variables with this pattern in them.
#'  @param label A string common label to preface the new variable name with. 
#'  @param pattern A string section to be removed from labels in the data key, such that any remaining text is added to the variable name.
#'  @param key True returns the renamed data key, false returns the data. Defaults to false.
#'  @export

varname_label <- function(vargrp, label, pattern, key = FALSE){
  for(i in names(data)){
    if(str_detect(i, vargrp) && !str_detect(i, "TEXT")){
      variable <- i
      length <- str_length(pattern)
      extracted <- str_trim(str_sub(get(paste0(variable), datakey), length))
      newname <- paste0(label, ".", tolower(extracted))
      newname <- str_replace_all(newname, " ", "_")
      names(data)[names(data) == variable] <- newname
      names(datakey)[names(datakey) == variable] <- newname
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









