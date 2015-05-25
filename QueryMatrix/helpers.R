library(sqldf)

getParamFromFile <- function(file_path) {
  r = read.csv(file = file_path, header = TRUE, sep = ";")
  r
}

buildQuery <- function(paramList) {
  statments = c()
  
  for(i in seq(1,length(paramList$name))){
    n = paramList$name[i]
    t = paramList$type[i]
    v = paramList$value[i]
    cond = character()
    
    if(t %in% c("String", "Factor")) {
      if(v != "") cond = paste0(n," = \"",v,"\"")
    }
    else
    {
      if(t == "Logical") {
        if(v != "-1") cond = paste0(n," = ",v)
      }
      else { # Numeric or Probability
        s = strsplit(x = v, split = ":", fixed = TRUE)[[1]]
        if(s[1] != "NA" && s[2] != "NA") {
          cond = paste0(n," BETWEEN ",s[1]," AND ",s[2])
        }
        else
        {
          if(s[1] != "NA") {
            cond = paste0(n," >= ",s[1])
          }
          else
          {
            if(s[2] != "NA") {
              cond = paste0(n," <= ",s[2])
            }
          }
        }
      }
    }
    
    if(length(cond) > 0) statments = c(statments, cond)
    
  }
  
  return(paste(statments, collapse = " AND "))
}


runQuery <- function(dataset, query) {
  fi <- file(dataset)
  #res = read.csv.sql(file = dataset, header = TRUE, sep = ",", sql = paste0("select * FROM file WHERE ",query)) 
  res = sqldf(paste0("select * FROM fi WHERE ",query), file.format = list(header = TRUE, sep = ",")) 
  close(fi)
  
  return(res)
}
