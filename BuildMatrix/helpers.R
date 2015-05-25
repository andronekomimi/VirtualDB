getColumns <- function(file_path) {
  r = read.csv(file = file_path, header = FALSE, nrows = 1)
  
  cols = c()
  col = character()
  
  for(i in seq(1, ncol(r))) {
    if(grepl(pattern = "^[0-9]", perl = TRUE,
             x = as.character(r[1,i]))) {
      col = paste0("X",as.character(r[1,i]))
    }
    else
    {
      col = as.character(r[1,i])
    }
    
    cols = c(cols,col)
  }
  
  cols
}