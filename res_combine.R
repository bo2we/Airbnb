
pathout = "data/"

files = list.files(path = pathout, pattern = ".csv")
n <- length(files)

y  <- NULL

for (i in 1:n){ 
  
  tmp <- read.csv(file = paste0(pathout, files[i]))
  f <- stringi::stri_sub(ff[i], 10, 19)
  tmp$date <- f
  tmp <- tmp[1:nrow(tmp),]
  y <- rbind(y, tmp)
}

#header = c('ID','R', 'AGB', 'Mean DBH', 'Mean height',	'Max Height', 'stem density')
#colnames(y) = header

write.csv(y, 'df.csv', row.names = FALSE)


df <- read.csv("df.csv")
head(df)
