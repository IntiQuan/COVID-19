# Only load once
if (!file.exists("Resources/population.csv")) {
  url <- "https://www.worldometers.info/world-population/population-by-country/"
  x <- download.file(url,destfile = "x.txt")
  suppressWarnings(html <- aux_fileread("x.txt",collapserows = FALSE))
  unlink("x.txt")
  x <- html[which(grepl("Holy See",html))]
  x <- gsub('<td style=\"font-weight: bold; font-size:15px; text-align:left\">','',x)
  x <- gsub(' <td>','',x)
  x <- gsub(' </td>',':::',x)
  x <- gsub('<a href=[^>]*>','',x)
  x <- gsub('<tr>','\n',x)
  x <- gsub('</td>',':::',x)
  x <- gsub('</a> <td style="font-weight: bold;">',' ',x)
  x <- gsub('%[^\n]+\n','\n',x)
  x <- gsub('%[^\n]+','',x)
  x <- gsub('</a>::: <td style="font-weight: bold;">',':::',x)
  x <- gsub('::: ',':::',x)
  x <- substr(x = x,start = aux_strFindAll(x,"1:::")$start[1],stop = nchar(x))
  x <- gsub(',','',x)
  x <- gsub('&amp;','&',x)
  x <- gsub('&eacute;','e',x)
  x <- gsub('&ccedil;','c',x)
  
  
  populationData <- do.call(rbind,lapply(aux_explode(x,"\n"), function (d) {
    z <- aux_explode(d,":::")
    data.frame(
      COUNTRY = z[[2]],
      POPULATION = z[[3]],
      stringsAsFactors = FALSE
    )
  }))
  
  populationData$COUNTRY <- aux_strtrim(populationData$COUNTRY)
  IQRsaveCSVdata(populationData,"Resources/population.csv")
}
