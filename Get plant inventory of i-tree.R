library(pdftools)
library(stringr)
setwd("C:/Users/kangj/Documents/R")
rawtext <- pdf_text("./new.pdf")
fun_gettable <- function(table) {
  table <- str_split(table, "\n", simplify = TRUE)
  table <- str_replace_all(table, "\\s{2,}", "|")
  table <- str_replace_all(table, "\r", "")
}
x <- fun_gettable(rawtext[1])

rawtable <- character()
for (i in 140:168) {
  newtable <- fun_gettable(rawtext[i])
  rawtable <- c(rawtable, newtable)
}


class(rawtable)
write.csv(rawtable, "rawtable.csv")

