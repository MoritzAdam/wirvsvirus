library("googlesheets4")
library("tidyverse")

sheets_deauth() 
test <- read_sheet("https://docs.google.com/spreadsheets/d/1J8KhzH5UwtVdXEViRASh1SVCpJSbicCpLb6Bedvf4JA/edit?usp=sharing")

grepl("Postleitzahl|Welche Maßname", names(test))

myVectorOfStrings <- c("Wann wurden die Maßnahmen", "Postleitzahl", "Um welche Maßnahme")
matchExpression <- paste(myVectorOfStrings, collapse = "|")
test2 <- test %>% select(matches(matchExpression)) %>% rename(wann=1, plz=2, was=3)

                         