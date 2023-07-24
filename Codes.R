file.URL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
file.dir <- paste0(getwd(), "/dataset")
file.name <- "data.csv"

download.file(file.URL, file.dir, method = "auto")
unzip(file.dir)
activity <- read.csv(file.name, na.strings = "NA")
file.remove(c(file.dir, file.name))
