local({
  r <- getOption("repos")
  r["CRAN"] <- "https://cloud.r-project.org/"
  options(repos = r)
})

install.packages("tidyverse")
install.packages("lubridate")
install.packages("jsonlite")
install.packages("glue")

