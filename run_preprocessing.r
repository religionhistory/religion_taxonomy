rm(list = ls())

# Load packages and functions
source("./project_support.r")

# Unzip data
unzip("./data/drh.csv.zip", exdir = "./data")

# Clean data
make.dir("./01_clean_data/input")
files <- c("./data/drh.csv", "./data/related_questions.csv", "./data/drh_v6_poll.csv")
file.copy(files, "./01_clean_data/input")
setwd("./01_clean_data/")
source("clean_data.r")
setwd("..")

# Make nexus files
make.dir("./02_make_nexus/input")
files <- c("01_clean_data/output/data_transposed.csv", "./data/drh_v6_poll.csv")
file.copy(files, "./02_make_nexus/input")
setwd("./02_make_nexus/")
source("make_nexus.r")
setwd("..")
