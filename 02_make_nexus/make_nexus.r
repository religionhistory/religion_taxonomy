# Make nexus files and dictionaries for each stage of analysis

rm(list = ls())

source("../project_support.r")

# Load data
raw_data <- read_csv("./input/data_transposed.csv")
questions <- read_csv("./input/drh_v6_poll.csv") 

# Create output directory
make.dir("./output")

# For GLRM to find how the value used for k was found run the following script	
# source("glrm_k_value.r")	

# initate h2o
h2o.init(nthreads = -1)

# Make nexus files with corresponding dictionaries and data
make_nexus_dict(raw_data, analysis = "b", granularity = "f", var_filter = 0.5, entry_filter = 0.5, k = 47)

# Shutdown H2O    
h2o.shutdown(prompt=FALSE)
