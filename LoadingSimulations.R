# Plots 

load("simulation_setups/multi_carve/Binomial_02-Mrz-2023 15.48/results 02-Mrz-2023 15.51 split=0.5 B=1 seed=3588.RData")

load("simulation_setups/multi_carve/Binomial_02-Mrz-2023 15.48/results 02-Mrz-2023 15.51 split=0.5 B=50 seed=3588.RData")


temp <- list.files(path ="C:/Users/viki1/Documents/Uni/Thesis/Thesis/simulation_setups/multi_carve/Binomial_02-Mrz-2023 15.48", pattern="*.RData")
  i <- 1
for(each in temp){
  load(each)
  eval(parse(text=paste(paste0("simulation",i),"<- simulation")))
  i <- i+1
}





#How to name the files differently from each other

loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}
d <- loadRData("simulation_setups/multi_carve/Binomial_02-Mrz-2023 15.48/results 02-Mrz-2023 15.51 split=0.5 B=1 seed=3588.RData")








#Trying to load all that are in one folder. Not working so far

# first create custom names for objects (e.g. add folder names)
files <- "simulation_setups/multi_carve/Binomial_02-Mrz-2023 15.48/"
file_names <- gsub("/", "_", files)
file_names <- gsub("\\.Rda", "", file_names)

# function to load objects in new environ
load_obj <- function(f, f_name) {
  env <- new.env()
  nm <- load(f, env)[1]  # load into new environ and capture name
  assign(f_name, env[[nm]], pos = 1) # pos 1 is parent env
}

# load all
mapply(load_obj, files, file_names)


