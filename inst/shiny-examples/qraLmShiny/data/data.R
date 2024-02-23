#load DR data
load("data/sysdata.rda")

#find model
find.model <- unique(DRParam$Model)
#find population
find.population <- unique(DRParam$Population)
set.seed(100)

# Return the data as a reactive object
return(reactive({
  find
}))
