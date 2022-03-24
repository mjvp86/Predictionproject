boolPackagesPresent <- function(lop){
  #boolean list of which function in a given list "lop" that is already installed
  #returns a boolen iterand of which functions in "lop" are already present
  packages.present <- (lop %in% installed.packages()[,"Package"]) 
  return(packages.present)
}


installRequiredPackages <- function(lop){
  new.packages <- lop[!boolPackagesPresent(lop)] #note negation - new packages are what is NOT installed yet
  if(length(new.packages))
    install.packages(new.packages) #shortcut to run through and install the list without making explicit loop
  sapply(lop, require, character.only = TRUE) #simple "apply" is the really smart R way to loop over
  return(lop[boolPackagesPresent(lop)])
  #returns elements of "lop" that are now successfully installed
}