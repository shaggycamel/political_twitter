

# Oliver Eaton
# Begun: 2021-06-14
# 
# Rscript to handle package management for political twitter research

# Init message ------------------------------------------------------------

print("Package management...")
print("")

# Package management ------------------------------------------------------

# Install packages
lapply(packages, \(p) if(require(p)) install.packages(p)) |> 
  invisible()

# Load packages
sapply(packages, require, character.only = TRUE)

# Remove packages vector
rm(packages)
print("")