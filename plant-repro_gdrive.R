## ------------------------------------------ ##
  # Plant Repro. - Google Drive Interactions
## ------------------------------------------ ##
# Written by: Nick J Lyon

# PURPOSE
## This script is ONLY for members of the LTER Plant Reproduction working group
## It _requires_ access to their Shared Google Drive and is meant as a convenience for down/uploading data from/to the various online data storage locations

## ------------------------------------------ ##
# Housekeeping ----
## ------------------------------------------ ##
# Load libraries
# install.packages("librarian")
librarian::shelf(googledrive, tidyverse)

# Authenticate Google Drive
googledrive::drive_auth()


# End ----

