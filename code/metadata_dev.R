
devtools::install_github("ropenscilabs/dataspice") # need Rtools. ask IT
library(dataspice)
library(readr)

# create metadata folders
create_spice(dir = "data-raw")
create_spice(dir = "data-processed")

# specify editors/creators
edit_creators(metadata_dir = here::here("data-raw","metadata"))