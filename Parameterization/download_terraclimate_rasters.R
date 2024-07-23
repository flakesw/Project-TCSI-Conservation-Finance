#Download GridMET .nc files
library(tidyverse)
options(timeout = max(300, getOption("timeout")))

vars <- c("pet", "aet")
years <- 1979:2023

filenames <- expand.grid(vars, "_", years, ".nc",
                         stringsAsFactors = FALSE)%>%
  tidyr::unite(col = url, sep = "")
filenames <- filenames$url

urls <- paste0("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_",
                filenames)

destfiles <- paste0("D:/Data/terraclimate/", filenames)


for(i in 1:length(urls)){
  if(!exists(destfiles[i])){
  download.file(urls[i], destfile = destfiles[i], method = "libcurl", 
                quiet = FALSE, mode = "wb",
                cacheOK = TRUE,
                extra = getOption("download.file.extra"),
                headers = NULL)
  }else{
    message(paste0(destfiles[i], "already exists. Skipping download."))
  }
}
