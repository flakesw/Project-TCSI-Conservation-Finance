#Download GridMET .nc files
options(timeout = max(300, getOption("timeout")))

vars <- c("pet")
years <- 1979:2023

filenames <- expand.grid(vars, "_", years, ".nc",
                         stringsAsFactors = FALSE)%>%
  tidyr::unite(col = url, sep = "")
filenames <- filenames$url

urls <- paste0("http://www.northwestknowledge.net/metdata/data/", filenames) 

destfiles <- paste0("D:/Data/gridmet/", filenames)


for(i in 1:length(urls)){
  download.file(urls[i], destfile = destfiles[i], method = "libcurl", 
                quiet = FALSE, mode = "wb",
                cacheOK = TRUE,
                extra = getOption("download.file.extra"),
                headers = NULL)
}
