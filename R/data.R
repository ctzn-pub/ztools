library(rio)
library(usethis)
countrydata<- import("/Users/umahuggins/Dropbox/ON/Projects/World/Meta/data/country_meta_use.rds")
use_data(countrydata, overwrite = TRUE)



globalMapData<- import("/Users/umahuggins/Dropbox/ON/Projects/World/visualization/globalMapData.rds")
use_data(globalMapData, overwrite = TRUE)
