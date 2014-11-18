library(faosws)
library(faoswsAupus)
library(faoswsUtil)

## Connection detail to the new working system R API
if(Sys.getenv("USER") == "mk"){
    GetTestEnvironment(
        baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        token = "5d9b8d4a-0989-4b50-869f-cd0bc566fd18"
        )
}

## Get item table
param = getAupusParameter(countryCode = "100", assignGlobal = FALSE)
getAupusDataset(param = param, assignGlobal = TRUE)
