library(FAOSTAT)
allCountries =
    sort(na.omit(FAOcountryProfile[FAOcountryProfile$FAOST_CODE < 1000,
                                   "FAOST_CODE"]))

noDataCountry = c(1, 6, 22, 24, 30, 31, 34, 36, 42, 71, 82, 92, 94,
111, 125, 139, 140, 152, 161, 163, 172, 177, 180, 187, 192, 224, 232,
242, 245, 258, 259, 260, 264, 270, 271, 272, 273, 274, 275, 276, 277,
278, 279, 280, 281, 282, 283, 284, 285, 351, 357)

allCountries = allCountries[!allCountries %in% noDataCountry]


failedCountry = c()
failedReason = c()
for(i in allCountries){
    testCountryCode = i
    cat("Running Aupus Module for country:",
        FAOcountryProfile[which(FAOcountryProfile$FAOST_CODE == i),
                          "FAO_TABLE_NAME"], "(", i, ")\n")
    aupus_run = try(
        {
            source("aupus_test.R")
        }
        )
    if(inherits(aupus_run, "try-error")){
        failedCountry = c(failedCountry, i)
        failedReason = c(failedReason, aupus_run)
    }
}



