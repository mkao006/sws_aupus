library(faosws)
library(RJSONIO)
library(reshape2)
library(data.table)
library(faoswsUtil)
lapply(dir("../codes/", full.names = TRUE), FUN = source)

## Connection detail to the new working system R API
if(Sys.getenv("USER") == "mk"){
    GetTestEnvironment(
        baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        token = "5d9b8d4a-0989-4b50-869f-cd0bc566fd18"
        )
    attach(as.list(fromJSON("~/connectionDetail.json")))
}


testCountryCode = 100
aupusElements = c(11, 21, 31, 41, 51, 58, 61, 62, 63, 66, 71, 91, 92,
    93, 95, 96, 101, 111, 121, 131, 141, 144, 151, 161, 171, 174, 181,
    191, 261, 264, 271, 274, 281, 284, 541, 546)
testYears = 2005:2012

## Fill in columns which are not available
valueName = paste0("NUM_", aupusElements)
symbName = paste0("SYMB_", aupusElements)
ratioName = paste0("RATIO_", aupusElements)

itemCodeList =
    GetCodeList(domain = "faostat_one",
                dataset = "FS1_SUA",
                dimension = "measuredItemFS")

elementCodeList =
    GetCodeList(domain = "faostat_one",
                dataset = "FS1_SUA",
                dimension = "measuredElementFS")

## Get aupus data
## ---------------------------------------------------------------------
aupusDimension =
    list(Dimension(name = "geographicAreaFS",
                   keys = as.character(testCountryCode)),
         Dimension(name = "measuredItemFS",
                   keys = itemCodeList[type != 0, code]),
         Dimension(name = "timePointYears",
                   keys = as.character(testYears)),
         Dimension(name = "measuredElementFS",
                   keys =
                       as.character(intersect(aupusElements,
                                              elementCodeList$code))))

aupusDataContext =
    DatasetKey(domain = "faostat_one",
               dataset = "FS1_SUA",
               dimensions = aupusDimension)

aupusPivot = c(
    Pivoting(code = "geographicAreaFS", ascending = TRUE),
    Pivoting(code = "measuredItemFS", ascending = TRUE),
    Pivoting(code = "timePointYears", ascending = FALSE),
    Pivoting(code = "measuredElementFS", ascending = TRUE)
)

aupus =
    GetData(key = aupusDataContext, flags = TRUE, normalized = FALSE,
            pivoting = aupusPivot)

## Convert list of NULL to vector of NA
sapply(names(which(sapply(aupus, typeof) == "list")),
       FUN = function(x){
           aupus[,
                 eval(parse(text = paste0(x, " := as.character(NULLtoNA(", x, "))")))
                 ]
           invisible(NULL)
       })
sapply(names(which(sapply(aupus, typeof) == "logical")),
       FUN = function(x){
           aupus[, eval(parse(text =
                                  paste0(x, " := as.numeric(", x, ")")))]
           invisible(NULL)
       })


## Temporary solution
setnames(aupus,
         old = c("geographicAreaFS", "measuredItemFS", "timePointYears"),
         new = c("areaCode", "itemCode", "Year"))
setnames(aupus,
         old = grep("Value", colnames(aupus), value = TRUE),
         new = paste0("NUM_",
             gsub("[^0-9]", "",
                  grep("Value", colnames(aupus), value = TRUE))))
setnames(aupus,
         old = grep("flag", colnames(aupus), value = TRUE),
         new = paste0("SYMB_",
             gsub("[^0-9]", "",
                  grep("flag", colnames(aupus), value = TRUE))))

aupusKey = c("areaCode", "itemCode", "Year")
aupus[, `:=`(c(aupusKey),
             lapply(aupus[, aupusKey, with = FALSE],
                    as.numeric))]
setkeyv(aupus, cols = aupusKey)
fillMissingColumn(aupus, valueName)
fillMissingColumn(aupus, symbName)

## setnames(aupus, "timePointYears", "timePointYearsSP")
## aupusKey = c("geographicAreaFS", "measuredItemFS", "timePointYearsSP")
## aupus[, `:=`(c(aupusKey),
##              lapply(aupus[, aupusKey, with = FALSE],
##                     as.numeric))]
## setkeyv(aupus, cols = aupusKey)


## Get input from processing data
## ---------------------------------------------------------------------
inputDimension =
    list(Dimension(name = "geographicAreaFS",
                   keys = as.character(testCountryCode)),
         Dimension(name = "measuredItemParentFS",
                   keys = itemCodeList[type != 0, code]),
         Dimension(name = "measuredItemChildFS",
                   keys = itemCodeList[type != 0, code]),
         Dimension(name = "timePointYearsSP",
                   keys = as.character(testYears)))

inputDataContext =
    DatasetKey(domain = "faostat_one",
               dataset = "input_from_proc_fs",
               dimensions = inputDimension)

inputPivot = c(
    Pivoting(code = "geographicAreaFS", ascending = TRUE),
    Pivoting(code = "measuredItemParentFS", ascending = TRUE),
    Pivoting(code = "measuredItemChildFS", ascending = TRUE),    
    Pivoting(code = "timePointYearsSP", ascending = FALSE)
)

input =
    GetData(key = inputDataContext, flags = TRUE,
            normalized = TRUE, pivoting = inputPivot)

## Temporary solution
setnames(input,
         old = c("geographicAreaFS", "measuredItemParentFS",
             "measuredItemChildFS", "timePointYearsSP", "Value",
             "flagFaostat"),
         new = c("areaCode", "itemParentCode", "itemCode", "Year",
                 "NUM_INPUT", "SYMB_INPUT"))

inputKey = c("areaCode", "itemParentCode", "itemCode", "Year")
input[, `:=`(c(inputKey),
             lapply(input[, inputKey, with = FALSE],
                    as.numeric))]
setkeyv(input, cols = inputKey)

## setnames(input,
##          old = c("Value", "flagFaostat"),
##          new = c("inpute_value", "input_flag"))

## ## NOTE (Michael): Why does the year has 'SP' suffix?
## inputKey = c("geographicAreaFS", "measuredItemParentFS",
##             "measuredItemChildFS", "timePointYearsSP")
## input[, `:=`(inputKey,
##              lapply(input[, inputKey, with = FALSE],
##                     as.numeric))]
## setkeyv(input, cols = inputKey)

## Get ratio data
## ---------------------------------------------------------------------

ratioDimension =
    list(Dimension(name = "geographicAreaFS",
                   keys = as.character(c(0, testCountryCode))),
         Dimension(name = "measuredItemFS",
                   keys = itemCodeList[type != 0, code]),
         Dimension(name = "timePointYearsSP",
                   keys = as.character(c(0, testYears))),
         Dimension(name = "measuredElementFS",
                   keys =
                       as.character(intersect(aupusElements,
                                              elementCodeList$code))))

ratioDataContext =
    DatasetKey(domain = "faostat_one",
               dataset = "aupus_ratio_fs",
               dimensions = ratioDimension)

ratioPivot = c(
    Pivoting(code = "geographicAreaFS", ascending = TRUE),
    Pivoting(code = "measuredItemFS", ascending = TRUE),
    Pivoting(code = "timePointYearsSP", ascending = FALSE),
    Pivoting(code = "measuredElementFS", ascending = TRUE)
)

ratioFull =
    GetData(key = ratioDataContext, flags = TRUE, normalized = FALSE,
            pivoting = ratioPivot)

## Remove the symbol since it is not used
ratioFull[, `:=`(c(grep("flag", colnames(ratioFull))), NULL)]


sapply(names(which(sapply(ratioFull, typeof) == "logical")),
       FUN = function(x){
           ratioFull[, eval(parse(text =
                                  paste0(x, " := as.numeric(", x, ")")))]
           invisible(NULL)
       })


## Temporary solution
setnames(ratioFull,
         old = c("geographicAreaFS", "measuredItemFS",
             "timePointYearsSP"),
         new = c("areaCode", "itemCode", "Year"))
setnames(ratioFull,
         old = grep("Value", colnames(ratioFull), value = TRUE),
         new = paste0("RATIO_",
             gsub("[^0-9]", "",
                  grep("Value", colnames(ratioFull), value = TRUE))))
fillMissingColumn(ratioFull, ratioName)
ratioFullKeys = c("areaCode", "itemCode", "Year")
ratioFull[, `:=`(c(ratioFullKeys),
             lapply(ratioFull[, ratioFullKeys, with = FALSE], as.numeric))]
setkeyv(ratioFull, cols = ratioFullKeys)

specific = ratioFull[areaCode != 0 & Year != 0, ]
setkeyv(specific, c("areaCode", "itemCode", "Year"))
yearWildCard = ratioFull[areaCode != 0 & Year == 0,
    !"Year", with = FALSE]
setkeyv(yearWildCard, c("areaCode", "itemCode"))
areaYearWildCard = ratioFull[areaCode == 0 & Year == 0,
    !c("areaCode", "Year"), with = FALSE]
setkeyv(areaYearWildCard, "itemCode")

ratio = list(specific = specific, 
    yearWildCard = yearWildCard,
    areaYearWildCard = areaYearWildCard)



## setnames(ratio,
##          old = grep("Value", colnames(ratio), value = TRUE),
##          new = gsub("Value", "ratio",
##              grep("Value", colnames(ratio), value = TRUE)))
## ratioKeys = c("geographicAreaFS", "measuredItemFS", "timePointYearsSP")
## ratio[, `:=`(c(ratioKeys),
##              lapply(ratio[, ratioKeys, with = FALSE], as.numeric))]
## setkeyv(ratio, cols = ratioKeys)



## Get share data
## ---------------------------------------------------------------------

shareDimension =
    list(Dimension(name = "geographicAreaFS",
                   keys = as.character(c(0, testCountryCode))),
         Dimension(name = "measuredItemParentFS",
                   keys = itemCodeList[type != 0, code]),
         Dimension(name = "measuredItemChildFS",
                   keys = itemCodeList[type != 0, code]),
         Dimension(name = "timePointYearsSP",
                   keys = as.character(c(0, testYears))))

shareDataContext =
    DatasetKey(domain = "faostat_one",
               dataset = "aupus_share_fs",
               dimensions = shareDimension)

sharePivot = c(
    Pivoting(code = "geographicAreaFS", ascending = TRUE),
    Pivoting(code = "measuredItemParentFS", ascending = TRUE),
    Pivoting(code = "measuredItemChildFS", ascending = TRUE),    
    Pivoting(code = "timePointYearsSP", ascending = FALSE)
)

shareFull =
    GetData(key = shareDataContext, flags = TRUE,
            normalized = TRUE, pivoting = sharePivot)
shareFull[, flagShare := NULL]

## Temporary solution
setnames(shareFull,
         old = c("geographicAreaFS", "measuredItemParentFS",
             "measuredItemChildFS", "timePointYearsSP", "Value"),
         new = c("areaCode", "itemCode", "itemChildCode", "Year",
                 "SHARE"))

shareFullKey = c("areaCode", "itemCode", "itemChildCode", "Year")
shareFull[, `:=`(c(shareFullKey),
             lapply(shareFull[, shareFullKey, with = FALSE],
                    as.numeric))]
setkeyv(shareFull, cols = shareFullKey)

specific = shareFull[areaCode != 0 & Year != 0, ]
setkeyv(specific, c("areaCode", "itemCode", "itemChildCode", "Year"))
yearWildCard = shareFull[areaCode != 0 & Year == 0,
    !"Year", with = FALSE]
setkeyv(yearWildCard, c("areaCode", "itemCode", "itemChildCode"))
areaYearWildCard = shareFull[areaCode == 0 & Year == 0,
    !c("areaCode", "Year"), with = FALSE]
setkeyv(areaYearWildCard, c("itemCode", "itemChildCode"))
share =
    mergeShare(list(specific = specific, 
                    yearWildCard = yearWildCard,
                    areaYearWildCard = areaYearWildCard),
               aupus = aupus)


## setnames(share, c("Value", "flagShare"), c("share_value", "share_flag"))
## shareKeys = c("geographicAreaFS", "measuredItemParentFS",
##                  "measuredItemChildFS", "timePointYearsSP")
## share[, `:=`(c(shareKeys),
##              lapply(share[, shareKeys, with = FALSE], as.numeric))]
## setkeyv(share, shareKeys)

## Get balancing item
##
## Note (Michael): This is just the flag of the 'ratio' table
## ---------------------------------------------------------------------


balanceElementAll =
    GetData(key = ratioDataContext, flags = TRUE, normalized = TRUE,
            pivoting = ratioPivot)

balanceElementFull = balanceElementAll[flagRatio == "Y",
    list(geographicAreaFS, measuredItemFS, measuredElementFS,
         timePointYearsSP)]
setnames(balanceElementFull,
         old = c("geographicAreaFS", "measuredItemFS",
             "measuredElementFS", "timePointYearsSP"),
         new = c("areaCode", "itemCode", "balanceElement", "Year"))

tmp = lapply(balanceElementFull[, colnames(balanceElementFull),
    with = FALSE], as.numeric)
balanceElementFull[, `:=`(c(colnames(balanceElementFull)), tmp)]
balanceElementFullKey = c("areaCode", "itemCode", "Year")
setkeyv(balanceElementFull, balanceElementFullKey)

specific = balanceElementFull[areaCode != 0 & Year != 0, ]
setkeyv(specific, c("areaCode", "itemCode", "Year"))
yearWildCard = balanceElementFull[areaCode != 0 & Year == 0,
    !"Year", with = FALSE]
setkeyv(yearWildCard, c("areaCode", "itemCode"))
areaYearWildCard = balanceElementFull[areaCode == 0 & Year == 0,
    !c("areaCode", "Year"), with = FALSE]
setkeyv(areaYearWildCard, "itemCode")

balanceElement = list(specific = specific, 
    yearWildCard = yearWildCard,
    areaYearWildCard = areaYearWildCard)

## setnames(balanceElement, "measuredElementFS", "balanceElement")
## setkeyv(balanceElement,
##         c("geographicAreaFS", "measuredItemFS", "timePointYearsSP"))
