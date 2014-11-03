library(faosws)
library(RJSONIO)
library(reshape2)
library(data.table)
library(faoswsUtil)
## lapply(dir("../codes/", full.names = TRUE), FUN = source)

## Connection detail to the new working system R API
if(Sys.getenv("USER") == "mk"){
    GetTestEnvironment(
        baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        token = "5d9b8d4a-0989-4b50-869f-cd0bc566fd18"
        )
    attach(as.list(fromJSON("~/connectionDetail.json")))
}


testCountryCode = c(100, 0)
aupusElements = c(11, 21, 31, 41, 51, 58, 61, 62, 66, 71, 91, 92, 95,
    96, 101, 111, 121, 131, 141, 144, 151, 161, 171, 174, 261, 274, 281,
    284, 541, 546)
testYears = 2005:2012

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
sapply(names(sapply(aupus, typeof) == "list"),
       FUN = function(x){
           aupus[,
                 eval(parse(text = paste0(x, " := NULLtoNA(", x, ")")))
                 ]
           invisible(NULL)
       })



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


## NOTE (Michael): Why does the year has 'SP' suffix?


## Get ratio data
## ---------------------------------------------------------------------

ratioDimension =
    list(Dimension(name = "geographicAreaFS",
                   keys = as.character(testCountryCode)),
         Dimension(name = "measuredItemFS",
                   keys = itemCodeList[type != 0, code]),
         Dimension(name = "timePointYearsSP",
                   keys = as.character(testYears)),
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

ratio =
    GetData(key = ratioDataContext, flags = TRUE, normalized = FALSE,
            pivoting = ratioPivot)

## Convert list of NULL to vector of NA
sapply(names(sapply(ratio, typeof) == "list"),
       FUN = function(x){
           ratio[,
                 eval(parse(text = paste0(x, " := NULLtoNA(", x, ")")))
                 ]
           invisible(NULL)
       })



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
                   keys = as.character(testYears)))

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

share =
    GetData(key = shareDataContext, flags = TRUE,
            normalized = TRUE, pivoting = sharePivot)

## Get balancing item
##
## Note (Michael): This is just the flag of the 'ratio' table
## ---------------------------------------------------------------------


balanceElementAll =
    GetData(key = ratioDataContext, flags = TRUE, normalized = TRUE,
            pivoting = ratioPivot)

balanceElement = balanceElementAll[flagRatio == "Y",
    list(geographicAreaFS, measuredItemFS, measuredElementFS,
         timePointYearsSP)]
