##' Function to get item information data
##'
##' The name and type of the commodity corresponding to the item code
##' is returned.
##'
##' @export
##' 

getItemInfoData = function(){
    itemCodeList =
        GetCodeList(domain = "faostat_one",
                    dataset = "FS1_SUA",
                    dimension = "measuredItemFS")
    setnames(itemCodeList,
             old = c("code", "description", "type"),
             new = c("measuredItemFS", "measuredItemNameFS",
                     "measuredItemTypeFS"))
    swsItemTable =
        itemCodeList[, list(measuredItemFS, measuredItemNameFS,
                            measuredItemTypeFS)]
    setkeyv(swsItemTable, "measuredItemFS")
    swsItemTable
}
