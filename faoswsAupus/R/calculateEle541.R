##' This function calculates element 541 (final demand)
##'
##' @param element541Num The column corresponding to value of element
##' 541.
##' @param element541Symb The column corresponding to symbol of element
##' 541.
##' @param element542Num The column corresponding to value of element
##' 542.
##' @param element543Num The column corresponding to value of element
##' 543.
##' @param element544Num The column corresponding to value of element
##' 544.
##' @param element545Num The column corresponding to value of element
##' 545.
##' @param data The data
##' @export
##' 

calculateEle541 = function(element541Num, element541Symb,
    element542Num, element543Num, element544Num, element545Num, data){

    ## If any of these elements were missing, then assign the column
    ## with NA
    finalDemandElements =
        c(element542Num, element543Num, element544Num, element545Num)
    if(any(!finalDemandElements %in% colnames(data)))
        data[, `:=`(finalDemandElements[which(!finalDemandElements %in%
                                              colnames(data))],
                    as.numeric(NA))]
    
    setnames(data,
             old = c(element541Num, element541Symb, element542Num,
                 element543Num, element544Num, element545Num),
             new = c("element541Num", "element541Symb", "element542Num",
                 "element543Num", "element544Num", "element545Num"))
    
    data[, numberOfMissingElements :=
             numberOfMissingElement(element542Num, element543Num,
                                    element544Num, element545Num)]
    replaceIndex1 = with(data, which(replaceable(element541Symb)))
    data[replaceIndex1,
         `:=`(c("element541Num", "element541Symb"),
              appendSymbol(rowSums(.SD[, list(element542Num,
                                              element543Num,
                                              element544Num,
                                              element545Num)],
                                   na.rm = TRUE), "C"))]
    replaceIndex2 = with(data, which(numberOfMissingElements == 4 &
                                    replaceable(element541Symb)))
    data[replaceIndex2,
         `:=`(c("element541Num", "element541Symb"), list(NA, "M"))]
    
    data[, numberOfMissingElements := NULL]
    
    setnames(data,
             new = c(element541Num, element541Symb, element542Num,
                 element543Num, element544Num, element545Num),
             old = c("element541Num", "element541Symb", "element542Num",
                 "element543Num", "element544Num", "element545Num"))
    list(replaceIndex1, replaceIndex2)
}
