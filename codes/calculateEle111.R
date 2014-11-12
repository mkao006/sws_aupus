##' The function calculates element 111 (Use for same product)
##'
##' @param element111Num The column corresponding to value of element
##' 111.
##' @param element111Symb The column corresponding to symbol of element
##' 111.
##' @param element21Num The column corresponding to value of element
##' 21.
##' @param element31Num The column corresponding to value of element
##' 31.
##' @param ratio171Num The column corresponding to ratio of element
##' 171.
##' @param ratio111Num The column corresponding to ratio of element
##' 111.
##' @param stotal The column corresponding to the total supply.
##' @param data The data
##' @export
##' 

calculateEle111 = function(element111Num, element111Symb, element21Num,
    element31Num, ratio171Num, ratio111Num, stotal, data){
    if(!ratio111Num %in% colnames(data))
        data[, c(ratio111Num) := as.numeric(NA)]
        
    setnames(data,
             old = c(element111Num, element111Symb, element21Num,
                 element31Num, ratio171Num, ratio111Num, stotal),
             new = c("element111Num", "element111Symb", "element21Num",
                 "element31Num", "ratio171Num", "ratio111Num", "stotal"))

    ## In this case it's the same to calculateEle101
    replaceIndex1 =
        with(data, which(is.na(ratio171Num) & !is.na(ratio111Num)))
    data[replaceIndex1,
         `:=`(c("element111Num", "element111Symb"),
              list(ratio111Num * stotal/100, "C"))]

    yearSearch = function(subData){
        n = NROW(subData)
        newValue = as.vector(rep(NA, n), mode = "numeric")
        newSymb = subData[, element111Symb]
        replaceIndex = which(!is.na(subData[, ratio171Num]) &
                             replaceable(subData[, element111Symb]))
        ele21t1 = c(subData[, element21Num], NA)[replaceIndex + 1]
        ele31t1 = c(subData[, element31Num], NA)[replaceIndex + 1]
        ele21t0 = subData[replaceIndex, element21Num]
        ele31t0 = subData[replaceIndex, element31Num]
        computed.mat = matrix(c(ele21t1, ele31t1, ele21t0, ele31t0),
            nc = 4) * subData[replaceIndex, ratio171Num]
        newValue[replaceIndex] = 
            apply(computed.mat, 1, FUN = function(x) na.omit(x)[1])
        newSymb[replaceIndex] = "C"
        replaced = rep(FALSE, NROW(subData))
        replaced[replaceIndex] = TRUE
        list(newValue, newSymb, replaced)
    }
    data[, c("element111Num", "element111Symb", "replaced") :=
             yearSearch(.SD), by = c(key(data)[2])]
    replaceIndex2 = which(unlist(data[, replaced]))
    data[, replaced := NULL]
    setnames(data,
             new = c(element111Num, element111Symb, element21Num,
                 element31Num, ratio171Num, ratio111Num, stotal),
             old = c("element111Num", "element111Symb", "element21Num",
                 "element31Num", "ratio171Num", "ratio111Num", "stotal"))
    list(replaceIndex1, replaceIndex2)
}    
