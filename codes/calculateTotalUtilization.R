##' This function calculates total utilization
##'
##' @param element91Num The column corresponding to value of element
##' 91.
##' @param element95Num The column corresponding to value of element
##' 95.
##' @param element96Num The column corresponding to value of element
##' 96.
##' @param element101Num The column corresponding to value of element
##' 101.
##' @param element111Num The column corresponding to value of element
##' 111.
##' @param element121Num The column corresponding to value of element
##' 121.
##' @param element131Num The column corresponding to value of element
##' 131.
##' @param element141Num The column corresponding to value of element
##' 141.
##' @param element151Num The column corresponding to value of element
##' 151.
##' @param element161Num The column corresponding to value of element
##' 161.
##' @param element546Num The column corresponding to value of element
##' 546.
##' @param itemTypeCol The column which identifies the item type of
##' the commodity item.
##' @param data The data
##' @export
##' 

calculateTotalUtilization = function(element91Num, element95Num,
    element96Num, element101Num, element111Num, element121Num,
    element131Num, element141Num, element151Num, element161Num,
    element546Num, itemTypeCol, data){
    setnames(data,
             old = c(element91Num, element95Num, element96Num,
                 element101Num, element111Num, element121Num,
                 element131Num, element141Num, element161Num,
                 element546Num, element151Num),
             new = c("element91Num", "element95Num", "element96Num",
                 "element101Num", "element111Num", "element121Num",
                 "element131Num", "element141Num", "element161Num",
                 "element546Num", "element151Num"))

    data[!data[[itemTypeCol]] %in% 53,
         TOTAL_UTILIZATION :=
             rowSums(.SD[, list(element91Num, element95Num,
                                element96Num, element101Num,
                                element111Num, element121Num,
                                element131Num, element141Num,
                                element161Num, element546Num)],
                     na.rm = TRUE)]

    data[data[[itemTypeCol]] %in% 53,
         TOTAL_UTILIZATION :=
             rowSums(.SD[, list(element91Num, element95Num,
                                element96Num, element101Num,
                                element111Num + element121Num,
                                element131Num, element141Num,
                                element151Num, element161Num,
                                element546Num)],
                     na.rm = TRUE)]

    setnames(data,
             new = c(element91Num, element95Num, element96Num,
                 element101Num, element111Num, element121Num,
                 element131Num, element141Num, element161Num,
                 element546Num, element151Num),
             old = c("element91Num", "element95Num", "element96Num",
                 "element101Num", "element111Num", "element121Num",
                 "element131Num", "element141Num", "element161Num",
                 "element546Num", "element151Num"))
}
