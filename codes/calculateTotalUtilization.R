calculateTotalUtilization = function(element91Num, element95Num,
    element96Num, element101Num, element111Num, element121Num,
    element131Num, element141Num, element161Num, element546Num,
    element151Num, itemTypeCol, data){
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
