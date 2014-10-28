calculateTotalUtilization = function(element91Num, element95Num,
    element96Num, element101Num, element111Num, element121Num,
    element131Num, element141Num, element161Num, element546Num,
    element151Num, data){
    setnames(data,
             old = c(element91Num, element95Num, element96Num,
                 element101Num, element111Num, element121Num,
                 element131Num, element141Num, element161Num,
                 element546Num, element151Num),
             new = c("element91Num", "element95Num", "element96Num",
                 "element101Num", "element111Num", "element121Num",
                 "element131Num", "element141Num", "element161Num",
                 "element546Num", "element151Num"))

    data[!itemType %in% 53,
         TOTAL_UTILIZATION := element91Num + element95Num +
             element96Num + element101Num + element111Num +
             element121Num + element131Num + element141Num +
             element161Num + element546Num]

    data[itemType %in% 53,
         TOTAL_UTILIZATION := element91Num + element95Num +
             element96Num + element101Num + element111Num +
             element121Num + element131Num + element141Num +
             element151Num, element161Num + element546Num]

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
