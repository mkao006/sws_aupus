calculateEle11 = function(element11Num, element161Num, data){
    setnames(data,
             old = c(element11Num, element161Num),
             new = c("element11Num", "element161Num"))
    ## The list is contained in appendix A
    data[itemCode %in% c(2:10, 13, 19:22, 25:28, 30, 57),
         element11Num := trendOnce(element161Num),
         by = c("areaCode", "itemCode", "Year")]
    setnames(new = c(element11Num, element161Num),
             old = c("element11Num", "element161Num"))    
}
