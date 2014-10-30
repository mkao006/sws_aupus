calculateEle11 = function(element11Num, element11Symb,
    element161Num, element161Symb, data){
    setnames(data,
             old = c(element11Num, element11Symb, element161Num,
                     element161Symb),
             new = c("element11Num", "element11Symb", "element161Num",
                     "element161Symb"))
    ## The list is contained in appendix A
    data[itemCode %in% c(2:10, 13, 19:22, 25:28, 30, 57),
         `:=`(c("element11Num", "element11Symb"),
              trendOnce(Num = element161Num, Symb = element161Symb,
                        transfer = TRUE)),
         by = "itemCode"]
    setnames(data,
             new = c(element11Num, element11Symb, element161Num,
                     element161Symb),
             old = c("element11Num", "element11Symb", "element161Num",
                     "element161Symb"))
}
