calculateEle11 = function(element11Num, element11Symb,
    element161Num, element161Symb, data){
    setnames(data,
             old = c(element11Num, element11Symb, element161Num,
                     element161Symb),
             new = c("element11Num", "element11Symb", "element161Num",
                     "element161Symb"))
    ## NOTE (Michael): The item Code list for stock type is contained
    ##                 in appendix A
    data[data[[key(data)[2]]] %in% c(2:10, 13, 19:22, 25:28, 30, 57),
         `:=`(c("element11Num", "element11Symb"),
              trendOnce(Num = element161Num, Symb = element161Symb,
                        transfer = TRUE)),
         by = c(key(data)[2])]
    setnames(data,
             new = c(element11Num, element11Symb, element161Num,
                     element161Symb),
             old = c("element11Num", "element11Symb", "element161Num",
                     "element161Symb"))
}
