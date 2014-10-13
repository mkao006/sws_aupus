calculateEle101 = function(element101Num, ratio101Num, stotal, data){
    ## Assumes total is calculated already.
    ## NOTE (Michael): how to calculate total supply?
    setnames(data,
             old = c(element101Num, ratio101Num, stotal),
             new = c("element101Num", "ratio101Num", "stotal"))
    data[!is.na(ratio101Num),
         element101Num := ratio101Num * stotal/100]
    setnames(data,
             new = c(element101Num, ratio101Num, stotal),
             old = c("element101Num", "ratio101Num", "stotal"))    
}
