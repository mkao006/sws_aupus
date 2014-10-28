calculateEle111 = function(element111Num, element111Symb, element21Num,
    element31Num, ratio171Num, ratio111Num, stotal, data){
    if(!ratio111Num %in% colnames(data))
        data[, c(ratio111Num) := NA]
        
    setnames(data,
             old = c(element111Num, element111Symb, element21Num,
                 element31Num, ratio171Num, ratio111Num, stotal),
             new = c("element111Num", "element111Symb", "element21Num",
                 "element31Num", "ratio171Num", "ratio111Num", "stotal"))

    ## In this case it's the same to calculateEle101
    data[is.na(ratio171Num) & !is.na(ratio111Num),
         element111Num := ratio111Num * stotal/100]

    foo = function(subData){
        n = NROW(subData)
        newValue = vector("numeric", n)
        newSymb = subData[, element111Symb]
        ratio171Available = which(!is.na(subData[, ratio171Num]))
        ele21t1 = c(subData[, element21Num], NA)[ratio171Available + 1]
        ele31t1 = c(subData[, element31Num], NA)[ratio171Available + 1]
        ele21t0 = subData[ratio171Available, element21Num]
        ele31t0 = subData[ratio171Available, element31Num]
        computed.mat = matrix(c(ele21t1, ele31t1, ele21t0, ele31t0),
            nc = 4) * subData[ratio171Available, ratio171Num]
        newValue[ratio171Available] = 
            apply(computed.mat, 1, FUN = function(x) na.omit(x)[1])
        newSymb[ratio171Available] = "C"
        list(newValue, newSymb)
    }
    data[, c("element111Num", "element111Symb") := foo(.SD),
         by = "itemCode"]
    setnames(data,
             new = c(element111Num, element111Symb, element21Num,
                 element31Num, ratio171Num, ratio111Num, stotal),
             old = c("element111Num", "element111Symb", "element21Num",
                 "element31Num", "ratio171Num", "ratio111Num", "stotal"))
}    