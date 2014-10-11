
modified.na.locf = function(num, symb, justOnce, ...){
    validNum = num
    validNum[!symb %in% c("T", "C", "M")] = NA
    if(!justOnce){
        trendedNum = na.locf(validNum, na.rm = FALSE, ...)
        trendedNum[!is.na(num)] = num[!is.na(num)]
    } else {
        trendedNum = c(NA, validNum)
        trendedNum[which(is.na(trendedNum))] =
            trendedNum[which(is.na(trendedNum)) - 1]
        trendedNum = trendNum[-1]
    }
    trendedNum
}
