trendOnce = function(Num, applyIndex = 1:length(Num)){
    tmp = c(NA, Num)
    newTrendIndex = intersect(applyIndex + 1, which(is.na(tmp)))
    tmp[newTrendIndex] = tmp[newTrendIndex - 1]
    trendedOnce = tmp[-1]
    trendedOnce
}
