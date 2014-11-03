fillMissingColumn = function(data, allColumn){
    missingColumn = allColumn[!allColumn %in% colnames(data)]
    columnType = sapply(data[, allColumn[allColumn %in% colnames(data)],
        with = FALSE], typeof)[1]
    if(length(missingColumn) > 0)
        data[, `:=`(c(missingColumn),
                    list(as.vector(NA, mode = columnType)))]
}
