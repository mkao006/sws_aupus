wildCardFill = function(originalData, wildCardData, variable,
    verbose = FALSE, ...){
    if(verbose)
        cat("Number of Miss for vairable", variable, ":",
            sum(is.na(originalData[, variable, with = FALSE])),
            "\n")
    evalText = paste0(variable, " := i.", variable)
    index = unique(wildCardData[originalData[is.na(get(variable)),
        key(wildCardData), with = FALSE], ][!is.na(get(variable)), ])
    setkeyv(index, key(wildCardData))
    okey = key(originalData)
    setkeyv(originalData, key(index))
    originalData[index[!is.na(get(variable)), 
                       c(key(index), variable),
                       with = FALSE],
                 eval(parse(text = evalText))]
    setkeyv(originalData, okey)
    if(verbose)
        cat("Number of Miss for vairable", variable, ":",    
            sum(is.na(originalData[, variable, with = FALSE])),
            "\n")
}
