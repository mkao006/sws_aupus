appendSymbol = function(value, newSymbol, missingSymbol = "M"){
    symb = rep(missingSymbol, length(value))
    symb[which(!is.na(value))] = newSymbol
    list(value, symb)
}
