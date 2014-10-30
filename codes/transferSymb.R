transferSymb = function(symb){
    transferedSymb = symb
    transferedSymb[symb == "*"] = "X"
    transferedSymb[symb %in% c("F", "T")] = "C"
    transferedSymb[!symb %in% c("*", "F", "T")] = "/"
    transferedSymb
}
