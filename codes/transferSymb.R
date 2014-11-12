##' This function converts symbol when the it is transfer to a new
##' cell
##'
##' @param symb The original symbol
##' @export

transferSymb = function(symb){
    transferedSymb = symb
    transferedSymb[symb == "*"] = "X"
    transferedSymb[symb %in% c("F", "T")] = "C"
    transferedSymb[!symb %in% c("*", "F", "T")] = "/"
    transferedSymb
}
