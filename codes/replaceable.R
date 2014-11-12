##' Returns the index which the values can be replaced
##'
##' Only data with replaceable symbols are replaced by the
##' module. This function will return the index in which the values
##' can be replaced given the corresponding symbol.
##' 
##' @param symb The symbol column correspoinding to the element to be
##' replaced.
##' @param replaceableSymb The original symbol which can be replaced.
##' @export
##' 

replaceable = function(symb, replaceableSymb = c("C", "T", "M")){
    symb %in% replaceableSymb
}
