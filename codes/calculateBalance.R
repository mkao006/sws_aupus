##' The function will calculate the balance and fill in the element to
##' be balanced.
##'
##' The function calculates the balance based on suppply and
##' utilization and fill in the balancing element.
##'
##' @param supply The column corresponding to calculated total supply.
##' @param utilization The column correspoding to calculated total
##' utilization.
##' @param element161Num The column corresponding to value of element
##' 161.
##' @param element171Num The column corresponding to value of element
##' 171.
##' @param element181Num The column corresponding to value of element
##' 181.
##' @param element181Symb The column corresponding to symbol of element
##' 181.
##' @param balanceElement The column corresponding to the balancing
##' element.
##' @param itemTypeCol The column which identifies the item type of
##' the commodity item.
##' @param data The data
##' @export
##' 

calculateBalance = function(supply, utilization, element161Num,
    element171Num, element181Num, element181Symb, balanceElement,
    itemTypeCol, data){
    ## TODO (Michael): Need to check the replace column of this
    ##                 function.
    setnames(data,
             old = c(supply, utilization, element161Num, element171Num,
                     element181Num, element181Symb, balanceElement),
             new = c("supply", "utilization", "element161Num",
                 "element171Num", "element181Num", "element181Symb",
                 "balanceElement"))
    ## Calculate the temporary value for balance
    data[!data[[itemTypeCol]] %in%
         c(04, 15, 16, 20, 21, 25, 32, 33, 37, 49, 50, 55, 56, 57),
         BALANCE := supply - utilization]
    data[data[[itemTypeCol]] == 57 & !is.na(element161Num) &
         !is.na(element171Num),
         BALANCE := element161Num/element171Num * 1000]
    data[data[[itemTypeCol]] == 57 &
         (is.na(element161Num) | is.na(element171Num)),
         BALANCE := 0]

    ## Reverse the value if balance element is 71.
    data[balanceElement == 71, BALANCE := -BALANCE]

    data[!is.na(balanceElement),
         `:=`(c(paste0("NUM_", .BY[[1]]), paste0("SYMB_", .BY[[1]]),
                "element181Num", "element181Symb", "replaced"),
              fillBalance("BALANCE", paste0("NUM_", .BY[[1]]),
                          paste0("SYMB_", .BY[[1]]),
                          "element181Num", "element181Symb", .SD)),
         by = "balanceElement"]
    replaceIndex1 = which(unlist(data[, replaced]))
    ## data[, replaced := NULL]
    setnames(data,
             new = c(supply, utilization, element161Num, element171Num,
                     element181Num, element181Symb, balanceElement),
             old = c("supply", "utilization", "element161Num",
                 "element171Num", "element181Num", "element181Symb",
                 "balanceElement"))    
    replaceIndex1
}
