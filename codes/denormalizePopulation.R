##' The function denormalize the population from the row to column.
##'
##' Item 1 is populaion rather than a commodity, and it has two
##' element which corresponds to two different population.
##'
##' The function denormalize the population so it is easier for later
##' calculate of per capita.
##'
##' @param data The data
##' @param element11Num The column corresponding to value of element
##' 11.
##' @param element21Num The column corresponding to value of element 21.
##' @export
##' 

denormalizePopulation = function(data, element11Num, element21Num){
    setnames(data,
             old = c(element11Num, element21Num),
             new = c("element11Num", "element21Num"))
    population =
        data[data[[key(data)[2]]] == 1,
             list(areaCode, Year, element11Num, element21Num)]
    setnames(population,
             old = c("element11Num", "element21Num"),
             new = c("NUM_POP11", "NUM_POP21"))
    setkeyv(population, c("areaCode", "Year"))
    okey = key(data)
    setkeyv(data, key(population))
    data[population, NUM_POP11 := i.NUM_POP11]
    data[population, NUM_POP21 := i.NUM_POP21]
    setkeyv(data, okey)
    setnames(data,
             new = c(element11Num, element21Num),
             old = c("element11Num", "element21Num"))
}
