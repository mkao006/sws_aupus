denormalizePopulation = function(data, elementNum11, elementNum21){
    setnames(data,
             old = c(elementNum11, elementNum21),
             new = c("elementNum11", "elementNum21"))
    population =
        data[itemCode == 1,
             list(areaCode, Year, elementNum11, elementNum21)]
    setnames(population,
             old = c("elementNum11", "elementNum21"),
             new = c("NUM_POP11", "NUM_POP21"))
    setkeyv(population, c("areaCode", "Year"))
    okey = key(data)
    setkeyv(data, key(population))
    data[population, NUM_POP11 := i.NUM_POP11]
    data[population, NUM_POP21 := i.NUM_POP21]
    setkeyv(data, okey)
    setnames(data,
             new = c(elementNum11, elementNum21),
             old = c("elementNum11", "elementNum21"))
}
