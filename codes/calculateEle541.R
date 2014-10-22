calculateEle541 = function(element541Num, element541Symb,
    element542Num, element543Num, element544Num, element545Num, data){

    finalDemandElements =
        c(element542Num, element543Num, element544Num, element545Num)
    if(any(!finalDemandElements %in% colnames(data)))
        data[, `:=`(finalDemandElements[which(!finalDemandElements %in%
                                              colnames(data))],
                    NA)]
    
    setnames(data,
             old = c(element541Num, element541Symb, element542Num,
                 element543Num, element544Num, element545Num),
             new = c("element541Num", "element541Symb,", "element542Num",
                 "element543Num", "element544Num", "element545Num"))
    
    data[, numberOfMissingElements :=
             numberOfMissingElement(element542Num, element543Num,
                                    element544Num, element545Num)]
    data[, element541Num := element542Num + element543Num +
             element544Num + element545Num]
    data[numberOfMissingElements == 4, element541Num := 0]

    data[!is.na(element541Num) & element541Symb == "M",
         element541Symb := "C"]
    data[element541Num == 0, element541Symb := "M"]    
    data[, numberOfMissingElements := NULL]
    
    setnames(data,
             new = c(element541Num, element541Symb, element542Num,
                 element543Num, element544Num, element545Num),
             old = c("element541Num", "element541Symb,", "element542Num",
                 "element543Num", "element544Num", "element545Num"))    
}
