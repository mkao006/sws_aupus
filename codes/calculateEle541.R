calculateEle541 = function(element541Num, element541Symb,
    element542Num, element543Num, element544Num, element545Num, data){

    ## If any of these elements were missing, then assign the column
    ## with NA
    finalDemandElements =
        c(element542Num, element543Num, element544Num, element545Num)
    if(any(!finalDemandElements %in% colnames(data)))
        data[, `:=`(finalDemandElements[which(!finalDemandElements %in%
                                              colnames(data))],
                    as.numeric(NA))]
    
    setnames(data,
             old = c(element541Num, element541Symb, element542Num,
                 element543Num, element544Num, element545Num),
             new = c("element541Num", "element541Symb,", "element542Num",
                 "element543Num", "element544Num", "element545Num"))
    
    data[, numberOfMissingElements :=
             numberOfMissingElement(element542Num, element543Num,
                                    element544Num, element545Num)]
    data[, `:=`(c("element541Num", "element541Symb"),
                list(element542Num + element543Num + element544Num +
                     element545Num, "C"))]
    data[numberOfMissingElements == 4,
         `:=`(c("element541Num", "element541Symb"),
              list(NA, "M"))]
    
    data[, numberOfMissingElements := NULL]
    
    setnames(data,
             new = c(element541Num, element541Symb, element542Num,
                 element543Num, element544Num, element545Num),
             old = c("element541Num", "element541Symb,", "element542Num",
                 "element543Num", "element544Num", "element545Num"))    
}
