
calculateEle541 = function(element542Num, element543Num, element544Num,
    element545Num, data){
    setnames(data,
             old = c(element542Num, element543Num, element544Num,
                 element545Num),
             new = c("element542Num", "element543Num", "element544Num",
                 "element545Num"))
    data[, numberOfMissingElements :=
             numberOfMissingElement(element542Num, element543Num,
                                    element544Num, element545Num)]
    data[, element541Num := element542Num + element543Num +
             element544Num + element545Num]
    data[numberOfMissingElements == 0, element541Num := 0]
    data[, numberOfMissingElements := NULL]
    setnames(data,
             new = c(element542Num, element543Num, element544Num,
                 element545Num),
             old = c("element542Num", "element543Num", "element544Num",
                 "element545Num"))    
}
