calculateTrend = function(element, elementNum, elementSymb, data){
    setnames(x = data, old = c(elementNum, elementSymb),
             new = c("elementNum", "elementSymb"))
    if(!element %in% c(31, 41, 51)){
        data[itemCode %in% c(0:1299, 1455:1700),
             elementNum :=
                 modified.na.locf(elementNum, elementSymb, FALSE),
             by = c("areaCode", "itemCode")]
    } else if(element %in% c(31, 41, 51)){
        data[itemCode %in% c(0:1299, 1455:1700),
             elementNum :=
                 modified.na.locf(elementNum, elementSymb, TRUE),
             by = c("areaCode", "itemCode")]
    } else if(element == 71){
        data[itemCode %in% c(12, 13),
             elementNum :=
                 modified.na.locf(elementNum, elementSymb, FALSE),
             by = c("areaCode", "itemCode")]        
    }
    setnames(x = data, new = c(elementNum, elementSymb),
             old = c("elementNum", "elementSymb"))
}
