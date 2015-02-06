##' This is the function which performs the full aupus module
##'
##' @param aupusFinalData The final data set returned by the mergeAll
##' function.
##' @param shareData The share data from returned from the function
##' getShareData
##' @param itemTypeCol The column corresponding to the item type of
##' the commodity item.
##' @param shareNum The column corresponding to the share in the share
##' data.
##' @param balanceElementNum The column corresponding to the balance
##' element.
##'
##' @export
##' 

calculateAupusElements = function(aupusFinalData, itemTypeCol, balanceElementNum){
    ## Element 11 - check the symbol, the value seems correct
    replaced11Index =
        calculateEle11(element11Num = "Value_measuredElementFS_11",
                       element11Symb = "flagFaostat_measuredElementFS_11",
                       element161Num = "Value_measuredElementFS_161",
                       element161Symb = "flagFaostat_measuredElementFS_161",
                       itemTypeCol = aupusParam$keyNames$itemTypeName,
                       data = aupusFinalData)
    ## compareFunction(replaced11Index, 11)

    ## Element 21
    replaced21Index =
        calculateEle21(element21Num = "Value_measuredElementFS_21",
                       element21Symb = "flagFaostat_measuredElementFS_21",
                       element11Num = "Value_measuredElementFS_11",
                       element111Num = "Value_measuredElementFS_111",
                       ratio171Num = "Ratio_measuredElementFS_171",
                       itemTypeCol = aupusParam$keyNames$itemTypeName,
                       data = aupusFinalData)
    ## compareFunction(replaced21Index[[1]], 21)
    ## compareFunction(replaced21Index[[2]], 21)

    ## Denormalize population
    ## denormalizePopulation(element11Num = "Value_measuredElementFS_11",
    ##                       element21Num = "Value_measuredElementFS_21",
    ##                       data = aupusFinalData)

    ## Element 31
    ## replaced31Index =
    ##     calculateEle31(element31Num = "Value_measuredElementFS_31",
    ##                    element31Symb = "flagFaostat_measuredElementFS_31",
    ##                    inputNum = inputNum,
    ##                    itemTypeCol = aupusParam$keyNames$itemTypeName,
    ##                    data = aupusFinalData)
    ## compareFunction(replaced31Index[[1]], 31)
    ## compareFunction(replaced31Index[[2]], 31)


    ## Element 41
    replaced41Index =
        calculateEle41(ratio41Num = "Ratio_measuredElementFS_41",
                       element41Num = "Value_measuredElementFS_41",
                       element41Symb = "flagFaostat_measuredElementFS_41",
                       data = aupusFinalData)
    ## compareFunction(replaced41Index, 41)


    ## Element 51
    replaced51Index =
        calculateEle51(element51Num = "Value_measuredElementFS_51",
                       element51Symb = "flagFaostat_measuredElementFS_51",
                       element58Num = "Value_measuredElementFS_58",
                       itemTypeCol = aupusParam$keyNames$itemTypeName,
                       data = aupusFinalData)
    ## compareFunction(replaced51Index, 51)



    ## Element 31, 41, 51 balance
    replaced314151Index =
        calculateEle314151(element31Num = "Value_measuredElementFS_31",
                           element31Symb = "flagFaostat_measuredElementFS_31",
                           element41Num = "Value_measuredElementFS_41",
                           element41Symb = "flagFaostat_measuredElementFS_41",
                           element51Num = "Value_measuredElementFS_51",
                           element51Symb = "flagFaostat_measuredElementFS_51",
                           itemTypeCol = aupusParam$keyNames$itemTypeName,
                           data = aupusFinalData)


    ## Element 58
    ## calculateEle58(element58Num = "Value_measuredElementFS_58", element58Symb = "flagFaostat_measuredElementFS_58",
    ##                itemTypeCol = aupusParam$keyNames$itemTypeName, data = aupusFinalData)



    ## Element 61, 62, 63
    replaced63Index =
        calculateEle63(element61Num = "Value_measuredElementFS_61",
                       element62Num = "Value_measuredElementFS_62", 
                       element63Num = "Value_measuredElementFS_63",
                       element63Symb = "flagFaostat_measuredElementFS_63",
                       data = aupusFinalData)
    ## compareFunction(replaced63Index[[1]], 63)



    ## Element 66
    ## replaced66Index =
    ##     calculateEle66(element41Num = "Value_measuredElementFS_41",
    ##                    element61Num = "Value_measuredElementFS_61",
    ##                    element66Num = "Value_measuredElementFS_66",
    ##                    element66Symb = "flagFaostat_measuredElementFS_66",
    ##                    shares = shareNum, itemTypeCol = aupusParam$keyNames$itemTypeName,
    ##                    data = aupusFinalData, shareData = shareData)
    ## compareFunction(replaced66Index, 66)

    ## Element71
    replaced71Index =
        calculateEle71(element71Num = "Value_measuredElementFS_71",
                       element71Symb = "flagFaostat_measuredElementFS_71",
                       element51Num = "Value_measuredElementFS_51",
                       element61Num = "Value_measuredElementFS_61",
                       element91Num = "Value_measuredElementFS_91",
                       element101Num = "Value_measuredElementFS_101",
                       element121Num = "Value_measuredElementFS_121",
                       element131Num = "Value_measuredElementFS_131",
                       element141Num = "Value_measuredElementFS_141",
                       element151Num = "Value_measuredElementFS_151",
                       element161Num = "Value_measuredElementFS_161",
                       itemTypeCol = aupusParam$keyNames$itemTypeName,
                       data = aupusFinalData)
    ## compareFunction(replaced71Index[[1]], 71)
    ## compareFunction(replaced71Index[[2]], 71)



    ## Element 91, 92, 93
    replaced93Index =
        calculateEle93(element91Num = "Value_measuredElementFS_91",
                       element92Num = "Value_measuredElementFS_92",
                       element93Num = "Value_measuredElementFS_93",
                       element93Symb = "flagFaostat_measuredElementFS_93",
                       itemTypeCol = aupusParam$keyNames$itemTypeName,
                       data = aupusFinalData)
    ## compareFunction(replaced93Index[[1]], 93)
    ## compareFunction(replaced93Index[[2]], 93)


    ## Element 96
    ## calculateEle96(element41Num = "Value_measuredElementFS_41",
    ##                element91Num = "Value_measuredElementFS_91",
    ##                element96Num = "Value_measuredElementFS_96",
    ##                element96Symb = "flagFaostat_measuredElementFS_96",
    ##                shares = shareNum, itemTypeCol = aupusParam$keyNames$itemTypeName,
    ##                data = aupusFinalData, shareData = shareData)


    ## Calculate total supply
    calculateTotalSupply(element11Num = "Value_measuredElementFS_11",
                         element51Num = "Value_measuredElementFS_51",
                         element58Num = "Value_measuredElementFS_58",
                         element61Num = "Value_measuredElementFS_61",
                         element66Num = "Value_measuredElementFS_66",
                         itemTypeCol = aupusParam$keyNames$itemTypeName,
                         data = aupusFinalData)

    ## Elemet 101
    replaced101Index =
        calculateEle101(element101Num = "Value_measuredElementFS_101",
                        element101Symb = "flagFaostat_measuredElementFS_101",
                        ratio101Num = "Ratio_measuredElementFS_101",
                        stotal = "TOTAL_SUPPLY",
                        data = aupusFinalData)
    ## compareFunction(replaced101Index, 101)



    ## Element 111
    replaced111Index =
        calculateEle111(element111Num = "Value_measuredElementFS_111",
                        element111Symb = "flagFaostat_measuredElementFS_111",
                        element21Num = "Value_measuredElementFS_21",
                        element31Num = "Value_measuredElementFS_31",
                        ratio171Num = "Ratio_measuredElementFS_171",
                        ratio111Num = "Ratio_measuredElementFS_111",
                        stotal = "TOTAL_SUPPLY",
                        data = aupusFinalData)
    ## compareFunction(replaced111Index, 111)



    ## Element 121
    replaced121Index =
        calculateEle121(element121Num = "Value_measuredElementFS_121",
                        element121Symb = "flagFaostat_measuredElementFS_121",
                        ratio121Num = "Ratio_measuredElementFS_121",
                        stotal = "TOTAL_SUPPLY",
                        data = aupusFinalData)
    ## compareFunction(replaced121Index, 121)

    ## Element 131
    replaced131Index =
        calculateEle131(element131Num = "Value_measuredElementFS_131",
                        element131Symb = "flagFaostat_measuredElementFS_131",
                        ratio131Num = "Ratio_measuredElementFS_131",
                        stotal = "TOTAL_SUPPLY",
                        data = aupusFinalData)
    ## compareFunction(replaced131Index, 131)


    ## Element 141
    replaced141Index =
        calculateEle141(element141Num = "Value_measuredElementFS_141",
                        element141Symb = "flagFaostat_measuredElementFS_141",
                        element11Num = "Value_measuredElementFS_11",
                        element51Num = "Value_measuredElementFS_51",
                        element61Num = "Value_measuredElementFS_61",
                        element91Num = "Value_measuredElementFS_91",
                        element95Num = "Value_measuredElementFS_95",
                        element161Num = "Value_measuredElementFS_161",
                        ratio141Num = "Ratio_measuredElementFS_141",
                        stotal = "TOTAL_SUPPLY",
                        itemTypeCol = aupusParam$keyNames$itemTypeName,
                        data = aupusFinalData)

    ## compareFunction(replaced141Index[[1]], 141)
    ## compareFunction(replaced141Index[[2]], 141)


    ## Element 144
    replaced144Index =
        calculateEle144(element144Num = "Value_measuredElementFS_144",
                        element144Symb = "flagFaostat_measuredElementFS_144",
                        element141Num = "Value_measuredElementFS_141",
                        population11Num = "Value_population_11",
                        itemTypeCol = aupusParam$keyNames$itemTypeName,
                        data = aupusFinalData)

    ## compareFunction(replaced144Index[[1]], 144)
    ## compareFunction(replaced144Index[[2]], 144)


    ## Element 151
    replaced151Index =
        calculateEle151(element151Num = "Value_measuredElementFS_151",
                        element151Symb = "flagFaostat_measuredElementFS_151",
                        element131Num = "Value_measuredElementFS_131",
                        element51Num = "Value_measuredElementFS_51",
                        ratio151Num = "Ratio_measuredElementFS_151",
                        stotal = "TOTAL_SUPPLY",
                        data = aupusFinalData)

    ## compareFunction(replaced151Index, 151)


    ## Element 161
    replaced161Index =
        calculateEle161(element161Num = "Value_measuredElementFS_161",
                        element161Symb = "flagFaostat_measuredElementFS_161",
                        element11Num = "Value_measuredElementFS_11",
                        element71Num = "Value_measuredElementFS_71",
                        itemTypeCol = aupusParam$keyNames$itemTypeName,
                        data = aupusFinalData)

    ## compareFunction(replaced161Index, 161)


    ## Element 171
    replaced171Index =
        calculateEle171(element171Num = "Value_measuredElementFS_171",
                        element171Symb = "flagFaostat_measuredElementFS_171",
                        element101Num = "Value_measuredElementFS_101",
                        element121Num = "Value_measuredElementFS_121",
                        element131Num = "Value_measuredElementFS_131",
                        element141Num = "Value_measuredElementFS_141",
                        element151Num = "Value_measuredElementFS_151",
                        data = aupusFinalData)

    ## compareFunction(replaced171Index, 171)




    ## Element 174
    replaced174Index =
        calculateEle174(element174Num = "Value_measuredElementFS_174",
                        element174Symb = "flagFaostat_measuredElementFS_174",
                        element171Num = "Value_measuredElementFS_171",
                        population11Num = "Value_population_11",
                        data = aupusFinalData)

    ## compareFunction(replaced174Index, 174)



    ## Element 261
    replaced261Index =
        calculateEle261(element261Num = "Value_measuredElementFS_261",
                        element261Symb = "flagFaostat_measuredElementFS_261",
                        ratio261Num = "Ratio_measuredElementFS_261",
                        element141Num = "Value_measuredElementFS_141",
                        data = aupusFinalData)

    ## compareFunction(replaced261Index, 261)


    ## Element 264
    calculateEle264(element264Num = "Value_measuredElementFS_264",
                    element264Symb = "flagFaostat_measuredElementFS_264",
                    element261Num = "Value_measuredElementFS_261",
                    population11 = "Value_population_11",
                    population21 = "Value_population_21",
                    data = aupusFinalData)

    ## Element 271
    calculateEle271(element271Num = "Value_measuredElementFS_271",
                    element271Symb = "flagFaostat_measuredElementFS_271",
                    ratio271Num = "Ratio_measuredElementFS_271",
                    element141Num = "Value_measuredElementFS_141",
                    data = aupusFinalData)

    ## Element 274
    calculateEle274(element274Num = "Value_measuredElementFS_274",
                    element274Symb = "flagFaostat_measuredElementFS_274",
                    element261Num = "Value_measuredElementFS_261",
                    population11 = "Value_population_11",
                    population21 = "Value_population_21", data = aupusFinalData)

    ## Element 281
    calculateEle281(element281Num = "Value_measuredElementFS_281",
                    element281Symb = "flagFaostat_measuredElementFS_281",
                    ratio281Num = "Ratio_measuredElementFS_281",
                    element141Num = "Value_measuredElementFS_141",
                    data = aupusFinalData)

    ## Element 284
    calculateEle284(element284Num = "Value_measuredElementFS_284",
                    element284Symb = "flagFaostat_measuredElementFS_284",
                    element261Num = "Value_measuredElementFS_261",
                    population11 = "Value_population_11",
                    population21 = "Value_population_21",
                    data = aupusFinalData)


    ## Element 541
    replaced541Index =
        calculateEle541(element541Num = "Value_measuredElementFS_541",
                        element541Symb = "flagFaostat_measuredElementFS_541",
                        element542Num = "Value_measuredElementFS_542",
                        element543Num = "Value_measuredElementFS_543",
                        element544Num = "Value_measuredElementFS_544",
                        element545Num = "Value_measuredElementFS_545",
                        data = aupusFinalData)

    ## compareFunction(replaced541Index[[1]], 541)
    ## compareFunction(replaced541Index[[2]], 541)

    ## Element 546
    replaced546Index =
        calculateEle546(element546Num = "Value_measuredElementFS_546",
                        element546Symb = "flagFaostat_measuredElementFS_546",
                        element541Num = "Value_measuredElementFS_541",
                        element151Num = "Value_measuredElementFS_151",
                        element191Num = "Value_measuredElementFS_191",
                        data = aupusFinalData)

    ## compareFunction(replaced546Index[[1]], 546)
    ## compareFunction(replaced546Index[[2]], 546)

    ## Total utilization
    calculateTotalUtilization(element91Num = "Value_measuredElementFS_91",
                              element95Num = "Value_measuredElementFS_95",
                              element96Num = "Value_measuredElementFS_96",
                              element101Num = "Value_measuredElementFS_101",
                              element111Num = "Value_measuredElementFS_111",
                              element121Num = "Value_measuredElementFS_121",
                              element131Num = "Value_measuredElementFS_131",
                              element141Num = "Value_measuredElementFS_141",
                              element151Num = "Value_measuredElementFS_151",
                              element161Num = "Value_measuredElementFS_161",
                              element546Num = "Value_measuredElementFS_546",
                              itemTypeCol = aupusParam$keyNames$itemTypeName,
                              data = aupusFinalData)
    ## compareFunction(1, "TOTAL_UTILIZATION")
    
    ## Balance
    calculateBalance(supply = "TOTAL_SUPPLY",
                     utilization = "TOTAL_UTILIZATION",
                     element161Num = "Value_measuredElementFS_161",
                     element171Num = "Value_measuredElementFS_171",
                     element181Num = "Value_measuredElementFS_181",
                     element181Symb = "flagFaostat_measuredElementFS_181",
                     balanceElement = aupusParam$keyNames$balanceElementName,
                     itemTypeCol = aupusParam$keyNames$itemTypeName,
                     data = aupusFinalData)
    ## compareFunction(1, "BALANCE")
}
