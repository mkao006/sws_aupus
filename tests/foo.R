foo = function(aupusFinal, share){
    ## Element 11
    calculateEle11(element11Num = "NUM_11", element11Symb = "SYMB_11",
                   element161Num = "NUM_161", element161Symb = "SYMB_161",
                   data = aupusFinal)

    ## Element 21
    calculateEle21(element21Num = "NUM_21", element21Symb = "SYMB_21",
                   element11Num = "NUM_11", element111Num = "NUM_111",
                   ratio171Num = "RATIO_171", itemTypeCol = "itemType",
                   data = aupusFinal)

    ## Denormalize population
    denormalizePopulation(elementNum11 = "NUM_11", elementNum21 = "NUM_21",
                          data = aupusFinal)


    ## Element 31
    calculateEle31(element31Num = "NUM_31", element31Symb = "SYMB_31",
                   inputNum = "NUM_TOTAL_INPUT", itemTypeCol = "itemType",
                   data = aupusFinal)

    ## Element 41
    calculateEle41(ratio41Num = "RATIO_41",
                   element41Num = "NUM_41", element41Symb = "SYMB_41",
                   data = aupusFinal)

    ## Element 51
    calculateEle51(element51Num = "NUM_51", element51Symb = "SYMB_51",
                   element58Num = "NUM_58", itemTypeCol = "itemType",
                   data = aupusFinal)


    ## Element 31, 41, 51 balance
    calculateEle314151(element31Num = "NUM_31", element31Symb = "SYMB_31",
                       element41Num = "NUM_41", element41Symb = "SYMB_41",
                       element51Num = "NUM_51", element51Symb = "SYMB_51",
                       itemTypeCol = "itemType", data = aupusFinal)

    ## Element 58
    calculateEle58(element58Num = "NUM_58", element58Symb = "SYMB_58",
                   itemTypeCol = "itemType", data = aupusFinal)


    ## Element 61, 62, 63
    calculateEle63(element61Num = "NUM_61",  element62Num = "NUM_62", 
                   element63Num = "NUM_63", element63Symb = "SYMB_63",
                   data = aupusFinal)



    ## Element 66
    calculateEle66(element41Num = "NUM_41", element61Num = "NUM_61",
                   element66Num = "NUM_66", element66Symb = "SYMB_66",
                   itemTypeCol = "itemType", data = aupusFinal,
                   share = share)

    ## Element71
    calculateEle71(element71Num = "NUM_71", element71Symb = "SYMB_71",
                   element51Num = "NUM_51", element61Num = "NUM_61",
                   element91Num = "NUM_91", element101Num = "NUM_101",
                   element121Num = "NUM_121", element131Num = "NUM_131",
                   element141Num = "NUM_141", element151Num = "NUM_151",
                   element161Num = "NUM_161", itemTypeCol = "itemType",
                   data = aupusFinal)


    ## Element 91, 92, 93
    calculateEle93(element91Num = "NUM_91", element92Num = "NUM_92",
                   element93Num = "NUM_93", element93Symb = "SYMB_93",
                   data = aupusFinal)

    ## Element 96
    calculateEle96(element41Num = "NUM_41", element91Num = "NUM_91",
                   element96Num = "NUM_96", element96Symb = "SYMB_96",
                   data = aupusFinal, itemTypeCol = "itemType",
                   share = share)


    ## Calculate total supply
    calculateTotalSupply(element11Num = "NUM_11", element51Num = "NUM_51",
                         element58Num = "NUM_58", element61Num = "NUM_61",
                         element66Num = "NUM_66", itemTypeCol = "itemType",
                         data = aupusFinal)

    ## Elemet 101
    calculateEle101(element101Num = "NUM_101", element101Symb = "SYMB_101",
                    ratio101Num = "RATIO_101", stotal = "TOTAL_SUPPLY",
                    data = aupusFinal)

    ## Element 111
    calculateEle111(element111Num = "NUM_111", element111Symb = "SYMB_111",
                    element21Num = "NUM_21", element31Num = "NUM_31",
                    ratio171Num = "RATIO_171", ratio111Num = "RATIO_111",
                    stotal = "TOTAL_SUPPLY", data = aupusFinal)


    ## Element 121
    calculateEle121(element121Num = "NUM_121", element121Symb = "SYMB_121",
                    ratio121Num = "RATIO_121", stotal = "TOTAL_SUPPLY",
                    data = aupusFinal)

    ## Element 131
    calculateEle131(element131Num = "NUM_131", element131Symb = "SYMB_131",
                    ratio131Num = "RATIO_131", stotal = "TOTAL_SUPPLY",
                    data = aupusFinal)


    ## Element 141
    calculateEle141(element141Num = "NUM_141", element141Symb = "SYMB_141",
                    element11Num = "NUM_11", element51Num = "NUM_51",
                    element61Num = "NUM_61", element91Num = "NUM_91",
                    element95Num = "NUM_95", element161Num = "NUM_161",
                    ratio141Num = "RATIO_141", stotal = "TOTAL_SUPPLY",
                    itemTypeCol = "itemType", data = aupusFinal)

    ## Element 144
    calculateEle144(element144Num = "NUM_144", element144Symb = "SYMB_144",
                    element141Num = "NUM_141", population = "NUM_POP11",
                    data = aupusFinal)

    ## Element 151
    calculateEle151(element151Num = "NUM_151", element151Symb = "SYMB_151",
                    element131Num = "NUM_131", element51Num = "NUM_51",
                    ratio151Num = "RATIO_151", stotal = "TOTAL_SUPPLY",
                    data = aupusFinal)

    ## Element 161
    calculateEle161(element161Num = "NUM_161", element161Symb = "SYMB_161",
                    element11Num = "NUM_11", element71Num = "NUM_71",
                    itemTypeCol = "itemType", data = aupusFinal)

    ## Element 171
    calculateEle171(element171Num = "NUM_161", element171Symb = "SYMB_171",
                    element101Num = "NUM_101", element121Num = "NUM_121",
                    element131Num = "NUM_131", element141Num = "NUM_141",
                    element151Num = "NUM_151", data = aupusFinal)


    ## Element 174
    calculateEle174(element174Num = "NUM_174", element174Symb = "SYMB_174",
                    element171Num = "NUM_171", population = "NUM_POP11",
                    data = aupusFinal)


    ## Element 261
    calculateEle261(element261Num = "NUM_261", element261Symb = "SYMB_261",
                    ratio261Num = "RATIO_261", element141Num = "NUM_141",
                    data = aupusFinal)

    ## Element 264
    calculateEle264(element264Num = "NUM_264", element264Symb = "SYMB_264",
                    element261Num = "NUM_261", population11 = "NUM_POP11",
                    population21 = "NUM_POP21", data = aupusFinal)

    ## Element 271
    calculateEle271(element271Num = "NUM_271", element271Symb = "SYMB_271",
                    ratio271Num = "RATIO_271", element141Num = "NUM_141",
                    data = aupusFinal)

    ## Element 274
    calculateEle274(element274Num = "NUM_274", element274Symb = "SYMB_274",
                    element261Num = "NUM_261", population11 = "NUM_POP11",
                    population21 = "NUM_POP21", data = aupusFinal)

    ## Element 281
    calculateEle281(element281Num = "NUM_281", element281Symb = "SYMB_281",
                    ratio281Num = "RATIO_281", element141Num = "NUM_141",
                    data = aupusFinal)

    ## Element 284
    calculateEle284(element284Num = "NUM_284", element284Symb = "SYMB_284",
                    element261Num = "NUM_261", population11 = "NUM_POP11",
                    population21 = "NUM_POP21", data = aupusFinal)


    ## Element 541
    calculateEle541(element541Num = "NUM_541", element541Symb = "SYMB_541",
                    element542Num = "NUM_542", element543Num = "NUM_543",
                    element544Num = "NUM_544", element545Num = "NUM_545",
                    data = aupusFinal)

    ## Element 546
    calculateEle546(element546Num = "NUM_546", element546Symb = "SYMB_546",
                    element541Num = "NUM_541", element151Num = "NUM_151",
                    element191Num = "NUM_191", data = aupusFinal)

    ## Total utilization
    calculateTotalUtilization(element91Num = "NUM_91",
                              element95Num = "NUM_95",
                              element96Num = "NUM_96",
                              element101Num = "NUM_101",
                              element111Num = "NUM_111",
                              element121Num = "NUM_121",
                              element131Num = "NUM_131",
                              element141Num = "NUM_141",
                              element151Num = "NUM_151",
                              element161Num = "NUM_161",
                              element546Num = "NUM_546",
                              itemTypeCol = "itemType",
                              data = aupusFinal)

    ## Balance
    calculateBalance(supply = "TOTAL_SUPPLY",
                     utilization = "TOTAL_UTILIZATION",
                     element161Num = "NUM_161",
                     element171Num = "NUM_171",
                     element181Num = "NUM_181",
                     element181Symb = "SYMB_181",
                     balanceElement = "balanceElement",
                     itemTypeCol = "itemType",
                     data = aupusFinal)   
}
