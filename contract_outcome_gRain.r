require(ggplot2)
require(stringr)
require(graph)
require(plyr)
require(Hmisc)
require(Matrix)
require(gRain)
require(gRbase)
require(methods)
require(bnlearn)
require(Rgraphviz)






StandardizeGRainQuery<-function(varGin,
                                studyVariableCol,
                                iVariableCol,
                                dVariableCol,
                                resultLabel=NA
){
    #All contracts by ceiling
    ResultsDF<-querygrain(varGin,
                          nodes=c(studyVariableCol,iVariableCol,dVariableCol),
                          type="joint",result="data.frame")
    colnames(ResultsDF)[colnames(ResultsDF)==iVariableCol]<-"iVariable"
    if(!is.na(resultLabel)){
        #         ResultsDF$iVariable<-factor(ResultsDF$iVariable,levels=levels(ResultsDF$iVariable),
        #                                     labels=paste(resultLabel,levels(ResultsDF$iVariable)))
        ResultsDF$Control<-resultLabel
    }
    else     ResultsDF$Control<-"Overall"
    ResultsDF
}    
    
#     subset(ContractModel)
#     
#     short_cb = table(subset(ContractModel, Dur == "(~2 years+]" & FxCb == "Cost-Based", select = Offr))
#     short_fx =  
#         
#         Compare sort_cb/sum(short_cb), for example, to the output of the Bayes net when the Dur and FxCb evidence are set appropriately
#     
#     The way to check that they are identical is to use the vector or Offr probabilities p_short_cb (of size 4) produced by the Bayes net
#     
#     chisq.test(short_cb, p = p_short_cb)
#     
#     To compare the effect of the pricing structure on the number of offers one could compute
#     
#     chisq.test(short_cb, p = short_fx/sum(short_fx))
#     
#     In my experience, the subsets are so large that most p-values will be vanishingly small so all differences are significant.
#     
#     # compute the approximate average number of offers
#     mean_short_cb = (short_cb[1] + 2*short_cb[2] + 4*short_cb[3] + 5*short_cb[4])/sum(short_cb)
#     mean_short_fx = (short_fx[1] + 2*short_fx[2] + 4*short_fx[3] + 5*short_fx[4])/sum(short_fx)
#     
#     You could then say, for example, that switching from the Cost-Based to Fixed-Priced model will get you on average
#     
#     100*(mean_short_fx - mean_short_cb)/mean_short_cb % 
#     
#     more offers.
#     
#     You could slice the dice the dataset by fixing more variables.  In each case it would be good to compare the Bayes net output to pure counts.
    



StandardizeTableQuery<-function(varGin,
                                studyVariableCol,
                                iVariableCol,
                                dVariableCol,
                                resultLabel=NA
){
    #All contracts by ceiling
    ResultsDF<-querygrain(varGin,
                          nodes=c(studyVariableCol,iVariableCol,dVariableCol),
                          type="joint",result="data.frame")
    colnames(ResultsDF)[colnames(ResultsDF)==iVariableCol]<-"iVariable"
    if(!is.na(resultLabel)){
        #         ResultsDF$iVariable<-factor(ResultsDF$iVariable,levels=levels(ResultsDF$iVariable),
        #                                     labels=paste(resultLabel,levels(ResultsDF$iVariable)))
        ResultsDF$Control<-resultLabel
    }
    else     ResultsDF$Control<-"Overall"
    ResultsDF
}
    
    #     subset(ContractModel)
    #     
    #     short_cb = table(subset(ContractModel, Dur == "(~2 years+]" & FxCb == "Cost-Based", select = Offr))
    #     short_fx =  
    #         
    #         Compare sort_cb/sum(short_cb), for example, to the output of the Bayes net when the Dur and FxCb evidence are set appropriately
    #     
    #     The way to check that they are identical is to use the vector or Offr probabilities p_short_cb (of size 4) produced by the Bayes net
    #     
    #     chisq.test(short_cb, p = p_short_cb)
    #     
    #     To compare the effect of the pricing structure on the number of offers one could compute
    #     
    #     chisq.test(short_cb, p = short_fx/sum(short_fx))
    #     
    #     In my experience, the subsets are so large that most p-values will be vanishingly small so all differences are significant.
    #     
    #     # compute the approximate average number of offers
    #     mean_short_cb = (short_cb[1] + 2*short_cb[2] + 4*short_cb[3] + 5*short_cb[4])/sum(short_cb)
    #     mean_short_fx = (short_fx[1] + 2*short_fx[2] + 4*short_fx[3] + 5*short_fx[4])/sum(short_fx)
    #     
    #     You could then say, for example, that switching from the Cost-Based to Fixed-Priced model will get you on average
    #     
    #     100*(mean_short_fx - mean_short_cb)/mean_short_cb % 
    #     
    #     more offers.
    #     
    #     You could slice the dice the dataset by fixing more variables.  In each case it would be good to compare the Bayes net output to pure counts.
    
# }


StandardizeTableChiSquared<-function(varRaw,
                                     varGin,
#                                 studyVariableCol,
#                                 iVariableCol,
                                dVariableCol
#                                 resultLabel=NA
){
    bayesian<-querygrain(varGin,
                         nodes=c(dVariableCol),
                         type="marginal",
                         result="array")[[dVariableCol]]
    raw<- table(varRaw[,dVariableCol])
    chisq.test(raw, 
               p = c(0.8,0.2 )
    )
}
# 
# QueryControlVariablesRaw<-function(varModel,
# #                                 studyVaruableCol,
# #                                 iVariableCol,
#                                 dVariableCol
# ){
#     ResultsDF<-StandardizeGRainQuery(varGin,studyVaruableCol,iVariableCol,dVariableCol)
#     
#     
#     querygrain(FixedPriceGin,
#                nodes=c(dVariableCol),
#                type="marginal")
#     
#     table(varModel[,dVariableCol])
# #     
#     chisq.test(table(varModel[Ceil==,"dVariableCol"]), 
#                p = querygrain(FixedPriceGin,
#                               nodes=c(dVariableCol),
#                               type="marginal",
#                               result="array")$Term
#     )
#     
#     
#     table(varModel[varModel[,"FxCb"]=="Fixed-Price",dVariableCol])
#     table(varModel[varModel[,"FxCb"]=="Cost-Based",dVariableCol])
#     
    #     short_cb = table(subset(ContractModel, Dur == "(~2 years+]" & FxCb == "Cost-Based", select = Offr))
    #     short_fx =  
    #         
    #         Compare sort_cb/sum(short_cb), for example, to the output of the Bayes net when the Dur and FxCb evidence are set appropriately
    #     
    #     The way to check that they are identical is to use the vector or Offr probabilities p_short_cb (of size 4) produced by the Bayes net
    #     
    #     chisq.test(short_cb, p = p_short_cb)
    #     
    
    #R&D contracts by ceiling    
    #     ResultsDF<-rbind(ResultsDF,StandardizeGRainQuery(
    #         setEvidence(varGin, 
    #                     nodes=c("PSR"),
    #                     states=c("R&D")),
    #         studyVaruableCol,iVariableCol,dVariableCol,resultLabel="R&D")
    #     )
    #   
    #Aircraft contracts by ceiling
#     ResultsDF<-rbind(ResultsDF,StandardizeGRainQuery(
#         setEvidence(varGin, 
#                     nodes=c("What"),
#                     states=c("Aircraft and Drones")),
#         studyVaruableCol,iVariableCol,dVariableCol,resultLabel="Aircraft")
#     )
#     
#     ResultsDF<-rbind(ResultsDF,StandardizeGRainQuery(
#         setEvidence(varGin, 
#                     nodes=c("What"),
#                     states=list(varGin[[1]]$levels$What[
#                         varGin[[1]]$levels$What!="Aircraft and Drones"])),
#         studyVaruableCol,iVariableCol,dVariableCol,resultLabel="Not Aircraft")
#     )
#     
#     ResultsDF<-rbind(ResultsDF,StandardizeGRainQuery(
#         setEvidence(varGin, 
#                     nodes=c("UCA"),
#                     states=c("UCA")),
#         studyVaruableCol,iVariableCol,dVariableCol,resultLabel="UCA")
#     )
#     
#     ResultsDF<-rbind(ResultsDF,StandardizeGRainQuery(
#         setEvidence(varGin, 
#                     nodes=c("UCA"),
#                     states=c("Not UCA")),
#         studyVaruableCol,iVariableCol,dVariableCol,resultLabel="Not UCA")
#     )
#     
#     #IDV contracts by ceiling
#     ResultsDF<-rbind(ResultsDF,StandardizeGRainQuery(
#         setEvidence(varGin, 
#                     nodes=c("Veh"),
#                     states=c("SINGLE AWARD" )),
#         studyVaruableCol,iVariableCol,dVariableCol,resultLabel="Single-Award IDV")
#     )
#     
#     ResultsDF<-rbind(ResultsDF,StandardizeGRainQuery(
#         setEvidence(varGin, 
#                     nodes=c("Veh"),
#                     states= list(c( "MULTIPLE AWARD", "Other IDV" ))),
#         studyVaruableCol,iVariableCol,dVariableCol,resultLabel="Other IDV")
#     )
#     
#     
#     ResultsDF<-rbind(ResultsDF,StandardizeGRainQuery(
#         setEvidence(varGin, 
#                     nodes=c("Veh"),
#                     states= list(c( "Def/Pur" ))),
#         studyVaruableCol,iVariableCol,dVariableCol,resultLabel="Def./Pur.")
#     )
#     
#     #LongDur contracts by ceiling
#     ResultsDF<-rbind(ResultsDF,StandardizeGRainQuery(
#         setEvidence(varGin, 
#                     nodes=c("Dur"),
#                     states=c("(~2 years+]")
#         ),
#         studyVaruableCol,iVariableCol,dVariableCol,resultLabel=       "2+ Year Dur.")
#     )
#     
#     ResultsDF<-rbind(ResultsDF,StandardizeGRainQuery(
#         setEvidence(varGin, 
#                     nodes=c("Dur"),
#                     states=c(list(
#                         varGin[[1]]$levels$Dur[varGin[[1]]$levels$Dur!=
#                                                    "(~2 years+"]))
#         ),
#         studyVaruableCol,iVariableCol,dVariableCol,resultLabel="<2 Year Dur.")
#     )
#     
#     
#     #Contracts competition by ceiling
#     ResultsDF<-rbind(ResultsDF,StandardizeGRainQuery(
#         setEvidence(varGin, 
#                     nodes=c("Comp"),
#                     states=c("Comp.")
#         ),
#         studyVaruableCol,iVariableCol,dVariableCol,resultLabel="Comp.")
#     )
#     
#     ResultsDF<-rbind(ResultsDF,StandardizeGRainQuery(
#         setEvidence(varGin, 
#                     nodes=c("Comp"),
#                     states=c("No Comp.")
#         ),
#         studyVaruableCol,iVariableCol,dVariableCol,resultLabel="No Comp.")
#     )
#     
#     
#     #Order the Controls, reversed is to put the first entry on top.
#     ResultsDF$Control<-factor(ResultsDF$Control,
#                               levels=rev(c("Overall",
#                                            "Aircraft",
#                                            "Not Aircraft",
#                                            "UCA",
#                                            "Not UCA",
#                                            "2+ Year Dur.",
#                                            "<2 Year Dur.",
#                                            "Single-Award IDV",
#                                            "Other IDV",
#                                            "Def./Pur.",
#                                            "Comp.",
#                                            "No Comp."
#                               )),
#                               ordered=TRUE
#     )
#     ResultsDF
#     
# }


QueryControlVariablesBayesian<-function(varGin,
                                studyVaruableCol,
                                iVariableCol,
                                dVariableCol
){
    ResultsDF<-StandardizeGRainQuery(varGin,studyVaruableCol,iVariableCol,dVariableCol)
    
    
    #R&D contracts by ceiling    
    #     ResultsDF<-rbind(ResultsDF,StandardizeGRainQuery(
    #         setEvidence(varGin, 
    #                     nodes=c("PSR"),
    #                     states=c("R&D")),
    #         studyVaruableCol,iVariableCol,dVariableCol,resultLabel="R&D")
    #     )
    #   
    #Aircraft contracts by ceiling
    ResultsDF<-rbind(ResultsDF,StandardizeGRainQuery(
        setEvidence(varGin, 
                    nodes=c("What"),
                    states=c("Aircraft and Drones")),
        studyVaruableCol,iVariableCol,dVariableCol,resultLabel="Aircraft")
    )
    
    ResultsDF<-rbind(ResultsDF,StandardizeGRainQuery(
        setEvidence(varGin, 
                    nodes=c("What"),
                    states=list(varGin[[1]]$levels$What[
                        varGin[[1]]$levels$What!="Aircraft and Drones"])),
        studyVaruableCol,iVariableCol,dVariableCol,resultLabel="Not Aircraft")
    )
    
    ResultsDF<-rbind(ResultsDF,StandardizeGRainQuery(
        setEvidence(varGin, 
                    nodes=c("UCA"),
                    states=c("UCA")),
        studyVaruableCol,iVariableCol,dVariableCol,resultLabel="UCA")
    )
    
    ResultsDF<-rbind(ResultsDF,StandardizeGRainQuery(
        setEvidence(varGin, 
                    nodes=c("UCA"),
                    states=c("Not UCA")),
        studyVaruableCol,iVariableCol,dVariableCol,resultLabel="Not UCA")
    )
    
    #IDV contracts by ceiling
    ResultsDF<-rbind(ResultsDF,StandardizeGRainQuery(
        setEvidence(varGin, 
                    nodes=c("Veh"),
                    states=c("SINGLE AWARD" )),
        studyVaruableCol,iVariableCol,dVariableCol,resultLabel="Single-Award IDV")
    )
    
    ResultsDF<-rbind(ResultsDF,StandardizeGRainQuery(
        setEvidence(varGin, 
                    nodes=c("Veh"),
                    states= list(c( "MULTIPLE AWARD", "Other IDV" ))),
        studyVaruableCol,iVariableCol,dVariableCol,resultLabel="Other IDV")
    )
    
    
    ResultsDF<-rbind(ResultsDF,StandardizeGRainQuery(
        setEvidence(varGin, 
                    nodes=c("Veh"),
                    states= list(c( "Def/Pur" ))),
        studyVaruableCol,iVariableCol,dVariableCol,resultLabel="Def./Pur.")
    )
    
    #LongDur contracts by ceiling
    ResultsDF<-rbind(ResultsDF,StandardizeGRainQuery(
        setEvidence(varGin, 
                    nodes=c("Dur"),
                    states=c("(~2 years+]")
        ),
        studyVaruableCol,iVariableCol,dVariableCol,resultLabel=       "2+ Year Dur.")
    )
    
    ResultsDF<-rbind(ResultsDF,StandardizeGRainQuery(
        setEvidence(varGin, 
                    nodes=c("Dur"),
                    states=c(list(
                        varGin[[1]]$levels$Dur[varGin[[1]]$levels$Dur!=
                                                   "(~2 years+"]))
        ),
        studyVaruableCol,iVariableCol,dVariableCol,resultLabel="<2 Year Dur.")
    )
    
    
    #Contracts competition by ceiling
    ResultsDF<-rbind(ResultsDF,StandardizeGRainQuery(
        setEvidence(varGin, 
                    nodes=c("Comp"),
                    states=c("Comp.")
        ),
        studyVaruableCol,iVariableCol,dVariableCol,resultLabel="Comp.")
    )
    
    ResultsDF<-rbind(ResultsDF,StandardizeGRainQuery(
        setEvidence(varGin, 
                    nodes=c("Comp"),
                    states=c("No Comp.")
        ),
        studyVaruableCol,iVariableCol,dVariableCol,resultLabel="No Comp.")
    )
    
    
    #Order the Controls, reversed is to put the first entry on top.
    ResultsDF$Control<-factor(ResultsDF$Control,
                              levels=rev(c("Overall",
                                           "Aircraft",
                                           "Not Aircraft",
                                           "UCA",
                                           "Not UCA",
                                           "2+ Year Dur.",
                                           "<2 Year Dur.",
                                           "Single-Award IDV",
                                           "Other IDV",
                                           "Def./Pur.",
                                           "Comp.",
                                           "No Comp."
                              )),
                              ordered=TRUE
    )
    ResultsDF
    
}



FixedPriceComparison<-function(varGin,HypothesisLabel=NA){
    
    TermDF<-QueryControlVariables(varGin,
                                  "FxCb",
                                  "Ceil",
                                  "Term"
    )
    
    
    TermDF<-ddply(TermDF,.(FxCb,Control,iVariable),transform,p=Freq/sum(Freq))
    colnames(TermDF)[colnames(TermDF)=="Term"]<-"dVariable"
    TermDF<-subset(TermDF,dVariable=="Terminated")
    TermDF$Average<-NA
    
    
    expNChgDF<-QueryControlVariables(varGin,
                                     "FxCb",
                                     "Ceil",
                                     "NChg"
    )
    
    expNChgDF<-ddply(expNChgDF,.(FxCb,Control,iVariable),transform,p=Freq/sum(Freq))
    expNChgDF$Average[expNChgDF$NChg=="   0"]<-0
    expNChgDF$Average[expNChgDF$NChg=="   1"]<-1*expNChgDF$p[expNChgDF$NChg=="   1"]
    expNChgDF$Average[expNChgDF$NChg=="   2"]<-2*expNChgDF$p[expNChgDF$NChg=="   2"]
    expNChgDF$Average[expNChgDF$NChg=="[   3,1040]"]<-3*expNChgDF$p[expNChgDF$NChg=="[   3,1040]"]
    expNChgDF<-ddply(expNChgDF,.(FxCb,Control,iVariable),summarise,Average=sum(Average),p=sum(p),Freq=sum(Freq))
    expNChgDF$dVariable<-"Average Number of Change Orders"
    
    expCRaiDF<-QueryControlVariables(varGin,
                                     "FxCb",
                                     "Ceil",
                                     "CRai"
    )
    
    
    expCRaiDF<-ddply(expCRaiDF,.(FxCb,Control,iVariable),transform,p=Freq/sum(Freq))
    expCRaiDF$Average[expCRaiDF$CRai=="[-0.001, 0.001)"]<-0
    expCRaiDF$Average[expCRaiDF$CRai=="[  -Inf,-0.001)"]<- -0.001*expCRaiDF$p[expCRaiDF$CRai=="[  -Inf,-0.001)"]
    expCRaiDF$Average[expCRaiDF$CRai=="[ 0.001, 0.150)"]<-0.001*expCRaiDF$p[expCRaiDF$CRai=="[ 0.001, 0.150)"]
    expCRaiDF$Average[expCRaiDF$CRai=="[ 0.150,   Inf]"]<-0.15*expCRaiDF$p[expCRaiDF$CRai=="[ 0.150,   Inf]"]
    expCRaiDF<-ddply(expCRaiDF,.(FxCb,Control,iVariable),summarise,Average=sum(Average),p=sum(p),Freq=sum(Freq))
    expCRaiDF$dVariable<-"Ceiling Raising Change Orders %"
    
    
    #Limit just to competed contracts
    varGin<-setEvidence(varGin, 
                        nodes=c("Comp"),
                        states=c("Comp.")
    )
    
    expOffrDF<-QueryControlVariables(varGin,
                                     "FxCb",
                                     "Ceil",
                                     "Offr"
    )
    
    expOffrDF<-ddply(expOffrDF,.(FxCb,Control,iVariable),transform,p=Freq/sum(Freq))
    expOffrDF$Average[expOffrDF$Offr=="1"]<-1*expOffrDF$p[expOffrDF$Offr=="1"]
    expOffrDF$Average[expOffrDF$Offr=="2"]<-2*expOffrDF$p[expOffrDF$Offr=="2"]
    expOffrDF$Average[expOffrDF$Offr=="3-4"]<-3.5*expOffrDF$p[expOffrDF$Offr=="3-4"]
    expOffrDF$Average[expOffrDF$Offr=="5+"]<-5*expOffrDF$p[expOffrDF$Offr=="5+"]
    expOffrDF<-ddply(expOffrDF,.(FxCb,Control,iVariable),summarise,Average=sum(Average),p=sum(p),Freq=sum(Freq))
    expOffrDF$dVariable<-"Average Number of Offers for Competed Contracts"
    
    
    
    OffrDF<-QueryControlVariables(varGin,
                                  "FxCb",
                                  "Ceil",
                                  "Offr"
    )
    
    
    colnames(OffrDF)[colnames(OffrDF)=="Ceil"]<-"iVariable"
    OffrDF<-ddply(OffrDF,.(FxCb,Control,iVariable),transform,p=Freq/sum(Freq))
    colnames(OffrDF)[colnames(OffrDF)=="Offr"]<-"dVariable"
    OffrDF<-subset(OffrDF,dVariable=="1")
    OffrDF$dVariable<-"% Single Offer Competition"
    OffrDF$Average<-NA
    ResultsDF<-rbind(TermDF,expOffrDF,OffrDF,expNChgDF,expCRaiDF)
    
    
    if(!is.na(HypothesisLabel)) ResultsDF$Hypothesis<-HypothesisLabel
    
    #Order the ceilings
    ResultsDF$iVariable<-factor(ResultsDF$iVariable,
                                levels=c("[75m+]",
                                         "[1m,10m)", 
                                         "[10m,75m)",
                                         "[100k,1m)",
                                         "[15k,100k)",
                                         "[0,15k)"
                                ),
                                ordered=TRUE
    )
    
    
    ResultsDF
    
}




FixedPriceCast<-function(ResultsDF){
    
    ResultsDF<-melt(ResultsDF,id=c("Control" , "dVariable" , "Hypothesis" , "iVariable",
                                   "FxCb"),
                    factorsAsStrings=T) # , "parameter","p.value","method", "statistic",
    ResultsDF<-dcast(ResultsDF, Control + dVariable + Hypothesis + iVariable  ~ FxCb +variable
#                      , sum    +statistic+ parameter+p.value+method
                     , value.var="value"
    )
    ResultsDF<-ResultsDF[!names(ResultsDF) %in% c("Fixed-Price_statistic",
                               "Fixed-Price_parameter",
                               "Fixed-Price_p.value",
                               "Fixed-Price_method",
                               "Fixed-Price_Significance"),]
ResultsDF[,"Fixed-Price_Freq"]<-FactorToNumber(ResultsDF[,"Fixed-Price_Freq"])
ResultsDF[,"Fixed-Price_Count"]<-FactorToNumber(ResultsDF[,"Fixed-Price_Count"])
ResultsDF[,"Fixed-Price_p"]<-FactorToNumber(ResultsDF[,"Fixed-Price_p"])
ResultsDF[,"Fixed-Price_Average"]<-FactorToNumber(ResultsDF[,"Fixed-Price_Average"])
ResultsDF[,"Cost-Based_statistic"]<-FactorToNumber(ResultsDF[,"Cost-Based_statistic"])
ResultsDF[,"Cost-Based_parameter"]<-factor(ResultsDF[,"Cost-Based_parameter"])
ResultsDF[,"Cost-Based_p.value"]<-FactorToNumber(ResultsDF[,"Cost-Based_p.value"])
ResultsDF[,"Cost-Based_method"]<-factor(ResultsDF[,"Cost-Based_method"])
ResultsDF[,"Cost-Based_Significance"]<-factor(ResultsDF[,"Cost-Based_Significance"])
ResultsDF[,"Cost-Based_Freq"]<-FactorToNumber(ResultsDF[,"Cost-Based_Freq"])
ResultsDF[,"Cost-Based_Count"]<-FactorToNumber(ResultsDF[,"Cost-Based_Count"])
ResultsDF[,"Cost-Based_p"]<-FactorToNumber(ResultsDF[,"Cost-Based_p"])
ResultsDF[,"Cost-Based_Average"]<-FactorToNumber(ResultsDF[,"Cost-Based_Average"])
ResultsDF[,"Combination or Other_statistic"]<-FactorToNumber(ResultsDF[,"Combination or Other_statistic"])
ResultsDF[,"Combination or Other_parameter"]<-factor(ResultsDF[,"Combination or Other_parameter"])
ResultsDF[,"Combination or Other_p.value"]<-FactorToNumber(ResultsDF[,"Combination or Other_p.value"])
ResultsDF[,"Combination or Other_method"]<-factor(ResultsDF[,"Combination or Other_method"])
ResultsDF[,"Combination or Other_Significance"]<-factor(ResultsDF[,"Combination or Other_Significance"])
ResultsDF[,"Combination or Other_Freq"]<-FactorToNumber(ResultsDF[,"Combination or Other_Freq"])
ResultsDF[,"Combination or Other_Count"]<-FactorToNumber(ResultsDF[,"Combination or Other_Count"])
ResultsDF[,"Combination or Other_p"]<-FactorToNumber(ResultsDF[,"Combination or Other_p"])
ResultsDF[,"Combination or Other_Average"]<-FactorToNumber(ResultsDF[,"Combination or Other_Average"])

    ResultsDF$FixedCostMargin_p<-(ResultsDF[,"Fixed-Price_p"]-ResultsDF[,"Cost-Based_p"])/
        ResultsDF[,"Cost-Based_p"]
    ResultsDF$FixedCombMargin_p<-(ResultsDF[,"Fixed-Price_p"]-ResultsDF[,"Combination or Other_p"])/
        ResultsDF[,"Combination or Other_p"]
    
    ResultsDF$FixedCostMargin_Average<-(ResultsDF[,"Fixed-Price_Average"]-ResultsDF[,"Cost-Based_Average"])/
        ResultsDF[,"Cost-Based_Average"]
    ResultsDF$FixedCombMargin_Average<-(ResultsDF[,"Fixed-Price_Average"]-ResultsDF[,"Combination or Other_Average"])/
        ResultsDF[,"Combination or Other_Average"]
    names(ResultsDF)<-gsub("-", ".", names(ResultsDF))
    
    ResultsDF
    
}


FixedPriceHypothesisTester<-function(varGin,HypothesisLabel=NA){
    
    TermDF<-QueryControlVariables(varGin,
                                  "FxCb",
                                  "Ceil",
                                  "Term"
    )
    
    
    TermDF<-ddply(TermDF,.(FxCb,Control,iVariable),transform,p=Freq/sum(Freq))
    TermDF<-dcast(TermDF,Term + Control + iVariable ~ FxCb, sum, value.var="p")
    TermDF$FixedCostMargin<-TermDF[,"Fixed-Price"]/TermDF[,"Cost-Based"]
    TermDF$FixedCombMargin<-TermDF[,"Fixed-Price"]/TermDF[,"Combination or Other"]
    colnames(TermDF)[colnames(TermDF)=="Term"]<-"dVariable"
    TermDF<-subset(TermDF,dVariable=="Terminated")
    
    
    
    
    
    
    
    expNChgDF<-QueryControlVariables(varGin,
                                     "FxCb",
                                     "Ceil",
                                     "NChg"
    )
    
    expNChgDF<-ddply(expNChgDF,.(FxCb,Control,iVariable),transform,p=Freq/sum(Freq))
    expNChgDF$expNChg[expNChgDF$NChg=="   0"]<-0
    expNChgDF$expNChg[expNChgDF$NChg=="   1"]<-1*expNChgDF$p[expNChgDF$NChg=="   1"]
    expNChgDF$expNChg[expNChgDF$NChg=="   2"]<-2*expNChgDF$p[expNChgDF$NChg=="   2"]
    expNChgDF$expNChg[expNChgDF$NChg=="[   3,1040]"]<-3*expNChgDF$p[expNChgDF$NChg=="[   3,1040]"]
    expNChgDF<-ddply(expNChgDF,.(FxCb,Control,iVariable),summarise,expNChg=sum(expNChg))
    expNChgDF<-dcast(expNChgDF,  iVariable + Control ~ FxCb  , sum, value.var="expNChg")
    expNChgDF$dVariable<-"Average Number of Change Orders"
    expNChgDF$FixedCostMargin<-expNChgDF[,"Fixed-Price"]/expNChgDF[,"Cost-Based"]
    expNChgDF$FixedCombMargin<-expNChgDF[,"Fixed-Price"]/expNChgDF[,"Combination or Other"]
    
    
    expCRaiDF<-QueryControlVariables(varGin,
                                     "FxCb",
                                     "Ceil",
                                     "CRai"
    )
    
    
    expCRaiDF<-ddply(expCRaiDF,.(FxCb,Control,iVariable),transform,p=Freq/sum(Freq))
    expCRaiDF$expCRai[expCRaiDF$CRai=="[-0.001, 0.001)"]<-0
    expCRaiDF$expCRai[expCRaiDF$CRai=="[  -Inf,-0.001)"]<- -0.001*expCRaiDF$p[expCRaiDF$CRai=="[  -Inf,-0.001)"]
    expCRaiDF$expCRai[expCRaiDF$CRai=="[ 0.001, 0.120)"]<-0.001*expCRaiDF$p[expCRaiDF$CRai=="[ 0.001, 0.120)"]
    expCRaiDF$expCRai[expCRaiDF$CRai=="[ 0.120,   Inf]"]<-0.12*expCRaiDF$p[expCRaiDF$CRai=="[ 0.120,   Inf]"]
    expCRaiDF<-ddply(expCRaiDF,.(FxCb,Control,iVariable),summarise,expCRai=sum(expCRai))
    expCRaiDF<-dcast(expCRaiDF, iVariable + Control~ FxCb, sum, value.var="expCRai")
    expCRaiDF$dVariable<-"Ceiling Raising Change Orders %"
    expCRaiDF$FixedCostMargin<-expCRaiDF[,"Fixed-Price"]/expCRaiDF[,"Cost-Based"]
    expCRaiDF$FixedCombMargin<-expCRaiDF[,"Fixed-Price"]/expCRaiDF[,"Combination or Other"]
    
    
    
    #Limit just to competed contracts
    varGin<-setEvidence(varGin, 
                        nodes=c("Comp"),
                        states=c("Comp.")
    )
    
    expOffrDF<-QueryControlVariables(varGin,
                                     "FxCb",
                                     "Ceil",
                                     "Offr"
    )
    
    expOffrDF<-ddply(expOffrDF,.(FxCb,Control,iVariable),transform,p=Freq/sum(Freq))
    expOffrDF$expOffr[expOffrDF$Offr=="1"]<-1*expOffrDF$p[expOffrDF$Offr=="1"]
    expOffrDF$expOffr[expOffrDF$Offr=="2"]<-2*expOffrDF$p[expOffrDF$Offr=="2"]
    expOffrDF$expOffr[expOffrDF$Offr=="3-4"]<-3.5*expOffrDF$p[expOffrDF$Offr=="3-4"]
    expOffrDF$expOffr[expOffrDF$Offr=="5+"]<-5*expOffrDF$p[expOffrDF$Offr=="5+"]
    expOffrDF<-ddply(expOffrDF,.(FxCb,Control,iVariable),summarise,expOffr=sum(expOffr))
    expOffrDF<-dcast(expOffrDF,  iVariable + Control ~ FxCb  , sum, value.var="expOffr")
    expOffrDF$dVariable<-"Average Number of Offers for Competed Contracts"
    expOffrDF$FixedCostMargin<-expOffrDF[,"Fixed-Price"]/expOffrDF[,"Cost-Based"]
    expOffrDF$FixedCombMargin<-expOffrDF[,"Fixed-Price"]/expOffrDF[,"Combination or Other"]
    
    
    
    OffrDF<-QueryControlVariables(varGin,
                                  "FxCb",
                                  "Ceil",
                                  "Offr"
    )
    
    
    colnames(OffrDF)[colnames(OffrDF)=="Ceil"]<-"iVariable"
    OffrDF<-ddply(OffrDF,.(FxCb,Control,iVariable),transform,p=Freq/sum(Freq))
    OffrDF<-dcast(OffrDF,Offr + Control + iVariable ~ FxCb, sum, value.var="p")
    OffrDF$FixedCostMargin<-OffrDF[,"Fixed-Price"]/OffrDF[,"Cost-Based"]
    OffrDF$FixedCombMargin<-OffrDF[,"Fixed-Price"]/OffrDF[,"Combination or Other"]
    colnames(OffrDF)[colnames(OffrDF)=="Offr"]<-"dVariable"
    OffrDF<-subset(OffrDF,dVariable=="1")
    expNChgDF$dVariable<-"% Single Offer Competition"
    
    ResultsDF<-rbind(TermDF,expOffrDF,OffrDF,expNChgDF,expCRaiDF)
    
    
    if(!is.na(HypothesisLabel)) ResultsDF$Hypothesis<-HypothesisLabel
    
    #Order the ceilings
    ResultsDF$iVariable<-factor(ResultsDF$iVariable,
                                levels=c("[75m+]",
                                         "[10m,75m)",
                                         "[1m,10m)", 
                                         "[100k,1m)",
                                         "[15k,100k)",
                                         "[0,15k)"
                                ),
                                ordered=TRUE
    )
    
    
    ResultsDF
    
}

