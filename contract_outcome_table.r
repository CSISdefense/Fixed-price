require(ggplot2)
require(stringr)
require(graph)
require(plyr)
require(Hmisc)
require(Matrix)
require(methods)
require(bnlearn)







StandardizeTableQuery<-function(varTable,
                                studyVariableCol,
                                iVariableCol,
                                dVariableCol,
                                resultLabel=NA
){
    
    #All contracts by ceiling
    ResultsDF<-varTable[,names(varTable) %in% c(studyVariableCol,
                                                iVariableCol,
                                                dVariableCol,
                                                "Freq",
                                                "Count")]
    if(nrow(varTable)>0){
        #     ResultsDF<-querygrain(varTable,
        #                           nodes=c(studyVariableCol,iVariableCol,dVariableCol),
        #                           type="joint",result="data.frame")
        ResultsDF<-ddply(ResultsDF,
                         c(studyVariableCol,
                           iVariableCol,
                           dVariableCol),
                         summarise,
                         Freq=sum(Freq),
                         Count=sum(Count))
        
        colnames(ResultsDF)[colnames(ResultsDF)==iVariableCol]<-"iVariable"
        if(!is.na(resultLabel)){
            #         ResultsDF$iVariable<-factor(ResultsDF$iVariable,levels=levels(ResultsDF$iVariable),
            #                                     labels=paste(resultLabel,levels(ResultsDF$iVariable)))
            ResultsDF$Control<-resultLabel
        }
        else     ResultsDF$Control<-"Overall"
    }
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




QueryControlVariablesTable<-function(varTable,
                                studyVariableCol,
                                iVariableCol,
                                dVariableCol
){
    ResultsDF<-StandardizeTableQuery(varTable,studyVariableCol,iVariableCol,dVariableCol)
    
    
    
    
    #Aircraft contracts by ceiling
    ResultsDF<-rbind(ResultsDF,StandardizeTableQuery(
        subset(varTable, 
                    What=="Aircraft and Drones"),
        studyVariableCol,iVariableCol,dVariableCol,resultLabel="Aircraft")
    )
    
    ResultsDF<-rbind(ResultsDF,StandardizeTableQuery(
        subset(varTable, What %in% levels(varTable$What)[
            levels(varTable$What)!="Aircraft and Drones"]),
        studyVariableCol,iVariableCol,dVariableCol,resultLabel="Not Aircraft")
    )
    
    ResultsDF<-rbind(ResultsDF,StandardizeTableQuery(
        subset(varTable, UCA=="UCA"),
        studyVariableCol,iVariableCol,dVariableCol,resultLabel="UCA")
    )
    
    ResultsDF<-rbind(ResultsDF,StandardizeTableQuery(
        subset(varTable, UCA=="Not UCA"),
        studyVariableCol,iVariableCol,dVariableCol,resultLabel="Not UCA")
    )
    
    #IDV contracts by ceiling
    ResultsDF<-rbind(ResultsDF,StandardizeTableQuery(
        subset(varTable, Veh=="SINGLE AWARD" ),
        studyVariableCol,iVariableCol,dVariableCol,resultLabel="Single-Award IDV")
    )
    
    ResultsDF<-rbind(ResultsDF,StandardizeTableQuery(
        subset(varTable, Veh %in% c( "MULTIPLE AWARD", "Other IDV" )),
        studyVariableCol,iVariableCol,dVariableCol,resultLabel="Other IDV")
    )
    
    
    ResultsDF<-rbind(ResultsDF,StandardizeTableQuery(
        subset(varTable, Veh =="Def/Pur" ),
        studyVariableCol,iVariableCol,dVariableCol,resultLabel="Def./Pur.")
    )
    
    #LongDur contracts by ceiling
    ResultsDF<-rbind(ResultsDF,StandardizeTableQuery(
        subset(varTable, Dur=="(~2 years+]"),
        studyVariableCol,iVariableCol,dVariableCol,resultLabel=       "2+ Year Dur.")
    )
    
    ResultsDF<-rbind(ResultsDF,StandardizeTableQuery(
        subset(varTable, Dur %in% levels(varTable$Dur)[
            levels(varTable$What)!="(~2 years+"]),
        studyVariableCol,iVariableCol,dVariableCol,resultLabel="<2 Year Dur.")
    )
    
    
    #Contracts competition by ceiling
    ResultsDF<-rbind(ResultsDF,StandardizeTableQuery(
        subset(varTable, Comp=="Comp."),
        studyVariableCol,iVariableCol,dVariableCol,resultLabel="Comp.")
    )
    
    ResultsDF<-rbind(ResultsDF,StandardizeTableQuery(
        subset(varTable, Comp=="No Comp."),
        studyVariableCol,iVariableCol,dVariableCol,resultLabel="No Comp.")
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
    ResultsDF<-AddZerosForMissingFreqs(ResultsDF,dVariableCol)
    
    

    
    ResultsDF$iVariable<-revalue(ResultsDF$iVariable, c("[15k,100k)"="[0,100k)","[0,15k)"="[0,100k)"))
    
    
    
    #Order the ceilings
    ResultsDF$iVariable<-factor(ResultsDF$iVariable,
                                levels=c("[75m+]",
                                         "[10m,75m)",
                                         "[1m,10m)",
                                         "[100k,1m)",
                                         "[0,100k)"
                                         
                                         
                                ),
                                labels=c("[75m+]",
                                         "[10m,75m)",
                                         "[1m,10m)",
                                         "[100k,1m)",
                                         "[0,100k)"
                                         ),
                                ordered=TRUE
    )
    
    ResultsDF<-ddply(ResultsDF,c("FxCb","Control","iVariable",dVariableCol),summarise,Freq=sum(Freq))
    
    
    ResultsDF

    
    
}


AddZerosForMissingFreqs<-function(resultsDF,dVariableCol){
    
    
    #Add rows with zeros for combinations that do not exist in the model, e.g. terminated cost plus $75+
    #but that do have corresponding contracts in other categoreis.
    zeroDF<-ddply(resultsDF,.(FxCb,Control,iVariable),summarise,Freq=0)
    #     blankDF<-resultsDF[0,]
    for(i in levels(resultsDF[,dVariableCol])){
        newrows<-zeroDF
        newrows[,dVariableCol]<-i
        resultsDF<-rbind(resultsDF,
                         newrows)
    }
    #Merge those zero lines into existing rows. This means most of them go away.
    resultsDF<-ddply(resultsDF,c("FxCb","Control","iVariable",dVariableCol),summarise,Freq=sum(Freq))
    resultsDF
}


FixedPriceComparisonTable<-function(varTable,HypothesisLabel=NA){
    
    TermDF<-QueryControlVariablesTable(varTable,
                                  "FxCb",
                                  "Ceil",
                                  "Term"
    )
    
        
    TermDF<-ddply(TermDF,.(FxCb,Control,iVariable),transform,p=Freq/sum(Freq))
    colnames(TermDF)[colnames(TermDF)=="Term"]<-"dVariable"
    TermDF<-subset(TermDF,dVariable=="Terminated")
    TermDF$Average<-NA
    
    
    expNChgDF<-QueryControlVariablesTable(varTable,
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
    
    expCRaiDF<-QueryControlVariablesTable(varTable,
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
    varTable<-subset(varTable, Comp=="Comp.")
    
    
    expOffrDF<-QueryControlVariablesTable(varTable,
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
    
    
    
    OffrDF<-QueryControlVariablesTable(varTable,
                                  "FxCb",
                                  "Ceil",
                                  "Offr"
    )
    
    
    OffrDF<-ddply(OffrDF,.(FxCb,Control,iVariable),transform,p=Freq/sum(Freq))
    colnames(OffrDF)[colnames(OffrDF)=="Offr"]<-"dVariable"
    OffrDF<-subset(OffrDF,dVariable=="1")
    OffrDF$dVariable<-"% Single Offer Competition"
    OffrDF$Average<-NA
    ResultsDF<-rbind(TermDF,expOffrDF,OffrDF,expNChgDF,expCRaiDF)
    
    
    if(!is.na(HypothesisLabel)) ResultsDF$Hypothesis<-HypothesisLabel
    
    
    
    ResultsDF
    
}



FixedPriceHypothesisTable<-function(varTable,HypothesisLabel=NA){
    
    TermDF<-QueryControlVariablesTable(varTable,
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
    
    
    
    
    
    
    
    expNChgDF<-QueryControlVariablesTable(varTable,
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
    
    
    expCRaiDF<-QueryControlVariablesTable(varTable,
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
    varTable<-subset(varTable, Comp=="Comp.")
    
    
    expOffrDF<-QueryControlVariablesTable(varTable,
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
    
    
    
    OffrDF<-QueryControlVariablesTable(varTable,
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
    

    
    ResultsDF
    
}

