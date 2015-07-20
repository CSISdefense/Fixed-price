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
                                resultLabel="Overall"
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
        ResultsDF$Control<-resultLabel
        
    }
    
    ResultsDF<-ChiSquaredFixedPriceCostBased(ResultsDF,dVariableCol,resultLabel)    
    
    
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


ChiSquaredFixedPriceCostBased<-function(ResultsDF,dVariableCol,ControlCol){
    ResultsDF<-AddZerosForMissingFreqs(ResultsDF,dVariableCol)
    
    
    for(i in levels(ResultsDF$iVariable)){
        Fx<-subset(ResultsDF,iVariable==i&Control==ControlCol&FxCb=="Fixed-Price")
        Fx<-Fx[order(Fx[,dVariableCol]),]
        Cb<-subset(ResultsDF,iVariable==i&Control==ControlCol&FxCb=="Cost-Based")
        Cb<-Cb[order(Cb[,dVariableCol]),]
        RelevantList<-ResultsDF$iVariable==i&ResultsDF$Control==ControlCol&
            ResultsDF$FxCb %in% c("Fixed-Price","Cost-Based")
        if(nrow(Fx)>0 & nrow(Cb)>0){
            #Chi-squared is less reliable if the expected value of any cell
            #is less than 5.
            if(min(sum(Cb$Count)*Fx$Count/sum(Fx$Count))<5){
                test<-chisq.test(Cb$Count,p=Fx$Count/sum(Fx$Count),simulate.p.value=TRUE)
            } else{
                test<-chisq.test(Cb$Count,p=Fx$Count/sum(Fx$Count),simulate.p.value=FALSE)
            }
            ResultsDF[RelevantList,"statistic"]<-test$statistic
            ResultsDF[RelevantList,"parameter"]<-test$parameter
            ResultsDF[RelevantList,"p.value"]<-test$p.value
            ResultsDF[RelevantList,"method"]<-test$method
        }
        else if(any(RelevantList)){
            ResultsDF[RelevantList,"statistic"]<-NA
            ResultsDF[RelevantList,"parameter"]<-NA
            ResultsDF[RelevantList,"p.value"]<-NA
            ResultsDF[RelevantList,"method"]<-"No comparison due to missing data"
        }
    }
    ResultsDF
}

ChiSquaredPopulationComparison<-function(ResultsDF,dVariableCol,ControlCol,
                                         HypothesisCol,HypothesisValue){
    ResultsDF<-AddZerosForMissingFreqs(ResultsDF,dVariableCol)
    
    for(d in levels(ResultsDF[dVariableCol,])){
        for(c in levels(ResultsDF[ControlCol,])){       
            for(i in levels(ResultsDF$iVariable)){
                Pop<-ResultsDF[[ResultsDF$iVariable==i&
                                   ResultsDF[,ControlCol]==c&
                                   ResultsDF[,dVariableCol]==d&
                                   ResultsDF[,HypothesisCol]=="Population",]]
                
                Hyp<-ResultsDF[[ResultsDF$iVariable==i&
                                   ResultsDF[,ControlCol]==c&
                                   ResultsDF[,dVariableCol]==d&
                                   ResultsDF[,HypothesisCol]=="HypothesisValue",]]
                
                RelevantList<-ResultsDF$iVariable==i&
                    ResultsDF[,ControlCol]==c&
                    ResultsDF[,dVariableCol]==d
                if(nrow(Pop)>0 & nrow(Hyp)>0){
                    #Use a Chi-Squared test when there is no average i.e. for Terminations and Single Offer
                    if(is.na(Hyp$Average)){
                        #Chi-squared is less reliable if the expected value of any cell
                        #is less than 5.
                        HypCount<-c(Hyp$Fixed-Price_Count,
                                    ((1/Hyp$Fixed-Price_p)-1)*Hyp$Fixed-Price_Count
                        if(min(sum(Hyp$Count)*Pop$Count/sum(Pop$Count))<5){
                            test<-chisq.test(Hyp$Count,p=Pop$Count/sum(Pop$Count),simulate.p.value=TRUE)
                        } else{
                            test<-chisq.test(Hyp$Count,p=Pop$Count/sum(Pop$Count),simulate.p.value=FALSE)
                        }
                        ResultsDF[RelevantList,"statistic"]<-test$statistic
                        ResultsDF[RelevantList,"parameter"]<-test$parameter
                        ResultsDF[RelevantList,"p.value"]<-test$p.value
                        ResultsDF[RelevantList,"method"]<-test$method
                    }
                }
                else if(any(RelevantList)){
                    ResultsDF[RelevantList,"statistic"]<-NA
                    ResultsDF[RelevantList,"parameter"]<-NA
                    ResultsDF[RelevantList,"p.value"]<-NA
                    ResultsDF[RelevantList,"method"]<-"No comparison due to missing data"
                }
            }
        }
    }
    ResultsDF
}


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
    
    
    ResultsDF

    
    
}


AddZerosForMissingFreqs<-function(resultsDF,dVariableCol){
    
    
    #Add rows with zeros for combinations that do not exist in the model, e.g. terminated cost plus $75+
    #but that do have corresponding contracts in other categoreis.
    zeroDF<-ddply(resultsDF,.(FxCb,Control,iVariable),summarise,Freq=0,Count=0)
    #     blankDF<-resultsDF[0,]
    if( nrow(zeroDF)>0){
    for(i in levels(resultsDF[,dVariableCol])){
        newrows<-zeroDF
        newrows[,dVariableCol]<-i
        resultsDF<-rbind(resultsDF,
                         newrows)
    }
    #Merge those zero lines into existing rows. This means most of them go away.
    resultsDF<-ddply(resultsDF,c("FxCb","Control","iVariable",dVariableCol),summarise,Freq=sum(Freq),Count=sum(Count))
    }
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

    
    
    expNChgDF<-ddply(expNChgDF,.(FxCb,Control,iVariable,statistic,parameter,p.value,method),transform,p=Freq/sum(Freq))
    expNChgDF$Average[expNChgDF$NChg=="   0"]<-0
    expNChgDF$Average[expNChgDF$NChg=="   1"]<-1*expNChgDF$p[expNChgDF$NChg=="   1"]
    expNChgDF$Average[expNChgDF$NChg=="   2"]<-2*expNChgDF$p[expNChgDF$NChg=="   2"]
    expNChgDF$Average[expNChgDF$NChg=="[   3,1040]"]<-3*expNChgDF$p[expNChgDF$NChg=="[   3,1040]"]
    expNChgDF<-ddply(expNChgDF,.(FxCb,Control,iVariable,statistic,parameter,p.value,method),summarise,Average=sum(Average),p=sum(p),
                     Freq=sum(Freq),
                     Count=sum(Count))
    expNChgDF$dVariable<-"Average Number of Change Orders"
    
#     debug(QueryControlVariablesTable)
    expCRaiDF<-QueryControlVariablesTable(varTable,
                                     "FxCb",
                                     "Ceil",
                                     "CRai"
    )
    
    
    expCRaiDF<-ddply(expCRaiDF,.(FxCb,Control,iVariable,statistic,parameter,p.value,method),transform,p=Freq/sum(Freq))
    expCRaiDF$Average[expCRaiDF$CRai=="[-0.001, 0.001)"]<-0
    expCRaiDF$Average[expCRaiDF$CRai=="[  -Inf,-0.001)"]<- -0.001*expCRaiDF$p[expCRaiDF$CRai=="[  -Inf,-0.001)"]
    expCRaiDF$Average[expCRaiDF$CRai=="[ 0.001, 0.150)"]<-0.001*expCRaiDF$p[expCRaiDF$CRai=="[ 0.001, 0.150)"]
    expCRaiDF$Average[expCRaiDF$CRai=="[ 0.150,   Inf]"]<-0.15*expCRaiDF$p[expCRaiDF$CRai=="[ 0.150,   Inf]"]
    expCRaiDF<-ddply(expCRaiDF,.(FxCb,Control,iVariable,statistic,parameter,p.value,method),summarise,Average=sum(Average)
                     ,p=sum(p),
                     Freq=sum(Freq)
                     ,Count=sum(Count))
    expCRaiDF$dVariable<-"Ceiling Raising Change Orders %"
    
    
    #Limit just to competed contracts
    varTable<-subset(varTable, Comp=="Comp.")
    
    
    expOffrDF<-QueryControlVariablesTable(varTable,
                                     "FxCb",
                                     "Ceil",
                                     "Offr"
    )
    
    expOffrDF<-ddply(expOffrDF,.(FxCb,Control,iVariable,statistic,parameter,p.value,method),transform,p=Freq/sum(Freq))
    expOffrDF$Average[expOffrDF$Offr=="1"]<-1*expOffrDF$p[expOffrDF$Offr=="1"]
    expOffrDF$Average[expOffrDF$Offr=="2"]<-2*expOffrDF$p[expOffrDF$Offr=="2"]
    expOffrDF$Average[expOffrDF$Offr=="3-4"]<-3.5*expOffrDF$p[expOffrDF$Offr=="3-4"]
    expOffrDF$Average[expOffrDF$Offr=="5+"]<-5*expOffrDF$p[expOffrDF$Offr=="5+"]
    expOffrDF<-ddply(expOffrDF,.(FxCb,Control,iVariable,statistic,parameter,p.value,method),summarise,Average=sum(Average),
                     p=sum(p),
                     Freq=sum(Freq),
                     Count=sum(Count))
    expOffrDF$dVariable<-"Average Number of Offers for Competed Contracts"
    
    
    
    OffrDF<-QueryControlVariablesTable(varTable,
                                  "FxCb",
                                  "Ceil",
                                  "Offr"
    )
    
    
    OffrDF<-ddply(OffrDF,.(FxCb,Control,iVariable,statistic,parameter,p.value,method),transform,p=Freq/sum(Freq))
    colnames(OffrDF)[colnames(OffrDF)=="Offr"]<-"dVariable"
    OffrDF<-subset(OffrDF,dVariable=="1")
    OffrDF$dVariable<-"% Single Offer Competition"
    OffrDF$Average<-NA
    ResultsDF<-rbind(TermDF,expOffrDF,OffrDF,expNChgDF,expCRaiDF)
    
    
    if(!is.na(HypothesisLabel)) ResultsDF$Hypothesis<-HypothesisLabel
    
    
ResultsDF$Significance<-cut2(ResultsDF$p.value,c(0.01,0.05))

ResultsDF$Significance<-addNA(ResultsDF$Significance,ifany=TRUE)
ResultsDF$Significance<-factor(ResultsDF$Significance,
                               exclude=NULL,
                               levels=c(levels(ResultsDF$Significance)),
                               labels=c("<0.01",
                                        "<0.05",
                                        "Not Significant\n(>0.05)",
                                        "Sample Too Small\nfor Chi-Squared"
                               )
)    


    ResultsDF
    
}
c
