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

QueryControlVariables<-function(varGin,
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
    # querygrain(varGin, nodes=c("Offr","NChg","CRai","Term"), type="marginal")
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
    expCRaiDF$Average[expCRaiDF$CRai=="[ 0.001, 0.120)"]<-0.001*expCRaiDF$p[expCRaiDF$CRai=="[ 0.001, 0.120)"]
    expCRaiDF$Average[expCRaiDF$CRai=="[ 0.120,   Inf]"]<-0.12*expCRaiDF$p[expCRaiDF$CRai=="[ 0.120,   Inf]"]
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
    # querygrain(varGin, nodes=c("Offr","NChg","CRai","Offr"), type="marginal")
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
    ResultsDF<-dcast(ResultsDF, Control + dVariable + iVariable ~ FxCb
                     , identity
                     , value.var=c("p","Freq","Average")
    )
    ResultsDF$FixedCostMargin<-ResultsDF[,"Fixed-Price"]/TermDF[,"Cost-Based"]-1
    ResultsDF$FixedCombMargin<-ResultsDF[,"Fixed-Price"]/TermDF[,"Combination or Other"]-1
    
    
    
    ResultsDF
    
}


FixedPriceHypothesisTester<-function(varGin,HypothesisLabel=NA){
    
    TermDF<-QueryControlVariables(varGin,
                                  "FxCb",
                                  "Ceil",
                                  "Term"
    )
    
    
    TermDF<-ddply(TermDF,.(FxCb,Control,iVariable),transform,p=Freq/sum(Freq))
    # querygrain(varGin, nodes=c("Offr","NChg","CRai","Term"), type="marginal")
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
    # querygrain(varGin, nodes=c("Offr","NChg","CRai","Offr"), type="marginal")
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


QueryCrossSectionOnNode<-function(
    gin,
    PivotNode,
    QueryNode
)
{
    looper<-NULL
    for(x in unlist(gin$universe$levels[PivotNode])){
        looper<-rbind(looper,list(evidence=getEvidence(setEvidence(gin,
                                                                   nodes = c(PivotNode),
                                                                   states = c(x)
                                                                   
        )),
        result=querygrain(setEvidence(gin,
                                      nodes = c(PivotNode),
                                      states = c(x))
                          , nodes = c(QueryNode)
        )
        )
        )
    }
    looper
}
# 