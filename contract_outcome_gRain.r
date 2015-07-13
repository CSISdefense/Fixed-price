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
    
    #IDV contracts by ceiling
    ResultsDF<-rbind(ResultsDF,StandardizeGRainQuery(
        setEvidence(varGin, 
                    nodes=c("IDV"),
                    states=c("IDV")),
        studyVaruableCol,iVariableCol,dVariableCol,resultLabel="IDV")
    )
    
    ResultsDF<-rbind(ResultsDF,StandardizeGRainQuery(
        setEvidence(varGin, 
                    nodes=c("IDV"),
                    states=c(list(
                        compGin[[1]]$levels$IDV[compGin[[1]]$levels$IDV!=
                                                    "IDV"]))),
        studyVaruableCol,iVariableCol,dVariableCol,resultLabel="Award")
    )
    
    
    
    #LongDur contracts by ceiling
    ResultsDF<-rbind(ResultsDF,StandardizeGRainQuery(
        setEvidence(varGin, 
                    nodes=c("Dur"),
                    states=c("[  366,33192]")
        ),
        studyVaruableCol,iVariableCol,dVariableCol,resultLabel="Long Dur.")
    )
    
    ResultsDF<-rbind(ResultsDF,StandardizeGRainQuery(
        setEvidence(varGin, 
                    nodes=c("Dur"),
                    states=c(list(
                        varGin[[1]]$levels$Dur[varGin[[1]]$levels$Dur!=
                                                   "[  366,33192]"]))
        ),
        studyVaruableCol,iVariableCol,dVariableCol,resultLabel="Not Long Dur.")
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
                                            #                     "R&D",
                                            "Long Dur.",
                                            "Not Long Dur.",
                                            "IDV",
                                            "Award",
                                            "Comp.",
                                            "No Comp.",
                                            "Aircraft",
                                            "Not Aircraft")),
                               ordered=TRUE
    )
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
    TermDF$FixedCombMargin<-TermDF[,"Fixed-Price"]/TermDF[,"Combination \nor Other"]
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
    expNChgDF$dVariable<-"Expected Number of Changes"
    expNChgDF$FixedCostMargin<-expNChgDF[,"Fixed-Price"]/expNChgDF[,"Cost-Based"]
    expNChgDF$FixedCombMargin<-expNChgDF[,"Fixed-Price"]/expNChgDF[,"Combination \nor Other"]
    
    
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
    expCRaiDF$FixedCombMargin<-expCRaiDF[,"Fixed-Price"]/expCRaiDF[,"Combination \nor Other"]
    
    

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
    expOffrDF$dVariable<-"Expected Number of Offers"
    expOffrDF$FixedCostMargin<-expOffrDF[,"Fixed-Price"]/expOffrDF[,"Cost-Based"]
    expOffrDF$FixedCombMargin<-expOffrDF[,"Fixed-Price"]/expOffrDF[,"Combination \nor Other"]
    
    
    
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
    OffrDF$FixedCombMargin<-OffrDF[,"Fixed-Price"]/OffrDF[,"Combination \nor Other"]
    colnames(OffrDF)[colnames(OffrDF)=="Offr"]<-"dVariable"
    OffrDF<-subset(OffrDF,dVariable=="1")
    
    ResultsDF<-rbind(TermDF,expOffrDF,OffrDF,expNChgDF,expCRaiDF)


    if(!is.na(HypothesisLabel)) ResultsDF$Hypothesis<-HypothesisLabel
    
    #Order the ceilings
    ResultsDF$iVariable<-factor(ResultsDF$iVariable,
                                levels=c("[30m+]",
                                         "[1m,30m)",
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