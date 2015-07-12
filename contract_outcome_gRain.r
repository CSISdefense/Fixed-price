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
    ResultsDF<-querygrain(varGin,
                          nodes=c(studyVariableCol,iVariableCol,dVariableCol),
                          type="joint",result="data.frame")
    colnames(ResultsDF)[colnames(ResultsDF)==iVariableCol]<-"iVariable"
    if(!is.na(resultLabel)){
#         ResultsDF$iVariable<-factor(ResultsDF$iVariable,levels=levels(ResultsDF$iVariable),
#                                     labels=paste(resultLabel,levels(ResultsDF$iVariable)))
        ResultsDF$CrossTab<-resultLabel
    }
    else     ResultsDF$CrossTab<-"Overall"
    ResultsDF
}

TerminationHypothesisTester<-function(varGin,ControlLabel=NA){
    #All contracts by ceiling
    ResultsDF<-StandardizeGRainQuery(varGin,"FxCb","Ceil","Term")
    
    
    #R&D contracts by ceiling    
#     ResultsDF<-rbind(ResultsDF,StandardizeGRainQuery(
#         setEvidence(varGin, 
#                     nodes=c("PSR"),
#                     states=c("R&D")),
#         "FxCb","Ceil","Term",resultLabel="R&D")
#     )
    #   
    #Aircraft contracts by ceiling
    ResultsDF<-rbind(ResultsDF,StandardizeGRainQuery(
        setEvidence(varGin, 
                    nodes=c("What"),
                    states=c("Aircraft and Drones")),
        "FxCb","Ceil","Term",resultLabel="Aircraft")
    )
    
    ResultsDF<-rbind(ResultsDF,StandardizeGRainQuery(
        setEvidence(varGin, 
                    nodes=c("What"),
                    states=list(varGin[[1]]$levels$What[
                        varGin[[1]]$levels$What!="Aircraft and Drones"])),
        "FxCb","Ceil","Term",resultLabel="Not Aircraft")
    )
    
#IDV contracts by ceiling
ResultsDF<-rbind(ResultsDF,StandardizeGRainQuery(
    setEvidence(varGin, 
                nodes=c("IDV"),
                states=c("IDV")),
    "FxCb","Ceil","Term",resultLabel="IDV")
)

ResultsDF<-rbind(ResultsDF,StandardizeGRainQuery(
    setEvidence(varGin, 
                nodes=c("IDV"),
                states=c(list(
                    compGin[[1]]$levels$IDV[compGin[[1]]$levels$IDV!=
                                                "IDV"]))),
    "FxCb","Ceil","Term",resultLabel="Award")
)



    #LongDur contracts by ceiling
    ResultsDF<-rbind(ResultsDF,StandardizeGRainQuery(
        setEvidence(varGin, 
                    nodes=c("Dur"),
                    states=c("[  366,33192]")
        ),
        "FxCb","Ceil","Term",resultLabel="Long Dur.")
    )
    
    ResultsDF<-rbind(ResultsDF,StandardizeGRainQuery(
        setEvidence(varGin, 
                    nodes=c("Dur"),
                    states=c(list(
                        varGin[[1]]$levels$Dur[varGin[[1]]$levels$Dur!=
                                                    "[  366,33192]"]))
        ),
        "FxCb","Ceil","Term",resultLabel="Not Long Dur.")
    )
    
    
#Contracts competition by ceiling
ResultsDF<-rbind(ResultsDF,StandardizeGRainQuery(
    setEvidence(varGin, 
                nodes=c("Comp"),
                states=c("Comp.")
    ),
    "FxCb","Ceil","Term",resultLabel="Comp.")
)

ResultsDF<-rbind(ResultsDF,StandardizeGRainQuery(
    setEvidence(varGin, 
                    nodes=c("Comp"),
                    states=c("No Comp.")
    ),
    "FxCb","Ceil","Term",resultLabel="No Comp.")
)


    
    
    
    
    ResultsDF<-ddply(ResultsDF,.(FxCb,CrossTab,iVariable),transform,p=Freq/sum(Freq))
    # querygrain(varGin, nodes=c("Offr","NChg","CRai","Term"), type="marginal")
    ResultsDF<-dcast(ResultsDF,Term + CrossTab + iVariable ~ FxCb, sum, value.var="p")
    ResultsDF$FixedCostMargin<-ResultsDF[,"Fixed-Price"]/ResultsDF[,"Cost-Based"]
    ResultsDF$FixedCombMargin<-ResultsDF[,"Fixed-Price"]/ResultsDF[,"Combination \nor Other"]
    colnames(ResultsDF)[colnames(ResultsDF)=="Term"]<-"dVariable"
    ResultsDF<-subset(ResultsDF,dVariable=="Terminated")
    
    
    if(!is.na(ControlLabel)) ResultsDF$Control<-ControlLabel
    
    
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
    
    #Order the crosstabs, reversed is to put the first entry on top.
    ResultsDF$CrossTab<-factor(ResultsDF$CrossTab,
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


ChangeOrdersHypothesisTester<-function(varGin){
    
    expNChgDF<-querygrain(varGin,nodes=c("FxCb","Ceil","NChg"), type="joint",result="data.frame")
    colnames(expNChgDF)[colnames(expNChgDF)=="Ceil"]<-"iVariable"
    
    
    
    #All contracts by PSR
    NewDF<-querygrain(varGin,
                      nodes=c("FxCb","PSR","NChg"), type="joint",result="data.frame")
    colnames(NewDF)[colnames(NewDF)=="PSR"]<-"iVariable"
    NChgDF<-rbind(NChgDF,NewDF)
    
    #Small contracts by PSR
    NewDF<-querygrain(setEvidence(varGin, 
                                  nodes=c("Ceil"),
                                  states=c(list(c("[0,15k)","[15k,100k)")))
    ),
    nodes=c("FxCb","PSR","NChg"), type="joint",result="data.frame")
    colnames(NewDF)[colnames(NewDF)=="PSR"]<-"iVariable"
    NewDF$iVariable<-factor(NewDF$iVariable,levels=c("Products","R&D","Services"),
                            labels=c("Products <100K","R&D <100K","Services <100K"))
    NChgDF<-rbind(NChgDF,NewDF)
    #     NewDF<-querygrain(varGin,nodes=c("FxCb","PSR","NChg"), type="joint",result="data.frame")
    #     colnames(NewDF)[colnames(NewDF)=="PSR"]<-"iVariable"
    #     expNChgDF<-rbind(expNChgDF,NewDF)
    #     
    expNChgDF<-ddply(expNChgDF,.(FxCb,iVariable),transform,p=Freq/sum(Freq))
    expNChgDF$expNChg[expNChgDF$NChg=="   0"]<-0
    expNChgDF$expNChg[expNChgDF$NChg=="   1"]<-1*expNChgDF$p[expNChgDF$NChg=="   1"]
    expNChgDF$expNChg[expNChgDF$NChg=="   2"]<-2*expNChgDF$p[expNChgDF$NChg=="   2"]
    expNChgDF$expNChg[expNChgDF$NChg=="[   3,1040]"]<-3*expNChgDF$p[expNChgDF$NChg=="[   3,1040]"]
    expNChgDF<-ddply(expNChgDF,.(FxCb,iVariable),summarise,expNChg=sum(expNChg))
    expNChgDF<-dcast(expNChgDF,  iVariable ~ FxCb  , sum, value.var="expNChg")
    expNChgDF$dVariable<-"Expected Number of Changes"
    expNChgDF$FixedCostMargin<-expNChgDF[,"Fixed-Price"]/expNChgDF[,"Cost-Based"]
    expNChgDF$FixedCombMargin<-expNChgDF[,"Fixed-Price"]/expNChgDF[,"Combination \nor Other"]
    
    
    expCRaiDF<-querygrain(varGin,nodes=c("FxCb","Ceil","CRai"), type="joint",result="data.frame")
    colnames(expCRaiDF)[colnames(expCRaiDF)=="Ceil"]<-"iVariable"
    NewDF<-querygrain(varGin,nodes=c("FxCb","PSR","CRai"), type="joint",result="data.frame")
    colnames(NewDF)[colnames(NewDF)=="PSR"]<-"iVariable"
    expCRaiDF<-rbind(expCRaiDF,NewDF)
    
    
    expCRaiDF<-ddply(expCRaiDF,.(FxCb,iVariable),transform,p=Freq/sum(Freq))
    expCRaiDF$expCRai[expCRaiDF$CRai=="[-0.001, 0.001)"]<-0
    expCRaiDF$expCRai[expCRaiDF$CRai=="[  -Inf,-0.001)"]<- -0.001*expCRaiDF$p[expCRaiDF$CRai=="[  -Inf,-0.001)"]
    expCRaiDF$expCRai[expCRaiDF$CRai=="[ 0.001, 0.120)"]<-0.001*expCRaiDF$p[expCRaiDF$CRai=="[ 0.001, 0.120)"]
    expCRaiDF$expCRai[expCRaiDF$CRai=="[ 0.120,   Inf]"]<-0.12*expCRaiDF$p[expCRaiDF$CRai=="[ 0.120,   Inf]"]
    expCRaiDF<-ddply(expCRaiDF,.(FxCb,iVariable),summarise,expCRai=sum(expCRai))
    expCRaiDF<-dcast(expCRaiDF, iVariable ~ FxCb, sum, value.var="expCRai")
    expCRaiDF$dVariable<-"Ceiling Raising Change Orders %"
    expCRaiDF$FixedCostMargin<-expCRaiDF[,"Fixed-Price"]/expCRaiDF[,"Cost-Based"]
    expCRaiDF$FixedCombMargin<-expCRaiDF[,"Fixed-Price"]/expCRaiDF[,"Combination \nor Other"]
    rbind(expNChgDF,expCRaiDF)
}    

OffersHypothesisTester<-function(varGin){
    
    expOffrDF<-querygrain(varGin,nodes=c("FxCb","Ceil","Offr"), type="joint",result="data.frame")
    colnames(expOffrDF)[colnames(expOffrDF)=="Ceil"]<-"iVariable"
    #     NewDF<-querygrain(varGin,nodes=c("FxCb","PSR","Offr"), type="joint",result="data.frame")
    #     colnames(NewDF)[colnames(NewDF)=="PSR"]<-"iVariable"
    #     expOffrDF<-rbind(expOffrDF,NewDF)
    #     
    
    
    
    expOffrDF<-ddply(expOffrDF,.(FxCb,iVariable),transform,p=Freq/sum(Freq))
    expOffrDF$expOffr[expOffrDF$Offr=="1"]<-1*expOffrDF$p[expOffrDF$Offr=="1"]
    expOffrDF$expOffr[expOffrDF$Offr=="2"]<-2*expOffrDF$p[expOffrDF$Offr=="2"]
    expOffrDF$expOffr[expOffrDF$Offr=="3-4"]<-3.5*expOffrDF$p[expOffrDF$Offr=="3-4"]
    expOffrDF$expOffr[expOffrDF$Offr=="5+"]<-5*expOffrDF$p[expOffrDF$Offr=="5+"]
    expOffrDF<-ddply(expOffrDF,.(FxCb,iVariable),summarise,expOffr=sum(expOffr))
    expOffrDF<-dcast(expOffrDF,  iVariable ~ FxCb  , sum, value.var="expOffr")
    expOffrDF$dVariable<-"Expected Number of Offers"
    expOffrDF$FixedCostMargin<-expOffrDF[,"Fixed-Price"]/expOffrDF[,"Cost-Based"]
    expOffrDF$FixedCombMargin<-expOffrDF[,"Fixed-Price"]/expOffrDF[,"Combination \nor Other"]
    
    
    
    
    
    OffrDF<-querygrain(setEvidence(varGin, 
                                   nodes=c("Comp"),
                                   states=c("Comp.")
    ),
    nodes=c("FxCb","Ceil","Offr"), type="joint",result="data.frame")
    colnames(OffrDF)[colnames(OffrDF)=="Ceil"]<-"iVariable"
    
    
    
    
    # NewDF<-querygrain(setEvidence(varGin, 
    #                                   nodes=c("Comp"),
    #                                   states=c("Comp.")
    #     ),
    #     nodes=c("FxCb","PSR","Offr"), type="joint",result="data.frame")
    #     colnames(NewDF)[colnames(NewDF)=="PSR"]<-"iVariable"
    #     OffrDF<-rbind(OffrDF,NewDF)
    
    colnames(OffrDF)[colnames(OffrDF)=="Ceil"]<-"iVariable"
    OffrDF<-ddply(OffrDF,.(FxCb,iVariable),transform,p=Freq/sum(Freq))
    # querygrain(varGin, nodes=c("Offr","NChg","CRai","Offr"), type="marginal")
    OffrDF<-dcast(OffrDF,Offr + iVariable ~ FxCb, sum, value.var="p")
    OffrDF$FixedCostMargin<-OffrDF[,"Fixed-Price"]/OffrDF[,"Cost-Based"]
    OffrDF$FixedCombMargin<-OffrDF[,"Fixed-Price"]/OffrDF[,"Combination \nor Other"]
    colnames(OffrDF)[colnames(OffrDF)=="Offr"]<-"dVariable"
    OffrDF<-subset(OffrDF,dVariable=="1")
    
    rbind(expOffrDF,OffrDF)
    
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