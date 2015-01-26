require(ggplot2)
require(plyr)

Path<-"K:\\2007-01 PROFESSIONAL SERVICES\\R scripts and data\\"
source(paste(Path,"lookups.r",sep=""))
# 
massive.data <-read.csv(
  paste(Path,"data\\defense_contract_contractdiscretization.csv",sep=""),
  header=TRUE, sep=",", dec=".", strip.white=TRUE, 
  na.strings="NULL",
  stringsAsFactors=FALSE
)

sample.size<-15000

massive.data$SumOfbaseandexercisedoptionsvalue<-FactorToNumber(massive.data$SumOfbaseandexercisedoptionsvalue)

sample.SumofObligatedAmount<-massive.data[sample(nrow(massive.data)
                                                 , size=15000
                                                 , prob=abs(massive.data$SumofObligatedAmount)
),]
massive.data<-subset(massive.data,!is.na(SumOfbaseandexercisedoptionsvalue)&!is.na(Sumofbaseandalloptionsvalue))

sample.SumOfbaseandexercisedoptionsvalue<-massive.data[sample(nrow(massive.data)
                                                              , size=15000
                                                              , prob=abs(massive.data$SumOfbaseandexercisedoptionsvalue)
),]
sample.Sumofbaseandalloptionsvalue<-massive.data[sample(nrow(massive.data)
                                                        , size=15000
                                                        , prob=abs(massive.data$Sumofbaseandalloptionsvalue)
),]
rm(massive.data)


CSIScontractID.lookup <-read.csv(
  paste(Path,"lookups\\contract_CSIScontractID.csv",sep=""),
  header=TRUE, sep=",", dec=".", strip.white=TRUE, 
  na.strings="NULL",
  stringsAsFactors=FALSE
)

# CSIScontractID.systemequipmentcode<-subset(CSIScontractID.lookup,!is.na(systemequipmentcode))


sample.SumofObligatedAmount<-join(
  sample.SumofObligatedAmount,
  CSIScontractID.lookup,
  match="first"
)

sample.SumOfbaseandexercisedoptionsvalue<-join(
  sample.SumOfbaseandexercisedoptionsvalue,
  CSIScontractID.lookup,
  match="first"
)

sample.Sumofbaseandalloptionsvalue<-join(
  sample.Sumofbaseandalloptionsvalue,
  CSIScontractID.lookup,
  match="first"
)


rm(CSIScontractID.lookup)      




CSIScontractID.delta <-read.csv(
  paste(Path,"data\\defense_contract_SP_ContractModificationDeltaCustomer.csv",sep=""),
  header=TRUE, sep=",", dec=".", strip.white=TRUE, 
  na.strings="NULL",
  stringsAsFactors=FALSE
)

# CSIScontractID.systemequipmentcode<-subset(CSIScontractID.delta,!is.na(systemequipmentcode))


sample.SumofObligatedAmount<-join(
  sample.SumofObligatedAmount,
  CSIScontractID.delta,
  match="first"
)

sample.SumOfbaseandexercisedoptionsvalue<-join(
  sample.SumOfbaseandexercisedoptionsvalue,
  CSIScontractID.delta,
  match="first"
)

sample.Sumofbaseandalloptionsvalue<-join(
  sample.Sumofbaseandalloptionsvalue,
  CSIScontractID.delta,
  match="first"
)


rm(CSIScontractID.delta)  

CSIScontractOutcomeID.lookup <-read.csv(
  paste(Path,"data\\Defense_contract_SP_ContractUnmodifiedandOutcomeDetailsCustomer.csv",sep=""),
  header=TRUE, sep=",", dec=".", strip.white=TRUE, 
  na.strings="NULL",
  stringsAsFactors=FALSE
)

# CSIScontractOutcomeID.systemequipmentcode<-subset(CSIScontractOutcomeID.lookup,!is.na(systemequipmentcode))

colnames(CSIScontractOutcomeID.lookup)[colnames(CSIScontractOutcomeID.lookup)=="ï..CSIScontractID"]<-"CSIScontractID"
sample.SumofObligatedAmount<-join(
  sample.SumofObligatedAmount,
  CSIScontractOutcomeID.lookup,
  match="first"
)

write.table(sample.SumofObligatedAmount
            ,file=paste(Path,"data\\defense_contract_CSIScontractID_sample"
                        ,sample.size
                        ,"_SumofObligatedAmount.csv"
                        ,sep=""
            )
            #   ,header=TRUE
            , sep=","
            , row.names=FALSE
            , append=FALSE
)

sample.SumOfbaseandexercisedoptionsvalue<-join(
  sample.SumOfbaseandexercisedoptionsvalue,
  CSIScontractOutcomeID.lookup,
  match="first"
)

write.table(sample.SumOfbaseandexercisedoptionsvalue
            ,file=paste(Path,"data\\defense_contract_CSIScontractID_sample"
                        ,sample.size
                        ,"_SumOfbaseandexercisedoptionsvalue.csv"
                        ,sep=""
            )
            #   ,header=TRUE
            , sep=","
            , row.names=FALSE
            , append=FALSE
)

sample.Sumofbaseandalloptionsvalue<-join(
  sample.Sumofbaseandalloptionsvalue,
  CSIScontractOutcomeID.lookup,
  match="first"
)

write.table(sample.Sumofbaseandalloptionsvalue
            ,file=paste(Path,"data\\defense_contract_CSIScontractID_sample"
                        ,sample.size
                        ,"_sample.Sumofbaseandalloptionsvalue.csv"
                        ,sep=""
            )
            #   ,header=TRUE
            , sep=","
            , row.names=FALSE
            , append=FALSE
)


rm(CSIScontractOutcomeID.lookup)


#System equipment code


systemequipmentlist <-read.csv(
  paste(Path,"Lookups\\Lookup_CSIScontractIDforIdentifiedSystemEquipment.csv",sep=""),
  header=TRUE, sep=",", dec=".", strip.white=TRUE, 
  na.strings="NULL",
  stringsAsFactors=FALSE
)

systemequipmentlist<-read_and_join(Path,"LOOKUP_systemequipmentcode.csv",systemequipmentlist)

systemequipmentlist<-subset(systemequipmentlist,SystemEquipmentInSample=TRUE

)

systemequipmentlist<-subset(systemequipmentlist,select=-c(Unseperated
                                                          ,systemequipmentcodeText
                                                          ,systemequipmentshorttext
                                                          ,SystemEquipmentInSample
)
)

massive.data <-read.csv(
  paste(Path,"data\\defense_contract_contractdiscretization.csv",sep=""),
  header=TRUE, sep=",", dec=".", strip.white=TRUE, 
  na.strings="NULL",
  stringsAsFactors=FALSE
)



systemequipmentlist<-join(
  systemequipmentlist,
  massive.data,
  match="first"
)

rm(massive.data)




CSIScontractID.delta <-read.csv(
  paste(Path,"data\\defense_contract_SP_ContractModificationDeltaCustomer.csv",sep=""),
  header=TRUE, sep=",", dec=".", strip.white=TRUE, 
  na.strings="NULL",
  stringsAsFactors=FALSE
)

# CSIScontractID.systemequipmentcode<-subset(CSIScontractID.delta,!is.na(systemequipmentcode))


systemequipmentlist<-join(
  systemequipmentlist,
  CSIScontractID.delta,
  match="first"
)


rm(CSIScontractID.delta)  

CSIScontractID.lookup <-read.csv(
  paste(Path,"lookups\\contract_CSIScontractID.csv",sep=""),
  header=TRUE, sep=",", dec=".", strip.white=TRUE, 
  na.strings="NULL",
  stringsAsFactors=FALSE
)

# CSIScontractID.lookup<=subset(CSIScontractID.lookup,select=-c(systemequipmentcode))

systemequipmentlist<-join(
  systemequipmentlist,
  CSIScontractID.lookup,
  match="first"
)

rm(CSIScontractID.lookup)

CSIScontractOutcomeID.lookup <-read.csv(
  paste(Path,"data\\Defense_contract_SP_ContractUnmodifiedandOutcomeDetailsCustomer.csv",sep=""),
  header=TRUE, sep=",", dec=".", strip.white=TRUE, 
  na.strings="NULL",
  stringsAsFactors=FALSE
)

colnames(CSIScontractOutcomeID.lookup)[colnames(CSIScontractOutcomeID.lookup)=="ï..CSIScontractID"]<-"CSIScontractID"
systemequipmentlist<-join(
  systemequipmentlist,
  CSIScontractOutcomeID.lookup,
  match="first"
)

rm(CSIScontractOutcomeID.lookup)


write.table(systemequipmentlist
            ,file=paste(Path,"data\\defense_contract_CSIScontractID_"
#                         ,sample.size
                        ,"_systemEquipmentCode.csv"
                        ,sep=""
            )
            #   ,header=TRUE
            , sep=","
            , row.names=FALSE
            , append=FALSE
)

# 
# #"defense_SumofObligatedAmount_SumOfbaseandexercisedoptionsvalue_StartFiscal_Year_exercisedweighted"
# 
# SpentvExpected<-qplot(log10(SumofObligatedAmount)
#                       ,log10(SumOfbaseandexercisedoptionsvalue)
#                       ,data=sample.SumofObligatedAmount
#                       ,color=IsIDV
# )+geom_abline(slope=1)+geom_abline(slope=1,intercept=3)+facet_wrap(StartFiscal_Year~IsIDV)
# 
# png(
#   paste(Path
#         
#         ,"defense_SumofObligatedAmount_SumOfbaseandexercisedoptionsvalue_StartFiscal_Year_obligationweighted"
#         ,".png"
#         , sep=""
#   )
#   , width=6#VAR.width
#   , height=6#VAR.height
#   , units='in'
#   , res=300
# )
# 
# print(SpentvExpected)
# 
# if (!(dev.cur()[[1]]==1)){
#   dev.off()
# }
# 
# #"defense_SumofObligatedAmount_Sumofbaseandalloptionsvalue_StartFiscal_Year_obligationweighted"
# SpentvExpected<-qplot(log10(SumofObligatedAmount)
#                       ,log10(Sumofbaseandalloptionsvalue)
#                       ,data=sample.SumofObligatedAmount
#                       ,color=IsIDV
# )+geom_abline(slope=1)+geom_abline(slope=1,intercept=3)+facet_wrap(StartFiscal_Year~IsIDV)
# 
# 
# png(
#   paste(Path
#         
#         ,"defense_SumofObligatedAmount_Sumofbaseandalloptionsvalue_StartFiscal_Year_obligationweighted"
#         ,".png"
#         , sep=""
#   )
#   , width=6#VAR.width
#   , height=6#VAR.height
#   , units='in'
#   , res=300
# )
# 
# print(SpentvExpected)
# 
# if (!(dev.cur()[[1]]==1)){
#   dev.off()
# }
# 
# 
# 
# #"defense_SumofObligatedAmount_SumOfbaseandexercisedoptionsvalue_StartFiscal_Year_exercisedweighted"
# SpentvExpected<-qplot(log10(SumofObligatedAmount)
#                       ,log10(SumOfbaseandexercisedoptionsvalue)
#                       ,data=sample.SumOfbaseandexercisedoptionsvalue
#                       ,color=IsIDV
# )+geom_abline(slope=1)+geom_abline(slope=1,intercept=3)+facet_wrap(StartFiscal_Year~IsIDV)
# 
# 
# png(
#   paste(Path
#         
#         ,"defense_SumofObligatedAmount_SumOfbaseandexercisedoptionsvalue_StartFiscal_Year_exercisedweighted"
#         ,".png"
#         , sep=""
#   )
#   , width=6#VAR.width
#   , height=6#VAR.height
#   , units='in'
#   , res=300
# )
# 
# print(SpentvExpected)
# 
# if (!(dev.cur()[[1]]==1)){
#   dev.off()
# }
# 
# 
# #"defense_SumofObligatedAmount_Sumofbaseandalloptionsvalue_StartFiscal_Year_allweighted"
# SpentvExpected<-qplot(log10(SumofObligatedAmount)
#                       ,log10(Sumofbaseandalloptionsvalue)
#                       ,data=massive.data[sample(nrow(massive.data), size=15000, prob=abs(massive.data$Sumofbaseandalloptionsvalue)),]
#                       ,color=IsIDV
# )+geom_abline(slope=1)+geom_abline(slope=1,intercept=3)+facet_wrap(StartFiscal_Year~IsIDV)
# 
# 
# png(
#   paste(Path
#         
#         ,"defense_SumofObligatedAmount_Sumofbaseandalloptionsvalue_StartFiscal_Year_allweighted"
#         ,".png"
#         , sep=""
#   )
#   , width=6#VAR.width
#   , height=6#VAR.height
#   , units='in'
#   , res=300
# )
# 
# print(SpentvExpected)
# 
# if (!(dev.cur()[[1]]==1)){
#   dev.off()
# }