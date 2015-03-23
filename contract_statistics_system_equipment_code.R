#System equipment code


systemequipmentlist <-read.csv(
  paste(Path,"Lookups\\Lookup_CSIScontractIDforIdentifiedSystemEquipment.csv",sep=""),
  header=TRUE, sep=",", dec=".", strip.white=TRUE, 
  na.strings="NULL",
  stringsAsFactors=FALSE
)
colnames(CSIScontractOutcomeID.lookup)[colnames(CSIScontractOutcomeID.lookup)=="ï..systemequipmentcode"]<-"systemequipmentcode"
systemequipmentlist<-read_and_join(Path,"LOOKUP_systemequipmentcode.csv",systemequipmentlist)

systemequipmentlist<-subset(systemequipmentlist,SystemEquipmentIn2000Sample==TRUE |
                              SystemEquipmentIn2007Sample==TRUE
                            
)


systemequipmentlist<-subset(systemequipmentlist,select=-c(Unseperated
                                                          ,systemequipmentcodeText
                                                          ,systemequipmentshorttext
                                                          ,SystemEquipmentIn2000Sample  
                                                          ,SystemEquipmentIn2007Sample
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

# 
# systemequipmentlist<-subset(systemequipmentlist, select=-c(IsFullAndOpen
#                                       ,IsSomeCompetition
#                                       ,IsOnlyOneSource
#                                       ,IsFollowonToCompetedAction
#                                       ,numberofoffersreceived
#                                       ,multipleorsingleawardidc
#                                       ,AwardOrIDVcontractactiontype
# )
# )

CSIScontractCompVehicleID.lookup <-read.csv(
  paste(Path,"data\\defense_contract_SP_ContractUnmodifiedCompetitionvehicleCustomer.csv",sep=""),
  header=TRUE, sep=",", dec=".", strip.white=TRUE, 
  na.strings="NULL",
  stringsAsFactors=FALSE
)




colnames(CSIScontractCompVehicleID.lookup)[colnames(CSIScontractCompVehicleID.lookup)=="ï..CSIScontractID"]<-"CSIScontractID"
systemequipmentlist<-join(
  systemequipmentlist,
  CSIScontractCompVehicleID.lookup,
  match="first"
)

rm(CSIScontractCompVehicleID.lookup)



# 
# systemequipmentlist <-read.csv(
#   paste(Path,"data\\defense_contract_CSIScontractID_sample_systemEquipmentCode.csv",sep=""),
#   header=TRUE, sep=",", dec=".", strip.white=TRUE, 
#   na.strings=c("NULL","NA"),
#   stringsAsFactors=FALSE
# )
systemequipmentlist<-read_and_join(Path,"LOOKUP_systemequipmentcode.csv",systemequipmentlist)
systemequipmentlist<-subset(systemequipmentlist,!(is.na(CSIScontractID=="NA"))

systemequipmentlist.gte.2000<-subset(systemequipmentlist,
                                     StartFiscal_Year>=2000 & SystemEquipmentIn2000Sample  ==TRUE
                                     ,select=-c(Unseperated
                                                          ,systemequipmentcodeText
                                                          ,systemequipmentshorttext
                                                          ,SystemEquipmentIn2000Sample  
                                                          ,SystemEquipmentIn2007Sample
)
)


write.table(systemequipmentlist.gte.2000
            ,file=paste(Path,"data\\defense_contract_CSIScontractID_sample_systemEquipmentCode.csv"
                        
                        ,sep=""
            )
            #   ,header=TRUE
            , sep=","
            , row.names=FALSE
            , append=FALSE
)


write.table(subset(systemequipmentlist.gte.2000,IsSomeCompetition==1)
            ,file=paste(Path,"data\\defense_contract_CSIScontractID_sample_systemEquipmentCode_IsCompeted.csv"
                        
                        ,sep=""
            )
            #   ,header=TRUE
            , sep=","
            , row.names=FALSE
            , append=FALSE
)

systemequipmentlist.gte.2007<-subset(systemequipmentlist,
                                     StartFiscal_Year>=2007 & SystemEquipmentIn2007Sample  ==TRUE
                                     ,select=-c(Unseperated
                                                ,systemequipmentcodeText
                                                ,systemequipmentshorttext
                                                ,SystemEquipmentIn2000Sample  
                                                ,SystemEquipmentIn2007Sample
                                     )
)

write.table(systemequipmentlist.gte.2007
            ,file=paste(Path,"data\\defense_contract_CSIScontractID_sample_systemEquipmentCode_gte_2007.csv"
                        #                         ,sample.size
#                                                 ,"_SumofObligatedAmount_gte_2007.csv"
                        ,sep=""
            )
            #   ,header=TRUE
            , sep=","
            , row.names=FALSE
            , append=FALSE
)


# defense_contract_SP_ContractSampleCriteriaDetailsCustomer.csv
# defense_contract_SP_ContractUnmodifiedCompetitionvehicleCustomer.csv

#Intentionally excluding for now
# contract_CSIScontractID.csv

# 
# CSIScontractID.lookup <-read.csv(
#   paste(Path,"lookups\\contract_CSIScontractID.csv",sep=""),
#   header=TRUE, sep=",", dec=".", strip.white=TRUE, 
#   na.strings="NULL",
#   stringsAsFactors=FALSE
# )
# 
# # CSIScontractID.systemequipmentcode<-subset(CSIScontractID.lookup,!is.na(systemequipmentcode))
# 
# 
# sample.SumofObligatedAmount<-join(
#   sample.SumofObligatedAmount,
#   CSIScontractID.lookup,
#   match="first"
# )

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
#                       ,data=sample.SumOfbaseandexercisedoptionsvalue.gte.2007
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