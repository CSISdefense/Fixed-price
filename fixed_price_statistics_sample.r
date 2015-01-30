#*************************************Required Libraries******************************************
require(plyr)
require(grid)
require(reshape2)
require(stringr)
require(ggplot2)
# require(logging)
# debug(VariableNumericalFormat)
#*************************************Options*****************************************************
options(error=recover)
options(warn=1)
# basicConfig()
# logdebug("not shown, basic is INFO")
# logwarn("shown and timestamped")

# system("defaults write org.R-project.R force.LANG en_US.UTF-8")
# debug("CreateCSV")

# debug(apply_lookups)
# debug(CreateDuration)



#*************************************Lookup Files*****************************************************
setwd("K:\\Development\\Fixed-price")
Path<-"K:\\2007-01 PROFESSIONAL SERVICES\\R scripts and data\\"
# Path<-"~\\FPDS\\R scripts and data\\"
# Path<-"C:\\Users\\Greg Sanders\\SkyDrive\\Documents\\R Scripts and Data SkyDrive\\"



source(paste(Path,"helper.r",sep=""))
source(paste(Path,"lookups.r",sep=""))
source(paste(Path,"helper.r",sep=""))
source(paste(Path,"statistics_aggregators.r",sep=""))

options(error=recover)
options(warn=1)



# 
sample.criteria <-read.csv(
  paste(Path,"data\\defense_contract_SP_ContractSampleCriteriaDetailsCustomer.csv",sep=""),
  header=TRUE, sep=",", dec=".", strip.white=TRUE, 
  na.strings=c("NULL","NA"),
  stringsAsFactors=FALSE
)

sample.size<-15000
#Drop contracts starting before the study period
sample.criteria$LastCurrentCompletionDate<-strptime(sample.criteria$LastCurrentCompletionDate,"%Y-%m-%d") 
# as.Date(sample.criteria$LastCurrentCompletionDate)
sample.criteria<-subset(sample.criteria,StartFiscal_Year>=2000 & (LastCurrentCompletionDate<=strptime("2013-09-30","%Y-%m-%d") | IsClosed==1))
View(sample.criteria)                        

sample.SumofObligatedAmount<-sample.criteria[sample(nrow(sample.criteria)
                                                 , size=15000
                                                 , prob=abs(sample.criteria$SumofObligatedAmount)
),]

sample.SumofObligatedAmount<-subset(sample.SumofObligatedAmount,select=-c(StartFiscal_Year,SumofObligatedAmount,IsClosed))

rm(sample.criteria)

#These are a hold over from doing samples with different criteria.

# sample.SumOfbaseandexercisedoptionsvalue.gte.2007<-sample.criteria[sample(nrow(sample.criteria)
#                                                               , size=15000
#                                                               , prob=abs(sample.criteria$SumOfbaseandexercisedoptionsvalue)
# ),]
# sample.Sumofbaseandalloptionsvalue.gte.2007<-sample.criteria[sample(nrow(sample.criteria)
#                                                         , size=15000
#                                                         , prob=abs(sample.criteria$Sumofbaseandalloptionsvalue)
# ),]


#data\\defense_contract_contractdiscretization.csv
sample.SumofObligatedAmount<-read_and_join(Path
                                           ,"defense_contract_contractdiscretization.csv"
                                           ,sample.SumofObligatedAmount
                                           ,"data\\"
)

#CSIScontractID
sample.SumofObligatedAmount<-read_and_join(Path
              ,"contract_CSIScontractID.csv"
              ,sample.SumofObligatedAmount
              ,"lookups\\"
              )

# # CSIScontractID.systemequipmentcode<-subset(CSIScontractID.lookup,!is.na(systemequipmentcode))

#defense_contract_SP_ContractModificationDeltaCustomer.csv
sample.SumofObligatedAmount<-read_and_join(Path
                                           ,"defense_contract_SP_ContractModificationDeltaCustomer.csv"
                                           ,sample.SumofObligatedAmount
                                           ,"data\\"
)



#"data\\Defense_contract_SP_ContractUnmodifiedandOutcomeDetailsCustomer.csv"
sample.SumofObligatedAmount<-read_and_join(Path
                                           ,"Defense_contract_SP_ContractUnmodifiedandOutcomeDetailsCustomer.csv"
                                           ,sample.SumofObligatedAmount
                                           ,"data\\"
)


#lookups\\contract_CSIScontractID.csv
sample.SumofObligatedAmount<-read_and_join(Path
                                           ,"contract_CSIScontractID.csv"
                                           ,sample.SumofObligatedAmount
                                           ,"lookups\\"
)


#lookups\\contract_CSIScontractID.csv
sample.SumofObligatedAmount<-read_and_join(Path
                                           ,"defense_contract_SP_ContractUnmodifiedCompetitionvehicleCustomer.csv"
                                           ,sample.SumofObligatedAmount
                                           ,"data\\"
)

#Use this to add just a single file
sample.SumofObligatedAmount <-read.csv(
  paste("data\\defense_contract_CSIScontractID_sample_15000_SumofObligatedAmount.csv",sep=""),
  header=TRUE, sep=",", dec=".", strip.white=TRUE, 
  na.strings=c("NULL","NA"),
  stringsAsFactors=FALSE
)


# 
# sample.SumofObligatedAmount <-subset(sample.SumofObligatedAmount ,select=-c(NumberOfOffersReceived
#                                                                             ,IsFullAndOpen
#                                                                             ,IsSomeCompetition
# #                                                                             ,ObligatedAmountIsSomeCompetition
#                                                                             ,IsOnlyOneSource
#                                                                             ,IsFollowonToCompetedAction
#                                                                             ,multipleorsingleawardidc
#                                                                             ,addmultipleorsingawardidc
#                                                                             ,AwardOrIDVcontractactiontype
# )
# )



#defense_Contract_SP_ContractDetailsR&DCustomer.csv
sample.SumofObligatedAmount<-read_and_join(Path
                                           ,"defense_Contract_SP_ContractDetailsR&DCustomer.csv"
                                           ,sample.SumofObligatedAmount
                                           ,"data\\"
)

# subset(sample.SumofObligatedAmount,select=-c("isAnyRnD1to5","obligatedAmountRnD1to5","firstSignedDateRnD1to5","UnmodifiedRnD1to5"))
# sample.SumofObligatedAmount<-sample.SumofObligatedAmount[,!names(sample.SumofObligatedAmount) %in% c("isAnyRnD1to5","obligatedAmountRnD1to5","firstSignedDateRnD1to5","UnmodifiedRnD1to5")]

#defense_contract_SP_ContractCompetitionVehicleCustomer.csv
sample.SumofObligatedAmount<-read_and_join(Path
                                           ,"defense_contract_SP_ContractCompetitionVehicleCustomer.csv"
                                           ,sample.SumofObligatedAmount
                                           ,"data\\"
)



write.table(sample.SumofObligatedAmount
            ,file=paste("data\\defense_contract_CSIScontractID_sample_"
                        ,sample.size
                        ,"_SumofObligatedAmount.csv"
                        ,sep=""
            )
            #   ,header=TRUE
            , sep=","
            , row.names=FALSE
            , append=FALSE
)

combined.MCC<-fixed.price.statistics(Path
                                     ,sample.SumofObligatedAmount
                                     ,"MajorCommandID"
)

write.table(combined.MCC
            ,file=paste("data\\defense_office_MajorCommandID_sample_"
                        ,sample.size
                        ,"_SumofObligatedAmount.csv"
                        ,sep=""
            )
            #   ,header=TRUE
            , sep=","
            , row.names=FALSE
            , append=FALSE
)
rm(combined.MCC)


sample.SumofObligatedAmount.gte.2007<-subset(sample.SumofObligatedAmount,StartFiscal_Year>=2007)
                                      

write.table(sample.SumofObligatedAmount.gte.2007
            ,file=paste("data\\defense_contract_CSIScontractID_sample_"
                        ,sample.size
                        ,"_SumofObligatedAmount_gte_2007.csv"
                        ,sep=""
            )
            #   ,header=TRUE
            , sep=","
            , row.names=FALSE
            , append=FALSE
)

combined.MCC<-fixed.price.statistics(Path
                                     ,sample.SumofObligatedAmount.gte.2007
                                     ,"MajorCommandID"
)

write.table(combined.MCC
            ,file=paste("data\\defense_office_MajorCommandID_sample_"
                        ,sample.size
                        ,"_SumofObligatedAmount_gte_2007.csv"
                        ,sep=""
            )
            #   ,header=TRUE
            , sep=","
            , row.names=FALSE
            , append=FALSE
)
rm(combined.MCC)


sample.SumofObligatedAmount.IsCompeted<-subset(sample.SumofObligatedAmount,IsSomeCompetition==1)

write.table(sample.SumofObligatedAmount.IsCompeted
            ,file=paste("data\\defense_contract_CSIScontractID_sample_"
                        ,sample.size
                        ,"_SumofObligatedAmount_IsCompeted.csv"
                        ,sep=""
            )
            #   ,header=TRUE
            , sep=","
            , row.names=FALSE
            , append=FALSE
)

sample.SumofObligatedAmount.gte.2007.isCompeted<-subset(sample.SumofObligatedAmount.gte.2007,IsSomeCompetition==1)


write.table(sample.SumofObligatedAmount.gte.2007.isCompeted
            ,file=paste("data\\defense_contract_CSIScontractID_sample_"
                        ,sample.size
                        ,"_SumofObligatedAmount_gte_2007_isCompeted.csv"
                        ,sep=""
            )
            #   ,header=TRUE
            , sep=","
            , row.names=FALSE
            , append=FALSE
)

# sample.SumofObligatedAmount <-read.csv(
#   paste("data\\defense_contract_CSIScontractID_sample_15000_SumofObligatedAmount.csv",sep=""),
#   header=TRUE, sep=",", dec=".", strip.white=TRUE, 
#   na.strings=c("NULL","NA"),
#   stringsAsFactors=FALSE
# )

# debug(fixed.price.statistics)
# debug(apply_lookups)


# 
# sample.SumOfbaseandexercisedoptionsvalue.gte.2007<-join(
#   sample.SumOfbaseandexercisedoptionsvalue.gte.2007,
#   CSIScontractOutcomeID.lookup,
#   match="first"
# )
# 
# write.table(sample.SumOfbaseandexercisedoptionsvalue.gte.2007
#             ,file=paste(Path,"data\\defense_contract_CSIScontractID_sample"
#                         ,sample.size
#                         ,"_SumOfbaseandexercisedoptionsvalue.csv"
#                         ,sep=""
#             )
#             #   ,header=TRUE
#             , sep=","
#             , row.names=FALSE
#             , append=FALSE
# )
# 
# sample.Sumofbaseandalloptionsvalue.gte.2007<-join(
#   sample.Sumofbaseandalloptionsvalue.gte.2007,
#   CSIScontractOutcomeID.lookup,
#   match="first"
# )
# 
# write.table(sample.Sumofbaseandalloptionsvalue.gte.2007
#             ,file=paste(Path,"data\\defense_contract_CSIScontractID_sample"
#                         ,sample.size
#                         ,"_sample.Sumofbaseandalloptionsvalue.gte.2007.csv"
#                         ,sep=""
#             )
#             #   ,header=TRUE
#             , sep=","
#             , row.names=FALSE
#             , append=FALSE
# )
# 



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


#defense_contract_contractdiscretization.csv
systemequipmentlist<-read_and_join(Path
                                   ,"defense_contract_contractdiscretization.csv"
                                   ,sample.SumofObligatedAmount
                                   ,"data\\"
)



#defense_contract_SP_ContractModificationDeltaCustomer.csv
systemequipmentlist<-read_and_join(Path
                                   ,"defense_contract_SP_ContractModificationDeltaCustomer.csv"
                                   ,sample.SumofObligatedAmount
                                   ,"data\\"
)


#lookups\\contract_CSIScontractID.csv"
systemequipmentlist<-read_and_join(Path
                                   ,"contract_CSIScontractID.csv"
                                   ,sample.SumofObligatedAmount
                                   ,"lookups\\"
)

#data\\Defense_contract_SP_ContractUnmodifiedandOutcomeDetailsCustomer.csv"
systemequipmentlist<-read_and_join(Path
                                           ,"Defense_contract_SP_ContractUnmodifiedandOutcomeDetailsCustomer.csv"
                                           ,sample.SumofObligatedAmount
                                           ,"data\\"
)


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