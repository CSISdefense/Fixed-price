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

# sample.SumOfbaseandexercisedoptionsvalue.gte.2007<-sample.criteria[sample(nrow(sample.criteria)
#                                                               , size=15000
#                                                               , prob=abs(sample.criteria$SumOfbaseandexercisedoptionsvalue)
# ),]
# sample.Sumofbaseandalloptionsvalue.gte.2007<-sample.criteria[sample(nrow(sample.criteria)
#                                                         , size=15000
#                                                         , prob=abs(sample.criteria$Sumofbaseandalloptionsvalue)
# ),]

massive.data <-read.csv(
  paste(Path,"data\\defense_contract_contractdiscretization.csv",sep=""),
  header=TRUE, sep=",", dec=".", strip.white=TRUE, 
  na.strings="NULL",
  stringsAsFactors=FALSE
)

massive.data$SumOfbaseandexercisedoptionsvalue<-FactorToNumber(massive.data$SumOfbaseandexercisedoptionsvalue)
sample.SumofObligatedAmount<-join(
  sample.SumofObligatedAmount,
  massive.data,
  match="first"
)

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


# sample.SumOfbaseandexercisedoptionsvalue.gte.2007<-join(
#   sample.SumOfbaseandexercisedoptionsvalue.gte.2007,
#   CSIScontractID.lookup,
#   match="first"
# )
# 
# sample.Sumofbaseandalloptionsvalue.gte.2007<-join(
#   sample.Sumofbaseandalloptionsvalue.gte.2007,
#   CSIScontractID.lookup,
#   match="first"
# )


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


# sample.SumOfbaseandexercisedoptionsvalue.gte.2007<-join(
#   sample.SumOfbaseandexercisedoptionsvalue.gte.2007,
#   CSIScontractID.delta,
#   match="first"
# )
# 
# sample.Sumofbaseandalloptionsvalue.gte.2007<-join(
#   sample.Sumofbaseandalloptionsvalue.gte.2007,
#   CSIScontractID.delta,
#   match="first"
# )


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

rm(CSIScontractOutcomeID.lookup)


CSIScontractCompVeh.lookup <-read.csv(
  paste(Path,"data\\defense_contract_SP_ContractUnmodifiedCompetitionvehicleCustomer.csv",sep=""),
  header=TRUE, sep=",", dec=".", strip.white=TRUE, 
  na.strings="NULL",
  stringsAsFactors=FALSE
)

sample.SumofObligatedAmount <-subset(sample.SumofObligatedAmount ,select=-c(IsSomeCompetition
                                                                            , IsFullAndOpen
#                                                                             ,IsOnlyOneSource
#                                                                             ,IsFollowonToCompetedAction
#                                                                             ,numberofoffersreceived
#                                                                             ,multipleorsingleawardidc
#                                                                             ,AwardOrIDVcontractactiontype
)
                                     )


# CSIScontractOutcomeID.systemequipmentcode<-subset(CSIScontractCompVeh.lookup,!is.na(systemequipmentcode))

sample.SumofObligatedAmount<-join(
  sample.SumofObligatedAmount,
  CSIScontractCompVeh.lookup,
  match="first"
)

rm(CSIScontractCompVeh.lookup)
# 
# sample.SumofObligatedAmount <-read.csv(
#   paste(Path,"data\\defense_contract_CSIScontractID_sample_15000_SumofObligatedAmount.csv",sep=""),
#   header=TRUE, sep=",", dec=".", strip.white=TRUE, 
#   na.strings=c("NULL","NA"),
#   stringsAsFactors=FALSE
# )



write.table(sample.SumofObligatedAmount
            ,file=paste(Path,"data\\defense_contract_CSIScontractID_sample_"
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
            ,file=paste(Path,"data\\defense_office_MajorCommandID_sample_"
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
            ,file=paste(Path,"data\\defense_contract_CSIScontractID_sample_"
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
            ,file=paste(Path,"data\\defense_office_MajorCommandID_sample_"
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
            ,file=paste(Path,"data\\defense_contract_CSIScontractID_sample_"
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
            ,file=paste(Path,"data\\defense_contract_CSIScontractID_sample_"
                        ,sample.size
                        ,"_SumofObligatedAmount_gte_2007_isCompeted.csv"
                        ,sep=""
            )
            #   ,header=TRUE
            , sep=","
            , row.names=FALSE
            , append=FALSE
)

sample.SumofObligatedAmount <-read.csv(
  paste(Path,"data\\defense_contract_CSIScontractID_sample_15000_SumofObligatedAmount.csv",sep=""),
  header=TRUE, sep=",", dec=".", strip.white=TRUE, 
  na.strings=c("NULL","NA"),
  stringsAsFactors=FALSE
)

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



