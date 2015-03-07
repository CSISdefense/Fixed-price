#title: "Interaction"
#author: "Greg Sanders"
#date: "Tuesday, March 03, 2015"

Interlinked  <- read.csv(
    "C:\\Users\\Greg Sanders\\Documents\\Development\\Fixed-price\\Data\\Office_cachedCSIScontractIDtoContractingOfficeIDlikelyInterlinkedPlatform.csv",
    header = TRUE, sep = ",", dec = ".", strip.white = TRUE, 
    na.strings = c("NULL","NA",""),
    stringsAsFactors = TRUE
)
#Reassign the name of CSIScontractID
colnames(Interlinked)[colnames(Interlinked)=="ï..CSIScontractID"]<-"CSIScontractID"

#Eliminate missing values. If this eliminates contracts entirely, that's okay, that means they've got no interlinked
Interlinked<-subset(Interlinked,!is.na(PlatformPortfolio)&
                        !is.na(ContractingOfficeID)&
                        !is.na(MinofSignedDate))

#Rather than deal with duplicate counting, I'm just treating contracts with 
#multiple contracting office and platform portfolio dyads as fractional. The more
#you link with them, the more likely it is you're interrelated
Interlinked$InverseVariants<-1/Interlinked$Variants

#Convert the date columns from text to date
Interlinked$MinofSignedDate<-strptime(Interlinked$MinofSignedDate,"%Y-%m-%d")
Interlinked$LastUltimateCompletionDate<-strptime(Interlinked$LastUltimateCompletionDate,"%Y-%m-%d")

DateCutoff<-strptime("2006-10-01","%Y-%m-%d")

#Drop all contracts that concluded before 
Interlinked<-subset(Interlinked,is.na(Interlinked$LastUltimateCompletionDate)|
                        Interlinked$LastUltimateCompletionDate>=DateCutoff)
write.table(Interlinked
            ,file=   "C:\\Users\\Greg Sanders\\Documents\\Development\\Fixed-price\\Data\\Office_cleanedCSIScontractIDtoContractingOfficeIDlikelyInterlinkedPlatform.csv"
            , sep=","
            , row.names=FALSE
            , append=FALSE
)

#END preprocessing I've alredy done


#Start the bit I can't run in R absent serious optimization
Interlinked  <- read.csv(
    "C:\\Users\\Greg Sanders\\Documents\\Development\\Fixed-price\\Data\\Office_cleanedCSIScontractIDtoContractingOfficeIDlikelyInterlinkedPlatform.csv",
    header = TRUE, sep = ",", dec = ".", strip.white = TRUE, 
    na.strings = c("NULL","NA",""),
    stringsAsFactors = TRUE
)




#Create a subset of only contracts that don't have any system equipment codes
InterlinkedNoSEC<-subset(interlinked,is.na(AnyIdentifiedSystemEquipment)|AnyIdentifiedSystemEquipment==0)

#The big loop 
for(i in i..nrow(Interlinked)){
    #Skip rows that start prior to 10/1/2006
    if(Interlinked[i,"MinofSignedDate"]>=DateCutoff)
    #If you've got any systemequipment codes, only compare with those contracts without.
    #No self matches for this set.
    if(Interlinked[i,"AnyIdentifiedSystemEquipment"]==1){
        Interlinked$InverselinkedCount[i]<-sum(InterlinkedNoSEC$InverseVariants[
            InterlinkedNoSEC$ContractingOfficeID==Interlinked[i,"ContractingOfficeID"]&
                InterlinkedNoSEC$PlatformPortfolio==Interlinked[i,"PlatformPortfolio"]&
                InterlinkedNoSEC$MinofSignedDate<=Interlinked[i,"MinofSignedDate"]&
                (is.na(InterlinkedNoSEC$LastUltimateCompletionDate)|
                     InterlinkedNoSEC$LastUltimateCompletionDate>=Interlinked[i,"MinofSignedDate"]
                )
            ])
        
    }#Otherwise compare with everything, subtract one to cancel out self matches.
    else{
        Interlinked$InverselinkedCount[i]<-sum(Interlinked$InverseVariants[
            Interlinked$ContractingOfficeID==Interlinked[i,"ContractingOfficeID"]&
                Interlinked$PlatformPortfolio==Interlinked[i,"PlatformPortfolio"]&
                Interlinked$MinofSignedDate<=Interlinked[i,"MinofSignedDate"]&
                (is.na(Interlinked$LastUltimateCompletionDate)|
                     Interlinked$LastUltimateCompletionDate>=Interlinked[i,"MinofSignedDate"]
                )
            ])-1
    }
}

#SUm up the InverseLinkedCount for each by CSIScontractID
aggregate(Interlinked$InverselinkedCount,by=list(Interlinked$CSIScontractID))

#Create a new CSV
write.table(InterlinkedOutput
            ,file="Office_platform_interlinked_processed.csv"                  
            , sep=","
            , row.names=FALSE
            , append=FALSE
)

