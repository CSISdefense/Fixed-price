
Path<-"K:\\2007-01 PROFESSIONAL SERVICES\\R scripts and data\\"
source(paste(Path,"lookups.r",sep=""))
source(paste(Path,"helper.r",sep=""))


FormatContractModel<-function(dfContract){
    colnames(dfContract)[colnames(dfContract)=="SubCustomer.sum"]<-"Who"
    colnames(dfContract)[colnames(dfContract)=="UnmodifiedIsSomeCompetition"]<-"Comp"
    colnames(dfContract)[colnames(dfContract)=="PlatformPortfolio.sum"]<-"What"
    colnames(dfContract)[colnames(dfContract)=="IsIDV"]<-"IDV"
    colnames(dfContract)[colnames(dfContract)=="FixedOrCost"]<-"FxCb"
    colnames(dfContract)[colnames(dfContract)=="AnyInternational"]<-"Intl"
    colnames(dfContract)[colnames(dfContract)=="SimpleArea"]<-"PSR"
    colnames(dfContract)[colnames(dfContract)=="qLowCeiling"]<-"LowCeil"
    colnames(dfContract)[colnames(dfContract)=="qHighCeiling"]<-"Ceil"
    colnames(dfContract)[colnames(dfContract)=="qLinked"]<-"Link"
    colnames(dfContract)[colnames(dfContract)=="qDuration"]<-"Dur"
    # colnames(dfContract)[colnames(dfContract)=="SingleOffer"]<-"One"
    colnames(dfContract)[colnames(dfContract)=="qOffers"]<-"Offr"
    colnames(dfContract)[colnames(dfContract)=="IsTerminated"]<-"Term"
    colnames(dfContract)[colnames(dfContract)=="SoftwareEng"]<-"Soft"
    colnames(dfContract)[colnames(dfContract)=="SimpleVehicle"]<-"Veh"
    colnames(dfContract)[colnames(dfContract)=="LabeledMDAP"]<-"MDAP"
    colnames(dfContract)[colnames(dfContract)=="qNChg"]<-"NChg"
    colnames(dfContract)[colnames(dfContract)=="qCRais"]<-"CRai"
    
    
    if(all(levels(dfContract$Ceil) %in% c("[75m+]",
                                  "[10m,75m)",
                                  "[1m,10m)", 
                                  "[100k,1m)",
                                  "[15k,100k)",
                                  "[0,15k)"
    ))){
        dfContract$Ceil<-factor(dfContract$Ceil,
                                levels=c("[75m+]",
                                         "[10m,75m)",
                                         "[1m,10m)", 
                                         "[100k,1m)",
                                         "[15k,100k)",
                                         "[0,15k)"
                                ),
                                labels=c("75m+",
                                         "10m - <75m",
                                         "1m - <10m", 
                                         "100k - <1m",
                                         "15k - <100k",
                                         "0 - <15k"
                                ),
                                ordered=TRUE
                                
        )
    }
    if(all(levels(dfContract$Dur) %in% c("[0 months,~2 months)",
                                  "[~2 months,~7 months)",
                                  "[~7 months-~1 year]",
                                  "(~1 year,~2 years]",
                                  "(~2 years+]"
    ))){
        dfContract$Dur<-factor(dfContract$Dur,
                                levels=c("[0 months,~2 months)",
                                         "[~2 months,~7 months)",
                                         "[~7 months-~1 year]",
                                         "(~1 year,~2 years]",
                                         "(~2 years+]"
                                ),
                                labels=c("[0 months,~2 months)",
                                         "[~2 months,~7 months)",
                                         "[~7 months-~1 year]",
                                         "(~1 year,~2 years]",
                                         "(~2 years+]"
                                ),
                                ordered=TRUE
                                
        )
    }
    
    dfContract$ContractCount<-1
    
    if("MinOfEffectiveDate" %in% colnames(dfContract) & 
       !"StartFiscalYear" %in% colnames(dfContract))
        dfContract$MinOfEffectiveDate<-as.Date(as.character(dfContract$MinOfEffectiveDate))
        dfContract$StartFiscalYear<-DateToFiscalYear(dfContract$MinOfEffectiveDate)
    
    if("MinOfEffectiveDate" %in% colnames(dfContract) &
       "UnmodifiedDays" %in% colnames(dfContract)){
        dfContract$UnmodifiedCompletionDate<-dfContract$MinOfEffectiveDate+dfContract$UnmodifiedDays
    }

    if("MinOfEffectiveDate" %in% colnames(dfContract) &
       "UnmodifiedCompletionDate" %in% colnames(dfContract)){
        dfContract$EndAfterPeriod<-dfContract$UnmodifiedCompletionDate>max(dfContract$MinOfEffectiveDate,na.rm=TRUE)
    }
    dfContract
}




## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
    library(plyr)
    
    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }
    
    # This does the summary. For each group's data frame, return a vector with
    # N, mean, and sd
    datac <- ddply(data, groupvars, .drop=.drop,
                   .fun = function(xx, col) {
                       c(N    = length2(xx[[col]], na.rm=na.rm),
                         mean = mean   (xx[[col]], na.rm=na.rm),
                         sd   = sd     (xx[[col]], na.rm=na.rm)
                       )
                   },
                   measurevar
    )
    
    # Rename the "mean" column    
    datac <- rename(datac, c("mean" = measurevar))
    
    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
    
    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult
    
    return(datac)
}