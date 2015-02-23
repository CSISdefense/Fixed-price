# DoD Fixed-Price Study: Contract Vehicle Classification
Greg Sanders  
Tuesday, January 13, 2015  


```
## Loading required package: ggplot2
## Loading required package: stringr
## Loading required package: plyr
```

Contracts are classified using a mix of numerical and categorical variables. While the changes in numerical variables are easy to grasp and summarize, a contract may have one line item that is competed and another that is not. As is detailed in the [exploration on R&D](RnD_1to5_exploration.md), we are only considering information available prior to contract start. The percentage of contract obligations that were competed is a valuable benchmark, but is highly influenced by factors that occured after contract start..

##Studying contract vehicle within the sample.
Describe contract vehicle here.


```r
contract.sample  <- read.csv(
    paste("data\\defense_contract_CSIScontractID_sample_15000_SumofObligatedAmount.csv", sep = ""),
    header = TRUE, sep = ",", dec = ".", strip.white = TRUE, 
    na.strings = c("NULL","NA",""),
    stringsAsFactors = TRUE
    )

#These will probably be moved into apply_lookups at some point
contract.sample<-apply_lookups(Path,contract.sample)
```

```
## Joining by: Contracting.Agency.ID
## Joining by: SubCustomer, Customer
## Joining by: MajorCommandID
## Joining by: systemequipmentcode
```

```
## Warning in apply_lookups(Path, contract.sample): NaNs produced
```

```
## Warning in apply_lookups(Path, contract.sample): NaNs produced
```

```
## Warning in apply_lookups(Path, contract.sample): NaNs produced
```

```
## Warning in apply_lookups(Path, contract.sample): NaNs produced
```
Describe source variables in FPDS here.

###Limiting the sample.

Because this model analyzes the number of offers on competed contracts, the first step is eliminating contracts that were not competed. This is done using the UnmodifiedIsSomeCompetetion field (see [competition exploration](contract_competition_exploration.md) for variable details). This variable has an unlabeled rate of 33.49%. As is done throughout the model, if all labeled values for the contract have are consistent, then that value is used to fill in for the blanks.


```r
#Impute missing values when labeled entries have a consistent value.
NAisSomeCompetition<-contract.sample$UnmodifiedIsSomeCompetition=="Unlabeled"&contract.sample$IsSomeCompetition!="Mixed or \nUnlabeled"
contract.sample$UnmodifiedIsSomeCompetition[NAisSomeCompetition]<-contract.sample$IsSomeCompetition[NAisSomeCompetition]
rm(NAisSomeCompetition)
```

After imputed, UnmodifiedIsSomeCompetition has a 3.64% missing data rate. This variable can now be used to narrow the sample.


```r
contract.sample<-contract.sample[contract.sample$UnmodifiedIsSomeCompetition=="Comp.",]
```


###Evidence variables
Note that these missing data rates are only for competed entries, so they will typically not match the overall unlabeled rates.

 * IsIDV is a classification for the entirity of the contract  (0.00% missing data). See [vehicle exploration](contract_vehicle_exploration.md) for more. Since this variable is consistently labeled, it isn't necessary to impute data or seperate out unmodified entries.
 * UnmodifiedIsFullAndOpen is the classification given by the first record for the contract (27.82% missing data). See [exploration on competition](contract_competition_exploration.md) for more.
 * UnmodifiedNumberOfOffersReceived reports the Number of Offers received according to the first reported transaction under a contract (31.43% missing data, far too high, there must be a SQL mistake). See [exploration on competition](contract_competition_exploration.md) for more.




```r
summary(subset(contract.sample,select=c(IsSomeCompetition,
                                IsIDV
                                ))
        )
```

```
##             IsSomeCompetition     IsIDV       
##  Comp.               :9888    Min.   :0.0000  
##  No Comp.            :   0    1st Qu.:0.0000  
##  Mixed or \nUnlabeled:   9    Median :1.0000  
##                               Mean   :0.7277  
##                               3rd Qu.:1.0000  
##                               Max.   :1.0000
```

```r
#Impute missing values
NAisFullAndOpen<-is.na(contract.sample$UnmodifiedIsFullAndOpen)
contract.sample$UnmodifiedIsFullAndOpen[NAisFullAndOpen]<-contract.sample$IsFullAndOpen[NAisFullAndOpen]
rm(NAisFullAndOpen)

NAnumberOfOffers<-is.na(contract.sample$UnmodifiedNumberOfOffersReceived)&!is.na(contract.sample$NumberOfOffersReceived)
contract.sample$UnmodifiedNumberOfOffersReceived[NAnumberOfOffers]<-contract.sample$NumberOfOffersReceived[NAnumberOfOffers]
rm(NAnumberOfOffers)
```

After imputing using consistent labeled entries where available.
 * UnmodifiedIsFullAndOpen has (0.00% missing data).
 * UnmodifiedNumberOfOffersReceived has (5.20% missing data.

