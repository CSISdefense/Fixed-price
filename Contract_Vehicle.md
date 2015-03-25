# DoD Fixed-Price Study: Procedural Competition classification
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

###IsIDV
Is IDV is a binary variable, true if the contract is an indefinite delivery vehicle (IDV), false if it a definitive contract or a purchase order. Unlabeled cases are classified as NAs.

* IsIDV is a classification for the entirity of the contract  (0.00% missing data).
  

```r
summary(subset(contract.sample,select=c(contractactiontype,
                                multipleorsingleawardidc,
                                IsIDV,
                                addmultipleorsingawardidc,
                                AwardOrIDVcontractactiontype
                                ))
        )
```

```
##  contractactiontype   multipleorsingleawardidc     IsIDV       
##  Mode:logical       MULTIPLE AWARD:3163        Min.   :0.0000  
##  NA's:15000         SINGLE AWARD  :6864        1st Qu.:0.0000  
##                     NA's          :4973        Median :1.0000  
##                                                Mean   :0.6985  
##                                                3rd Qu.:1.0000  
##                                                Max.   :1.0000  
##                                                                
##  addmultipleorsingawardidc
##  Min.   :0.0000           
##  1st Qu.:0.0000           
##  Median :1.0000           
##  Mean   :0.6284           
##  3rd Qu.:1.0000           
##  Max.   :1.0000           
##  NA's   :418              
##                        AwardOrIDVcontractactiontype
##  Indefinite Delivery Contract        :9163         
##  Definitive Contract                 :3824         
##  Federal Supply Schedule             : 751         
##  Purchase Order                      : 679         
##  Government Wide Acquisition Contract:  46         
##  (Other)                             : 114         
##  NA's                                : 423
```
