# DoD Fixed-Price Study: Contract Size
Greg Sanders  
Tuesday, January 13, 2015  


```
## Loading required package: ggplot2
## Loading required package: stringr
## Loading required package: plyr
```

Contracts are classified using a mix of numerical and categorical variables. While the changes in numerical variables are easy to grasp and summarize, a contract may have one line item that is competed and another that is not. As is detailed in the [exploration on R&D](RnD_1to5_exploration.md), we are only considering information available prior to contract start. The percentage of contract obligations that were competed is a valuable benchmark, but is highly influenced by factors that occurred after contract start.

**Contract Size**
For the purpose of this report, a contract refers to either an award with a unique procurement identifier or an IDV with a unique pairing of a delivery order procurement identifier and a referenced IDV procurement identifier.  Contracts were classified on the basis of total expenditures for the fiscal year in question.  Groupings are in nominal dollars because many regulatory thresholds are not adjusted for inflation; as a result, smaller contracts will be slightly overrepresented in recent years.  Unlike some prior reports, de-obligations are excluded rather than being grouped with contracts under $250,000.

**Methodological notes:**
Initial contract size is not calculated in the same methods as Size of Contracts for other CSIS reports.  Instead, size of contract is determined using the base and all options value amount of the original unmodified transaction for the contract.




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

* ObligatedAmount is a classification for the entirity of the contract  (0.00% missing data).
* UnmodifiedContractBaseAndAllOptionsValue is a classification for the entirity of the contract  (45.59% missing data).
* UnmodifiedContractBaseAndExercisedOptionsValue is a classification for the entirity of the contract  (45.83% missing data).
* ContractBaseAndAllOptionsValue is a classification for the entirity of the contract  (44.51% missing data).
* ContractBaseAndExercisedOptionsValue is a classification for the entirity of the contract  (44.61% missing data).


```r
summary(subset(contract.sample,select=c(ObligatedAmount,
                                UnmodifiedContractObligatedAmount,
                                UnmodifiedContractBaseAndAllOptionsValue,
                                UnmodifiedContractBaseAndExercisedOptionsValue,
                                ContractBaseAndAllOptionsValue,
                                ContractBaseAndExercisedOptionsValue                        
                                ))
        )
```

```
##  ObligatedAmount      UnmodifiedContractObligatedAmount
##  Min.   :-1.924e+08   Min.   : -20050105               
##  1st Qu.: 1.049e+06   1st Qu.:    341632               
##  Median : 6.994e+06   Median :   2350480               
##  Mean   : 7.333e+07   Mean   :  21253161               
##  3rd Qu.: 3.613e+07   3rd Qu.:  11800000               
##  Max.   : 2.341e+10   Max.   :1895207544               
##  UnmodifiedContractBaseAndAllOptionsValue
##  Min.   :0.000e+00                       
##  1st Qu.:0.000e+00                       
##  Median :8.760e+04                       
##  Mean   :3.903e+07                       
##  3rd Qu.:6.899e+06                       
##  Max.   :3.673e+10                       
##  UnmodifiedContractBaseAndExercisedOptionsValue
##  Min.   :0.000e+00                             
##  1st Qu.:0.000e+00                             
##  Median :7.938e+04                             
##  Mean   :3.026e+07                             
##  3rd Qu.:5.235e+06                             
##  Max.   :3.673e+10                             
##  ContractBaseAndAllOptionsValue ContractBaseAndExercisedOptionsValue
##  Min.   :-3.055e+07             Min.   :-3.055e+07                  
##  1st Qu.: 0.000e+00             1st Qu.: 0.000e+00                  
##  Median : 1.367e+05             Median : 1.283e+05                  
##  Mean   : 7.246e+07             Mean   : 6.453e+07                  
##  3rd Qu.: 9.998e+06             3rd Qu.: 9.484e+06                  
##  Max.   : 1.418e+11             Max.   : 1.418e+11
```

```r
names(contract.sample)
```

```
##   [1] "systemequipmentcode"                                
##   [2] "MajorCommandID"                                     
##   [3] "SubCustomer"                                        
##   [4] "Customer"                                           
##   [5] "Contracting.Agency.ID"                              
##   [6] "CSIScontractID"                                     
##   [7] "Unmodifiedmultipleorsingleawardidc"                 
##   [8] "LastSignedLastDateToOrder"                          
##   [9] "LastUltimateCompletionDate"                         
##  [10] "LastCurrentCompletionDate"                          
##  [11] "SumOfnumberOfActions"                               
##  [12] "Action.Obligation"                                  
##  [13] "ContractBaseAndExercisedOptionsValue"               
##  [14] "ContractBaseAndAllOptionsValue"                     
##  [15] "StartFiscal_Year"                                   
##  [16] "maxoffiscal_year"                                   
##  [17] "IsIDV"                                              
##  [18] "idvpiid"                                            
##  [19] "piid"                                               
##  [20] "ContractLabelID"                                    
##  [21] "hyphenatedIDVpiid"                                  
##  [22] "CSISidvpiidID"                                      
##  [23] "IsPerformanceBasedLogistics"                        
##  [24] "ContractingOfficeID"                                
##  [25] "FundingAgencyID"                                    
##  [26] "FundingOfficeID"                                    
##  [27] "Pricing.Mechanism.Code"                             
##  [28] "statutoryexceptiontofairopportunity"                
##  [29] "extentcompeted"                                     
##  [30] "IsCostBased"                                        
##  [31] "IsFixedPrice"                                       
##  [32] "IsIncentive"                                        
##  [33] "contractactiontype"                                 
##  [34] "ObligatedAmount"                                    
##  [35] "MinOfEffectiveDate"                                 
##  [36] "MaxOfEffectiveDate"                                 
##  [37] "ChangeOrderObligatedAmount"                         
##  [38] "ChangeOrderBaseAndExercisedOptionsValue"            
##  [39] "ChangeOrderBaseAndAllOptionsValue"                  
##  [40] "NewWorkObligatedAmount"                             
##  [41] "NewWorkBaseAndExercisedOptionsValue"                
##  [42] "NewWorkBaseAndAllOptionsValue"                      
##  [43] "UnmodifiedContractObligatedAmount"                  
##  [44] "UnmodifiedContractBaseAndExercisedOptionsValue"     
##  [45] "UnmodifiedContractBaseAndAllOptionsValue"           
##  [46] "IsClosed"                                           
##  [47] "IsModified"                                         
##  [48] "IsTerminated"                                       
##  [49] "SumOfisChangeOrder"                                 
##  [50] "MaxOfisChangeOrder"                                 
##  [51] "SumOfisNewWork"                                     
##  [52] "MaxOfisNewWork"                                     
##  [53] "UnmodifiedCurrentCompletionDate"                    
##  [54] "UnmodifiedUltimateCompletionDate"                   
##  [55] "UnmodifiedLastDateToOrder"                          
##  [56] "SimpleArea"                                         
##  [57] "isAnyRnD1to5"                                       
##  [58] "obligatedAmountRnD1to5"                             
##  [59] "firstSignedDateRnD1to5"                             
##  [60] "UnmodifiedRnD1to5"                                  
##  [61] "Indefinite.Delivery.Contract"                       
##  [62] "multipleorsingleawardidc"                           
##  [63] "addmultipleorsingawardidc"                          
##  [64] "AwardOrIDVcontractactiontype"                       
##  [65] "UnmodifiedNumberOfOffersReceived"                   
##  [66] "UnmodifiedIsFullAndOpen"                            
##  [67] "UnmodifiedIsSomeCompetition"                        
##  [68] "UnmodifiedIsOnlyOneSource"                          
##  [69] "UnmodifiedIsFollowonToCompetedAction"               
##  [70] "Unmodifiedaddmultipleorsingawardidc"                
##  [71] "UnmodifiedAwardOrIDVcontractactiontype"             
##  [72] "NumberOfOffersReceived"                             
##  [73] "IsFullAndOpen"                                      
##  [74] "IsSomeCompetition"                                  
##  [75] "ObligatedAmountIsSomeCompetition"                   
##  [76] "IsOnlyOneSource"                                    
##  [77] "IsFollowonToCompetedAction"                         
##  [78] "MultipleOrSingleAwardIDC"                           
##  [79] "AddMultipleOrSingleAwardIDC"                        
##  [80] "AwardOrIDVcontractActionType"                       
##  [81] "Contracting.Department.ID"                          
##  [82] "Contracting.Agency.Name"                            
##  [83] "CSIS.Name"                                          
##  [84] "Summarized.Agency"                                  
##  [85] "SubCustomer.sum"                                    
##  [86] "SubCustomer.detail"                                 
##  [87] "SubCustomer.component"                              
##  [88] "Subcustomer.Obsolete"                               
##  [89] "MajorCommandCode"                                   
##  [90] "ContractingOfficeCode"                              
##  [91] "AgencyID"                                           
##  [92] "MajorCommandName"                                   
##  [93] "MCC_StartFiscal_Year"                               
##  [94] "MCC_EndFiscal_Year"                                 
##  [95] "ContractingOfficeName"                              
##  [96] "DepartmentID"                                       
##  [97] "Unseperated"                                        
##  [98] "systemequipmentcodeText"                            
##  [99] "systemequipmentshorttext"                           
## [100] "SystemEquipmentIn2000Sample"                        
## [101] "SystemEquipmentIn2007Sample"                        
## [102] "UnmodifiedNumberOfOffersSummary"                    
## [103] "LogOfAction.Obligation"                             
## [104] "pNewWorkVsContractObligatedAmount"                  
## [105] "pChangeOrderVsContractObligatedAmount"              
## [106] "LogOfContractBaseAndAllOptionsValue"                
## [107] "pNewWorkVsContractBaseAndAllOptionsValue"           
## [108] "pChangeOrderVsContractBaseAndAllOptionsValue"       
## [109] "LogOfContractBaseAndExercisedOptionsValue"          
## [110] "pNewWorkVsContractBaseAndExercised"                 
## [111] "pChangeOrderVsContractBaseAndExercised"             
## [112] "LogOfUnmodifiedContractObligatedAmount"             
## [113] "pUnmodifiedContractObligated"                       
## [114] "pNewWorkVsUnmodifiedObligatedAmount"                
## [115] "pChangeOrderVsUnmodifiedObligatedAmount"            
## [116] "LogOfUnmodifiedContractBaseAndAllOptionsValue"      
## [117] "SizeOfUnmodifiedContractBaseAndAll"                 
## [118] "pUnmodifiedContractBaseAndAll"                      
## [119] "pNewWorkVsUnmodifiedBaseAndAll"                     
## [120] "pChangeOrderVsUnmodifiedBaseAndAll"                 
## [121] "LogOfUnmodifiedContractBaseAndExercisedOptionsValue"
## [122] "pUnmodifiedContractBaseAndExercised"                
## [123] "pNewWorkVsUnmodifiedBaseAndExercised"               
## [124] "pChangeOrderVsUnmodifiedBaseAndExercised"           
## [125] "Graph"                                              
## [126] "CurrentMonths"                                      
## [127] "CategoryOfCurrentMonths"                            
## [128] "UnmodifiedMonths"                                   
## [129] "CategoryOfUnmodifiedMonths"                         
## [130] "UnmodifiedCompetition"                              
## [131] "UnmodifiedVehicle"
```

```r
#Reading in 2007 and later sample.
list.files(path="data")
```

```
##  [1] "defense_contract_CSIScontractID_sample_15000_SumofObligatedAmount.csv"                    
##  [2] "defense_contract_CSIScontractID_sample_15000_SumofObligatedAmount_gte_2007.csv"           
##  [3] "defense_contract_CSIScontractID_sample_15000_SumofObligatedAmount_gte_2007_isCompeted.csv"
##  [4] "defense_contract_CSIScontractID_sample_15000_SumofObligatedAmount_IsCompeted.csv"         
##  [5] "defense_contract_CSIScontractID_sample_systemEquipmentCode.csv"                           
##  [6] "defense_contract_CSIScontractID_sample_systemEquipmentCode_gte_2007.csv"                  
##  [7] "defense_contract_CSIScontractID_sample_systemEquipmentCode_IsCompeted.csv"                
##  [8] "defense_contract_CSIScontractID_sample15000_sample.Sumofbaseandalloptionsvalue.csv"       
##  [9] "defense_contract_CSIScontractID_sample15000_SumOfbaseandexercisedoptionsvalue.csv"        
## [10] "defense_contract_CSIScontractID_sample15000_SumofObligatedAmount.csv"                     
## [11] "defense_contract_CSIScontractID_systemEquipmentCode.csv"                                  
## [12] "defense_office_MajorCommandID_sample_15000_SumofObligatedAmount.csv"                      
## [13] "defense_office_MajorCommandID_sample_15000_SumofObligatedAmount_gte_2007.csv"
```

```r
contract.sample  <- read.csv(
    "data\\defense_contract_CSIScontractID_sample_15000_SumofObligatedAmount_gte_2007.csv",
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


* ObligatedAmount is a classification for the entirity of the contract  (0.00% missing data).
* UnmodifiedContractBaseAndAllOptionsValue is a classification for the entirity of the contract  (0.24% missing data).
* UnmodifiedContractBaseAndExercisedOptionsValue is a classification for the entirity of the contract  (0.26% missing data).
* ContractBaseAndAllOptionsValue is a classification for the entirity of the contract  (0.00% missing data).
* ContractBaseAndExercisedOptionsValue is a classification for the entirity of the contract  (0.00% missing data).

