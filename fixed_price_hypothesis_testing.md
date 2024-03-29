

```
## Loading required package: ggplot2
## Loading required package: stringr
## Loading required package: graph
## Loading required package: plyr
## 
## Attaching package: 'plyr'
## 
## The following object is masked from 'package:graph':
## 
##     join
## 
## Loading required package: Hmisc
## Loading required package: grid
## Loading required package: lattice
## Loading required package: survival
## Loading required package: Formula
## 
## Attaching package: 'Hmisc'
## 
## The following objects are masked from 'package:plyr':
## 
##     is.discrete, summarize
## 
## The following objects are masked from 'package:base':
## 
##     format.pval, round.POSIXt, trunc.POSIXt, units
## 
## Loading required package: Matrix
## Loading required package: gRain
## Loading required package: gRbase
## Loading required package: bnlearn
## 
## Attaching package: 'bnlearn'
## 
## The following objects are masked from 'package:gRbase':
## 
##     children, parents
## 
## Loading required package: Rgraphviz
## Loading required package: reshape2
## Loading required package: scales
```

Contracts are classified using a mix of numerical and categorical variables. While the changes in numerical variables are easy to grasp and summarize, a contract may have one line item that is competed and another that is not. As is detailed in the [exploration on R&D](RnD_1to5_exploration.md), we are only considering information available prior to contract start. The percentage of contract obligations that were competed is a valuable benchmark, but is highly influenced by factors that occured after contract start..

##Studying contract vehicle within the sample.
Describe contract vehicle here.




```r
load("Output\\FixedPriceGin.Rdata",.GlobalEnv)
load("Output\\compGin.Rdata",.GlobalEnv)

# zipfile<-unz("Data\\defense_contract_CSIScontractID_model.zip",
#              "defense_contract_CSIScontractID_model.csv")

ContractModel  <- read.csv(
#     zipfile,
    "Data\\defense_contract_CSIScontractID_model.csv",
    header = TRUE, sep = ",", dec = ".", strip.white = TRUE, 
    na.strings = c("NULL","NA",""),
    stringsAsFactors = TRUE
    )

ContractModel$Count<-1
names(ContractModel)
```

```
##  [1] "IDV"     "FxCb"    "Comp"    "Link"    "MDAP"    "Who"     "What"   
##  [8] "Intl"    "PSR"     "LowCeil" "Ceil"    "Dur"     "Offr"    "Veh"    
## [15] "Term"    "Soft"    "UCA"     "CRai"    "NChg"    "Count"
```

```r
dropped<-subset(ContractModel,!complete.cases(ContractModel)|
                    Intl=="Unlabeled"|
                    PSR =="Mixed or Unlabeled" |
                    Who=="Uncategorized" |
                    What =="Unlabeled")

ContractModel<-subset(ContractModel,complete.cases(ContractModel)&
                          Intl!="Unlabeled"&
                          PSR !="Mixed or Unlabeled"&
                          Who!="Uncategorized" & 
                          What!="Unlabeled")

ContractModel$Intl<-droplevels(ContractModel$Intl)
ContractModel$PSR<-droplevels(ContractModel$PSR)
ContractModel$Who<-droplevels(ContractModel$Who)
ContractModel$What<-droplevels(ContractModel$What)
ContractModel$Comp<-droplevels(ContractModel$Comp)


ContractModel$Ceil<-revalue(ContractModel$Ceil, c("[15k,100k)"="[0,100k)","[0,15k)"="[0,100k)"))
    
    
    
    #Order the ceilings
    ContractModel$Ceil<-factor(ContractModel$Ceil,
                                levels=c("[75m+]",
                                         "[10m,75m)",
                                         "[1m,10m)",
                                         "[100k,1m)",
                                         "[0,100k)"
                                         
                                         
                                ),
                                labels=c("[75m+]",
                                         "[10m,75m)",
                                         "[1m,10m)",
                                         "[100k,1m)",
                                         "[0,100k)"
                                ),
                                ordered=TRUE
    )
    

ModelSummary<-ddply(ContractModel,
                    .(FxCb,Comp,Link,MDAP,Who,What,Intl,PSR,Ceil,Dur,Offr,Veh,Term,Soft,UCA,CRai,NChg),
                    summarise,
                    Count=sum(Count)
                    )

ModelSummary$Freq<-ModelSummary$Count/sum(ModelSummary$Count)




summary(FixedPriceGin)
```

```
## Independence network: Compiled: FALSE Propagated: FALSE 
##  Nodes : Named chr [1:17] "FxCb" "Comp" "Link" "MDAP" "Who" "What" ...
##  - attr(*, "names")= chr [1:17] "FxCb" "Comp" "Link" "MDAP" ...
```

```r
# str(FixedPriceGin$universe$levels["FxCb"])
# cost.list <- list(nodes = c("FxCb"),
#                         states = c("Cost-Based"))
# cost.find <- setEvidence(FixedPriceGin, 
#                         nodes=cost.list$nodes,
#                         states=cost.list$states)
# fixed.list<-list(nodes = c("FxCb"),
#                         states = c("Fixed-Price"))
# fixed.find <- setEvidence(FixedPriceGin, 
#                         nodes=fixed.list$nodes,
#                         states=fixed.list$states)
# 


# read in the data and convert to a data frame (the csv file being read does not have the header row)

# select subsets of rows fixing a pair of variables 
# and count occurrences of Offr bins
# 
# short_cb = table(subset(ContractModel, Dur == "(~2 years+]" & FxCb == "Cost-Based", select = Offr))
# short_fx =  
#     
#     Compare sort_cb/sum(short_cb), for example, to the output of the Bayes net when the Dur and FxCb evidence are set appropriately
# 
# The way to check that they are identical is to use the vector or Offr probabilities p_short_cb (of size 4) produced by the Bayes net
# 
# chisq.test(short_cb, p = p_short_cb)
# 
# To compare the effect of the pricing structure on the number of offers one could compute
# 
# chisq.test(short_cb, p = short_fx/sum(short_fx))
# 
# In my experience, the subsets are so large that most p-values will be vanishingly small so all differences are significant.
# 
# # compute the approximate average number of offers
# mean_short_cb = (short_cb[1] + 2*short_cb[2] + 4*short_cb[3] + 5*short_cb[4])/sum(short_cb)
# mean_short_fx = (short_fx[1] + 2*short_fx[2] + 4*short_fx[3] + 5*short_fx[4])/sum(short_fx)
# 
# You could then say, for example, that switching from the Cost-Based to Fixed-Priced model will get you on average
# 
# 100*(mean_short_fx - mean_short_cb)/mean_short_cb % 
# 
# more offers.
# 
# You could slice the dice the dataset by fixing more variables.  In each case it would be good to compare the Bayes net output to pure counts.
# 
# 

# StandardizeTableChiSquared(ContractModel,
#                            FixedPriceGin,
#                            "Term"
#                            )
# 
# StandardizeTableChiSquared(ContractModel,
#                            FixedPriceGin,
#                            "Offr"
#                            )
# 
# StandardizeTableChiSquared(ContractModel,
#                            FixedPriceGin,
#                            "CRai"
#                            )
# 
# StandardizeTableChiSquared(subset(ContractModel,PSR=="R&D"&FxCb=="Fixed-Price"),
#                            setEvidence(FixedPriceGin, 
#                            nodes=c("PSR","FxCb"),
#                            states=c("R&D","Cost-Based")
#                            ),
#                            "Term"
#                            )
# debug(StandardizeTableChiSquared)
# StandardizeTableChiSquared(subset(ContractModel,PSR=="Products"&FxCb=="Fixed-Price"),
#                            setEvidence(FixedPriceGin, 
#                            nodes=c("PSR","FxCb"),
#                            states=c("R&D","Cost-Based")
#                            ),
#                            "Term"
#                            )
# 
# 
# StandardizeTableChiSquared(subset(ContractModel,PSR=="R&D"&FxCb=="Cost-Based"),
#                            setEvidence(FixedPriceGin, 
#                            nodes=c("PSR","FxCb"),
#                            states=c("R&D","Cost-Based")
#                            ),
#                            "Term"
#                            )
# 
# StandardizeTableChiSquared(subset(ContractModel,PSR=="R&D"&FxCb=="Fixed-Price"),
#                            setEvidence(FixedPriceGin, 
#                            nodes=c("PSR","FxCb"),
#                            states=c("R&D","Fixed-Price")
#                            ),
#                            "Term"
#                            )
# 
```





```r
# debug(FixedPriceComparisonTable)

PopulationLongDF<-FixedPriceComparisonTable(ModelSummary,"Population")




# debug(PointRowWrapper)
#Single Offer Competition
PointRowWrapper(NULL,#VAR.main.label,
                "All Contracts\n(Overall and Controls)",          #VAR.row.label,
                "% of Contracts Receiving Single Offer Competition",          #VAR.data.label,
                "Contract Pricing and Significance of Fixed-Price vs. Cost-Based Difference", #VAR.legend.label
                
                Coloration,#VAR.Coloration
    subset(PopulationLongDF,!is.na(Significance)&dVariable=="% Single Offer Competition" & Control!="Comp." & FxCb!="Combination or Other"),
                          "Control",
                          "p",
                          "FxCb",
                          "Significance",
                          "iVariable")
```

```
## Joining by: variable
```

![](fixed_price_hypothesis_testing_files/figure-html/PopulationAbsolute-1.png) 

```r
#Average Number of Offers
PointRowWrapper(NULL,#VAR.main.label,
                "All Contracts\n(Overall and Controls)",          #VAR.row.label,
                "Approximate Average Number of Offers",          #VAR.data.label,
                "Contract Pricing and Significance of Fixed-Price vs. Cost-Based Difference", #VAR.legend.label
                
                Coloration,#VAR.Coloration
    subset(PopulationLongDF,!is.na(Significance)&dVariable=="Average Number of Offers for Competed Contracts" & Control!="Comp." & FxCb!="Combination or Other"),
                          "Control",
                          "Average",
                          "FxCb",
                          "Significance",
                          "iVariable")
```

```
## Joining by: variable
```

![](fixed_price_hypothesis_testing_files/figure-html/PopulationAbsolute-2.png) 

```r
#Average Number of Changes
PointRowWrapper(NULL,#VAR.main.label,
                "All Contracts\n(Overall and Controls)",          #VAR.row.label,
                "Approximate Average Number of Change Orders",          #VAR.data.label,
                "Contract Pricing and Significance of Fixed-Price vs. Cost-Based Difference", #VAR.legend.label
                
                Coloration,#VAR.Coloration
    subset(PopulationLongDF,!is.na(Significance)&dVariable=="Average Number of Change Orders"  & FxCb!="Combination or Other"),
                          "Control",
                          "Average",
                          "FxCb",
                          "Significance",
                          "iVariable")
```

```
## Joining by: variable
```

![](fixed_price_hypothesis_testing_files/figure-html/PopulationAbsolute-3.png) 

```r
#Ceiling Raising Change Orders %
PointRowWrapper(NULL,#VAR.main.label,
                "All Contracts\n(Overall and Controls)",          #VAR.row.label,
                "Approximate Average Extent of Ceiling Breaches",          #VAR.data.label,
                "Contract Pricing and Significance of Fixed-Price vs. Cost-Based Difference", #VAR.legend.label
                
                Coloration,#VAR.Coloration
    subset(PopulationLongDF,!is.na(Significance)&dVariable=="Ceiling Raising Change Orders %" & FxCb!="Combination or Other"),
                          "Control",
                          "Average",
                          "FxCb",
                          "Significance",
                          "iVariable",
    Percentage=TRUE)
```

```
## Joining by: variable
```

![](fixed_price_hypothesis_testing_files/figure-html/PopulationAbsolute-4.png) 

```r
#Terminations
PointRowWrapper(NULL,#VAR.main.label,
                "All Contracts\n(Overall and Controls)",          #VAR.row.label,
                "Contract Termination Rate",          #VAR.data.label,
                "Contract Pricing and Significance of Fixed-Price vs. Cost-Based Difference", #VAR.legend.label
                
                Coloration,#VAR.Coloration
    subset(PopulationLongDF,!is.na(Significance)&dVariable=="Terminated"  & FxCb!="Combination or Other"),
                          "Control",
                          "p",
                          "FxCb",
                          "Significance",
                          "iVariable")
```

```
## Joining by: variable
```

![](fixed_price_hypothesis_testing_files/figure-html/PopulationAbsolute-5.png) 



```r
PopulationWideDF<-FixedPriceCast(PopulationLongDF)
```

```
## Warning: attributes are not identical across measure variables; they will
## be dropped
```

```r
#Remove redundant tests
PopulationWideDF<-subset(PopulationWideDF,
                  !Control %in% unique(PopulationWideDF$Hypothesis))
# PopulationWideDF$Hypothesis<-factor(PopulationWideDF$Hypothesis,levels=c("R&D","Population"),ordered=TRUE)

#Single Offer Competition
PointRowWrapper(NULL,#VAR.main.label,
                "All Contracts\n(Overall and Controls)",          #VAR.row.label,
                "Fixed-to-Cost Ratio for Single Offer Competition Rate",          #VAR.data.label,
                "Contract Sample and Significance of Fixed-Price vs. Cost-Based Difference",
                
                Coloration,#VAR.Coloration
    subset(PopulationWideDF,!is.na(Cost.Based_Significance)&dVariable=="% Single Offer Competition"),
                          "Control",
                          "pFixedCostMargin",
                          "Hypothesis",
                          "Cost.Based_Significance",
                          "iVariable")+geom_hline(aes(yintercept=0))  
```

```
## Joining by: variable
```

![](fixed_price_hypothesis_testing_files/figure-html/PopulationDivision-1.png) 

```r
#Average Number of Offers
PointRowWrapper(NULL,#VAR.main.label,
                "All Contracts\n(Overall and Controls)",          #VAR.row.label,
                "Fixed-to-Cost Ratio for Approximate Average Number of Offers",   #VAR.data.label,
                "Contract Sample and Significance of Fixed-Price vs. Cost-Based Difference",
                
                Coloration,#VAR.Coloration
    subset(PopulationWideDF,!is.na(Cost.Based_Significance)&dVariable=="Average Number of Offers for Competed Contracts"),
                          "Control",
                          "avgFixedCostMargin",
                          "Hypothesis",
                          "Cost.Based_Significance",
                          "iVariable",
    Percentage=TRUE)+geom_hline(aes(yintercept=0))
```

```
## Joining by: variable
```

![](fixed_price_hypothesis_testing_files/figure-html/PopulationDivision-2.png) 

```r
#Average Number of Changes
PointRowWrapper(NULL,#VAR.main.label,
                "All Contracts\n(Overall and Controls)",          #VAR.row.label,
                "Fixed-to-Cost Ratio for Approximate Average Number of Change Orders",          #VAR.data.label,
                "Contract Sample and Significance of Fixed-Price vs. Cost-Based Difference",
                
                Coloration,#VAR.Coloration
    subset(PopulationWideDF,!is.na(Cost.Based_Significance)&dVariable=="Average Number of Change Orders"),
                          "Control",
                          "avgFixedCostMargin",
                          "Hypothesis",
                          "Cost.Based_Significance",
                          "iVariable",
    Percentage=TRUE)+geom_hline(aes(yintercept=0))
```

```
## Joining by: variable
```

![](fixed_price_hypothesis_testing_files/figure-html/PopulationDivision-3.png) 

```r
#Ceiling Raising Change Orders %
PointRowWrapper(NULL,#VAR.main.label,
                "All Contracts\n(Overall and Controls)",          #VAR.row.label,
                "Fixed-to-Cost Ratio for Approximate Average Extent of Ceiling Breaches",          #VAR.data.label,
                "Contract Sample and Significance of Fixed-Price vs. Cost-Based Difference",
                
                Coloration,#VAR.Coloration
    subset(PopulationWideDF,!is.na(Cost.Based_Significance)&dVariable=="Ceiling Raising Change Orders %"),
                          "Control",
                          "avgFixedCostMargin",
                          "Hypothesis",
                          "Cost.Based_Significance",
                          "iVariable",
    high=10,
    Percentage=TRUE)+geom_hline(aes(yintercept=0))
```

```
## Joining by: variable
```

![](fixed_price_hypothesis_testing_files/figure-html/PopulationDivision-4.png) 

```r
PopulationOutliersDF<-ListOutliers(PopulationWideDF,"avgFixedCostMargin",NA,10)

#Terminations
PointRowWrapper(NULL,#VAR.main.label,
                "All Contracts\n(Overall and Controls)",          #VAR.row.label,
                "Fixed-to-Cost Ratio for Termination Rate",          #VAR.data.label,
                "Contract Sample and Significance of Fixed-Price vs. Cost-Based Difference",
                
                Coloration,#VAR.Coloration
    subset(PopulationWideDF,!is.na(Cost.Based_Significance)&dVariable=="Terminated"),
                          "Control",
                          "pFixedCostMargin",
                          "Hypothesis",
                          "Cost.Based_Significance",
                          "iVariable",
    high=10
    )+geom_hline(aes(yintercept=0))
```

```
## Joining by: variable
```

![](fixed_price_hypothesis_testing_files/figure-html/PopulationDivision-5.png) 

```r
PopulationOutliersDF<-rbind(PopulationOutliersDF,ListOutliers(PopulationWideDF,"pFixedCostMargin",NA,10))

write.csv(PopulationWideDF,paste("Output\\",
                          paste("Population"
                                ,"Fixed_Price"
                                ,"Hypothesis_Testing"
                                ,"2007-2013"
                                ,sep="_"
                                )
                          ,".csv",
                          sep=""))
```


##C1A:  Contracts that are initially expected to be very large contracts will encounter more problems.
*    For overall DoD contracting in 2013, contracts with an annual value of $500 or more saw the lowest rate of competition with three or more offers- 20 percent. The next lowest rate for contracts between $100 million and $500 million (26 percent). (CSIS Analysis of FPDS Data).
*	Dependent Variables:
*	Single offer competition, contract termination, number of change orders, and cost of change orders.
*	Initial Taxonomy:
*	The sample will include all contracts with an initial annualized base and all options amount of at least $500 million, or a initial base and exercised options amount of at least $500 million.

###Placeholder and next steps.
$500M contracts is not a level of granularity yet available in the Ceil variable, so the lower $30M threshold will be used for the initial analysis. In addition, the criteria we are using is base and all values alone, but that should always match or exceed the base and exercised value. 


###Initial results.
At the $30M ceil level the results were ambigious. For all contracts, single offer is slightly more common among large contracts but competition with 3-4 and 5+. For contracts that were competed, large contracts actually received more offers.

##C1B Contracts initially expected to have smaller values will receive more offers.
*    For overall DoD contracting in 2013, contracts with an annual value of less than $250,000 saw the highest rate of competition with two or more offers (62 percent), and that rate generally declines as size of contract increases. (CSIS Analysis of FPDS Data). This phenomenon is reversed with services contracts which have less competition when the size is smaller.
*	Dependent Variables:
*	Single offer competition.
*	Initial Taxonomy:
*	The independent variable will be the base and all exercised options amount for the contract. This value will not be annualized, as the steady rate of exercising options means that the initial value has severe limitations when predicting total contract value.

For small contracts, the pattern 

###Initial results
The hypothesis does hold weakly for contracts under $15k, although larger contracts have more 5+ offer. The hypothesis does not hold for contracts under $100k and again 5+ offer contracts are remain more common among larger contracts.  If the dividing line is instead $1 million, then the analysis does hold, with contracts of $1m and lower having more competition at every level of analysis.



```r
# 
# FixedPriceGin[[1]]$levels$Ceil
# 
# # debug(OffersHypothesisTester)
# SizeDF<-FixedPriceHypothesisTester(FixedPriceGin)
# #No redundant tests to remove
# inuous(labels = percent_format())
# 
# 
# #Single Offer Competition
# ggplot(subset(SizeDF,dVariable=="% Single Offer Competition"),
#        aes(x=Control,y=avgFixedCostMargin)
#        )+
#     geom_point()+
#     facet_grid(dVariable~iVariable)+ coord_flip()+theme(legend.position="bottom",axis.text.x = element_text(angle = 90, hjust = 1))+ scale_y_continuous(labels = percent_format())+geom_hline(aes(yintercept=0))  
# 
# 
# #Average Number of Offers
# ggplot(subset(SizeDF,dVariable=="Average Number of Offers for Competed Contracts"),
#        aes(x=Control,y=avgFixedCostMargin)
#        )+
#     geom_point()+
#     facet_grid(dVariable~iVariable)+ coord_flip()+theme(legend.position="bottom",axis.text.x = element_text(angle = 90, hjust = 1))+ scale_y_continuous(labels = percent_format())+geom_hline(aes(yintercept=0))  
# 
# #Average Number of Changes
# ggplot(subset(SizeDF,dVariable=="Average Number of Change Orders"),
#        aes(x=Control,y=avgFixedCostMargin)
#        )+
#     geom_point()+
#     facet_grid(dVariable~iVariable)+ coord_flip()+theme(legend.position="bottom",axis.text.x = element_text(angle = 90, hjust = 1))+ scale_y_continuous(labels = percent_format())+geom_hline(aes(yintercept=0))  
# 
# 
# #Ceiling Raising Change Orders %
# ggplot(subset(SizeDF,dVariable=="Ceiling Raising Change Orders %"),
#        aes(x=Control,y=avgFixedCostMargin)
#        )+
#     geom_point()+
#     facet_grid(dVariable~iVariable)+ coord_flip()+theme(legend.position="bottom",axis.text.x = element_text(angle = 90, hjust = 1))+ scale_y_continuous(labels = percent_format())+geom_hline(aes(yintercept=0))  
# 
# 
# 
# #Terminations
# ggplot(subset(SizeDF,dVariable=="Terminated"),
#        aes(x=Control,y=pFixedCostMargin)
#        )+
#     geom_point()+
#     facet_grid(dVariable~iVariable)+ coord_flip()+theme(legend.position="bottom",axis.text.x = element_text(angle = 90, hjust = 1))+ scale_y_continuous(labels = percent_format())+geom_hline(aes(yintercept=0))  
# 
```


##C2 Aircraft encounter

*    Aircraft have a 22 point corresponding effect on total contract growth that is explained by schedule and cost overruns. The report looked at time, schedule, and differences between major commands, but not contract pricing mechanism. (Performance of the Defense Acquisition System 2013)
*	"During the Second World War leading belligerents such as Britain, the United States, and Germany, favored the use of fixed-price contracts for purchasing military aircraft. These contracts were believed to encourage economical and efficient production. Yet only Britain succeeded in developing contractual procedures based primarily upon the fixed-price. This was primarily driven by the degree of organization by the weapons companies." (The Price of Air Power: Technological Change, Industrial Policy, and Military Aircraft Contracts in the Era of British Rearmament, 1935-39)
*	Dependent Variables:
*	Single offer competition, contract termination, number of change orders, and cost of change orders.
*	Initial Taxonomy:
*	The sample will include all contracts with a 50 percent or greater share of obligations in the Aircraft and Drone platform portfolio.

#Changes
The model incorporated a 75% threshold for classification as a given Platform under What. This excludes some contracts, but captured the vast majority of contracts with a mix of platforms.

#Results
As expected, numbers of offers are far higher with non-aircraft, for both competed and for all contracts.


```r
# 
# FixedPriceGin[[1]]$levels$What
# 
# AircraftFind<- subset(FixedPriceGin, What=="Aircraft and Drones")
#                            
# 
# getEvidence(AircraftFind)
# pEvidence(AircraftFind)
# 
# 
# NotAircraftFind<- setEvidence(FixedPriceGin, 
#                               nodes=c("What"),
#                               states=list(FixedPriceGin[[1]]$levels$What[
#                                   FixedPriceGin[[1]]$levels$What!="Aircraft and Drones"]))
# 
# 
# getEvidence(NotAircraftFind)
# 
# 
# AircraftDF<-FixedPriceHypothesisTester(AircraftFind,"Aircraft")
# AircraftDF<-rbind(AircraftDF,
#                   FixedPriceHypothesisTester(NotAircraftFind,"Not Aircraft")
#                   )
# 
# 
# #Remove redundant tests
# AircraftDF<-subset(AircraftDF,
#                    !Control %in% unique(AircraftDF$Hypothesis))
# 
# 
# #Single Offer Competition
# ggplot(subset(AircraftDF,dVariable=="% Single Offer Competition"),
#        aes(x=Control,color=Hypothesis,shape=Hypothesis,y=pFixedCostMargin)
#        )+
#     geom_point()+
#     facet_grid(dVariable~iVariable)+ coord_flip()+theme(legend.position="bottom",axis.text.x = element_text(angle = 90, hjust = 1))+ scale_y_continuous(labels = percent_format())+geom_hline(aes(yintercept=0))  
# 
# 
# #Average Number of Offers
# ggplot(subset(AircraftDF,dVariable=="Average Number of Offers for Competed Contracts"),
#        aes(x=Control,color=Hypothesis,shape=Hypothesis,y=avgFixedCostMargin)
#        )+
#     geom_point()+
#     facet_grid(dVariable~iVariable)+ coord_flip()+theme(legend.position="bottom",axis.text.x = element_text(angle = 90, hjust = 1))+ scale_y_continuous(labels = percent_format())+geom_hline(aes(yintercept=0))  
# 
# #Average Number of Changes
# ggplot(subset(AircraftDF,dVariable=="Average Number of Change Orders"),
#        aes(x=Control,color=Hypothesis,shape=Hypothesis,y=avgFixedCostMargin)
#        )+
#     geom_point()+
#     facet_grid(dVariable~iVariable)+ coord_flip()+theme(legend.position="bottom",axis.text.x = element_text(angle = 90, hjust = 1))+ scale_y_continuous(labels = percent_format())+geom_hline(aes(yintercept=0))  
# 
# 
# #Ceiling Raising Change Orders %
# ggplot(subset(AircraftDF,dVariable=="Ceiling Raising Change Orders %"),
#        aes(x=Control,color=Hypothesis,shape=Hypothesis,y=avgFixedCostMargin)
#        )+
#     geom_point()+
#     facet_grid(dVariable~iVariable)+ coord_flip()+theme(legend.position="bottom",axis.text.x = element_text(angle = 90, hjust = 1))+ scale_y_continuous(labels = percent_format())+geom_hline(aes(yintercept=0))  
# 
# 
# 
# #Terminations
# ggplot(subset(AircraftDF,dVariable=="Terminated"),
#        aes(x=Control,color=Hypothesis,shape=Hypothesis,y=pFixedCostMargin)
#        )+
#     geom_point()+
#     facet_grid(dVariable~iVariable)+ coord_flip()+theme(legend.position="bottom",axis.text.x = element_text(angle = 90, hjust = 1))+ scale_y_continuous(labels = percent_format())+geom_hline(aes(yintercept=0))  
# 
# 
# 
# rm(AircraftFind,NotAircraftFind)
# 
```


##C3: Undefinitized contract actions (UCAs) in development will encounter more problems
*	"For example, analysis is revealing that UCAs can be successfully employed in early production but are concerns for developmental work" (Performance of the Defense Acquisition System 2013)
*	"UCAs increase development cycle time by increasing schedule growth." GAO Study
*	"The contractor has little incentive to control costs during this period.". (GAO study?)
*	DOD has had difficulty overseeing the use of UCAs on at least three levels: (1) how and when to use UCAs, (2) definitizing UCAs in a timely fashion and 3) documenting the basis for negotiated profit or fees (GAO study?)
*	Dependent Variables:
*	Single offer competition, contract termination, number of change orders, and cost of change orders.
*	Initial Taxonomy:
*	UCA status will be defined as those contracts labeled as "Letter Contracts."
*	The first five phases of R&D as defined by product or service code will be included. This excluding R&D management and support and operational system development. 
*	Contracts will be included in the sample if more than 50 percent of obligations in any given year meet the above criteria.

###Next steps: 
UCA status will be added to IDV. Since the contract type will not be known at the offset, the information lost will primarily be information not available to the contracting officer at contract start. However, theres no available proxy until that information is added.



##Contracts using purchase orders and "Other Indefinite Delivery Contracts (IDCs)" are more likely to receive a single offer.
*	Past CSIS research has found that contracts using a purchase orders and "Other IDCs" (Other IDCs includes Federal Supply Schedule, Basic Ordering Agreements, Blanket Purchasing Agreements, Government-Wide Acquisition Contracts) have notably higher  rates of single offer competition than other vehicles. (CSIS, Federal Services Contracting and the Supporting Industrial Base, 2000-2012)
*	Dependent Variables:
*	Single offer competition.
*	Initial Taxonomy:
*	The sample will include contracts with a 50 percent or greater share of obligations reporting purchase orders or "Other IDCs".

###Placeholders and next steps
At present, the vehicle variable only has granularity to look at at IDVs versus definitive and purchase orders. Multi-award IDVs have the highest number of offers, so they will be used as the place holder until greater granularity is available.

The analysis indicates validates this past finding, IDVs are marketedly more likely to be competed and to receive more offers when competed.


```r
# 
# FixedPriceGin[[1]]$levels$UCA
# 
# 
# 
# UCAFind<- setEvidence(FixedPriceGin, 
#                       nodes=c("UCA"),
#                       states=c("UCA" )
#                       )
# 
# getEvidence(UCAFind)
# 
# NotUCAFind<- setEvidence(FixedPriceGin, 
#                          nodes=c("UCA"),
#                          states= list(c( "Not UCA" ))
#                          )
# 
# getEvidence(NotUCAFind)
# 
# 
# 
# 
# # undebug(FixedPriceHypothesisTester)
# UCAdf<-FixedPriceHypothesisTester(UCAFind,"UCA")
# UCAdf<-rbind(UCAdf,
#              FixedPriceHypothesisTester(NotUCAFind,"Not UCA")
#              )
# 
# 
# #Remove redundant tests
# UCAdf<-subset(UCAdf,
#               !Control %in% unique(UCAdf$Hypothesis))
# 
# 
# #Single Offer Competition
# ggplot(subset(UCAdf,dVariable=="% Single Offer Competition"),
#        aes(x=Control,color=Hypothesis,shape=Hypothesis,y=pFixedCostMargin)
#        )+
#     geom_point()+
#     facet_grid(dVariable~iVariable)+ coord_flip()+theme(legend.position="bottom",axis.text.x = element_text(angle = 90, hjust = 1))+ scale_y_continuous(labels = percent_format())+geom_hline(aes(yintercept=0))  
# 
# 
# #Average Number of Offers
# ggplot(subset(UCAdf,dVariable=="Average Number of Offers for Competed Contracts"),
#        aes(x=Control,color=Hypothesis,shape=Hypothesis,y=avgFixedCostMargin)
#        )+
#     geom_point()+
#     facet_grid(dVariable~iVariable)+ coord_flip()+theme(legend.position="bottom",axis.text.x = element_text(angle = 90, hjust = 1))+ scale_y_continuous(labels = percent_format())+geom_hline(aes(yintercept=0))  
# 
# #Average Number of Changes
# ggplot(subset(UCAdf,dVariable=="Average Number of Change Orders"),
#        aes(x=Control,color=Hypothesis,shape=Hypothesis,y=avgFixedCostMargin)
#        )+
#     geom_point()+
#     facet_grid(dVariable~iVariable)+ coord_flip()+theme(legend.position="bottom",axis.text.x = element_text(angle = 90, hjust = 1))+ scale_y_continuous(labels = percent_format())+geom_hline(aes(yintercept=0))  
# 
# 
# #Ceiling Raising Change Orders %
# ggplot(subset(UCAdf,dVariable=="Ceiling Raising Change Orders %"),
#        aes(x=Control,color=Hypothesis,shape=Hypothesis,y=avgFixedCostMargin)
#        )+
#     geom_point()+
#     facet_grid(dVariable~iVariable)+ coord_flip()+theme(legend.position="bottom",axis.text.x = element_text(angle = 90, hjust = 1))+ scale_y_continuous(labels = percent_format())+geom_hline(aes(yintercept=0))  
# 
# 
# 
# #Terminations
# ggplot(subset(UCAdf,dVariable=="Terminated"),
#        aes(x=Control,color=Hypothesis,shape=Hypothesis,y=pFixedCostMargin)
#        )+
#     geom_point()+
#     facet_grid(dVariable~iVariable)+ coord_flip()+theme(legend.position="bottom",axis.text.x = element_text(angle = 90, hjust = 1))+ scale_y_continuous(labels = percent_format())+geom_hline(aes(yintercept=0))  
# 
# 
```



```r
# 
# FixedPriceGin[[1]]$levels$Veh
# 
# 
# 
# SACFind<- setEvidence(FixedPriceGin, 
#                       nodes=c("Veh"),
#                       states=c("SINGLE AWARD" )
#                       )
# 
# getEvidence(SACFind)
# 
# OtherIDVFind<- setEvidence(FixedPriceGin, 
#                            nodes=c("Veh"),
#                            states= list(c( "MULTIPLE AWARD", "Other IDV" ))
#                            )
# 
# getEvidence(OtherIDVFind)
# 
# 
# 
# AwardFind<- setEvidence(FixedPriceGin, 
#                         nodes=c("Veh"),
#                         states=c( "Def/Pur" )
#                         )
# 
# getEvidence(AwardFind)
# 
# 
# 
# 
# 
# VehicleDF<-FixedPriceHypothesisTester(SACFind,"Single-Award IDV")
# VehicleDF<-rbind(VehicleDF,
#                  FixedPriceHypothesisTester(OtherIDVFind,"Other IDV"),
#                  FixedPriceHypothesisTester(AwardFind,"Award")
#                  )
# 
# 
# #Remove redundant tests
# VehicleDF<-subset(VehicleDF,
#                   !Control %in% unique(VehicleDF$Hypothesis))
# 
# 
# #Single Offer Competition
# ggplot(subset(VehicleDF,dVariable=="% Single Offer Competition"),
#        aes(x=Control,color=Hypothesis,shape=Hypothesis,y=pFixedCostMargin)
#        )+
#     geom_point()+
#     facet_grid(dVariable~iVariable)+ coord_flip()+theme(legend.position="bottom",axis.text.x = element_text(angle = 90, hjust = 1))+ scale_y_continuous(labels = percent_format())+geom_hline(aes(yintercept=0))  
# 
# 
# #Average Number of Offers
# ggplot(subset(VehicleDF,dVariable=="Average Number of Offers for Competed Contracts"),
#        aes(x=Control,color=Hypothesis,shape=Hypothesis,y=avgFixedCostMargin)
#        )+
#     geom_point()+
#     facet_grid(dVariable~iVariable)+ coord_flip()+theme(legend.position="bottom",axis.text.x = element_text(angle = 90, hjust = 1))+ scale_y_continuous(labels = percent_format())+geom_hline(aes(yintercept=0))  
# 
# #Average Number of Changes
# ggplot(subset(VehicleDF,dVariable=="Average Number of Change Orders"),
#        aes(x=Control,color=Hypothesis,shape=Hypothesis,y=avgFixedCostMargin)
#        )+
#     geom_point()+
#     facet_grid(dVariable~iVariable)+ coord_flip()+theme(legend.position="bottom",axis.text.x = element_text(angle = 90, hjust = 1))+ scale_y_continuous(labels = percent_format())+geom_hline(aes(yintercept=0))  
# 
# 
# #Ceiling Raising Change Orders %
# ggplot(subset(VehicleDF,dVariable=="Ceiling Raising Change Orders %"),
#        aes(x=Control,color=Hypothesis,shape=Hypothesis,y=avgFixedCostMargin)
#        )+
#     geom_point()+
#     facet_grid(dVariable~iVariable)+ coord_flip()+theme(legend.position="bottom",axis.text.x = element_text(angle = 90, hjust = 1))+ scale_y_continuous(labels = percent_format())+geom_hline(aes(yintercept=0))  
# 
# 
# 
# #Terminations
# ggplot(subset(VehicleDF,dVariable=="Terminated"),
#        aes(x=Control,color=Hypothesis,shape=Hypothesis,y=pFixedCostMargin)
#        )+
#     geom_point()+
#     facet_grid(dVariable~iVariable)+ coord_flip()+theme(legend.position="bottom",axis.text.x = element_text(angle = 90, hjust = 1))+ scale_y_continuous(labels = percent_format())+geom_hline(aes(yintercept=0))  
# 
# 
# 
# 
# 
# 
# 
# rm(IDVFind,AwardFind)
```

##Contracts with longer initially expected periods of performance will encounter more problems.
*	Past CSIS research on cost overruns in major defense acquisition projects have found that cost growth is correlated with contract duration. "If cost increases accrue over time, then programs with an older baseline estimate would tend to accumulate relatively higher cost increases. The data for the analyzed programs show that older programs indeed experience larger overruns... this growth correlation not only provides further evidence for the assertion that cost growth occurs steadily throughout the program lifespan, but it also suggests that younger programs are not performing better than older programs." (David Berteau et al.: Cost and Time Overruns for Major Defense Acquisition Programs (2011))
*	Dependent Variables:
*	Contract termination, number of change orders, and cost of change orders.
*	Initial Taxonomy:
*	Period of performance will be measured as expected contract duration in 4-week months, rounding up.
*	Note: CSIS is including a separate hypothesis (Slide 9) to test whether fixed-price contracts encounter an even greater number of problems under contracts with longer expected periods of performance 



```r
# 
# FixedPriceGin[[1]]$levels$Dur
# 
# LongDurFind<- setEvidence(FixedPriceGin, 
#                           nodes=c("Dur"),
#                           states=c("(~2 years+]")
#                           )
# 
# getEvidence(LongDurFind)
# 
# 
# 
# 
# 
# LongDurWideDF<-FixedPriceHypothesisTester(LongDurFind,"Long Dur.")
# LongDurWideDF<-rbind(LongDurWideDF,
#                  FixedPriceHypothesisTester(FixedPriceGin,"Population")
#                  )
# 
# 
# #Remove redundant tests
# LongDurWideDF<-subset(LongDurWideDF,
#                   !Control %in% unique(LongDurWideDF$Hypothesis))
# 
# 
# #Single Offer Competition
# ggplot(subset(LongDurWideDF,!is.na(Cost.Based_Significance)&dVariable=="% Single Offer Competition"),
#        aes(x=Control,color=Hypothesis,shape=Hypothesis,y=pFixedCostMargin)
#        )+
#     geom_point()+
#     facet_grid(dVariable~iVariable)+ coord_flip()+theme(legend.position="bottom",axis.text.x = element_text(angle = 90, hjust = 1))+ scale_y_continuous(labels = percent_format())+geom_hline(aes(yintercept=0))  
# 
# 
# #Average Number of Offers
# ggplot(subset(LongDurWideDF,!is.na(Cost.Based_Significance)&dVariable=="Average Number of Offers for Competed Contracts"),
#        aes(x=Control,color=Hypothesis,shape=Hypothesis,y=avgFixedCostMargin)
#        )+
#     geom_point()+
#     facet_grid(dVariable~iVariable)+ coord_flip()+theme(legend.position="bottom",axis.text.x = element_text(angle = 90, hjust = 1))+ scale_y_continuous(labels = percent_format())+geom_hline(aes(yintercept=0))  
# 
# #Average Number of Changes
# ggplot(subset(LongDurWideDF,!is.na(Cost.Based_Significance)&dVariable=="Average Number of Change Orders"),
#        aes(x=Control,color=Hypothesis,shape=Hypothesis,y=avgFixedCostMargin)
#        )+
#     geom_point()+
#     facet_grid(dVariable~iVariable)+ coord_flip()+theme(legend.position="bottom",axis.text.x = element_text(angle = 90, hjust = 1))+ scale_y_continuous(labels = percent_format())+geom_hline(aes(yintercept=0))  
# 
# 
# #Ceiling Raising Change Orders %
# ggplot(subset(LongDurWideDF,!is.na(Cost.Based_Significance)&dVariable=="Ceiling Raising Change Orders %"),
#        aes(x=Control,color=Hypothesis,shape=Hypothesis,y=avgFixedCostMargin)
#        )+
#     geom_point()+
#     facet_grid(dVariable~iVariable)+ coord_flip()+theme(legend.position="bottom",axis.text.x = element_text(angle = 90, hjust = 1))+ scale_y_continuous(labels = percent_format())+geom_hline(aes(yintercept=0))  
# 
# 
# 
# #Terminations
# ggplot(subset(LongDurWideDF,!is.na(Cost.Based_Significance)&dVariable=="Terminated"),
#        aes(x=Control,color=Hypothesis,shape=Hypothesis,y=pFixedCostMargin)
#        )+
#     geom_point()+
#     facet_grid(dVariable~iVariable)+ coord_flip()+theme(legend.position="bottom",axis.text.x = element_text(angle = 90, hjust = 1))+ scale_y_continuous(labels = percent_format())+geom_hline(aes(yintercept=0))  
# 
# 
# 
# rm(LongDurFind)
```


##Incentive Fee contracts experience fewer problems than other forms of contracts.
*	"Incentive formula-type contracts had lower cost growth at the median (i.e., its central tendency), shared savings with the government, and earned slightly lower margins than CPAF/CPFF/CS/other (O) contracts. In other words, to achieve about the same margin on incentive formula-type contracts, contractors had to control cost growth better-and they did so." (Performance of the Defense Acquisition System, 2014) This finding is in line with multiple earlier policy statements from Undersecretary Kendall emphasizing fixed-price incentive fee contracts and vendor motivation. 
*	One study uses the F-35 as a case study to examine when to utilize different types of contract incentive structures. The model presented in this study could provide insight into the use of different types of fixed-price contracts. (The Effect of Processes and Incentives on Acquisition Cost Growth)
*	Dependent Variables:
*	Contract termination, number of change orders, and cost of change orders.
*	Initial Taxonomy:
*	The sample will include contracts with a 50 percent or greater share of obligations reporting cost-plus incentive fee or fixed-price incentive fee as the pricing mechanism.

##Next steps
Incentive was not included in the initial model because it made up a small proportion of all contracts. Fee structure should probably be included as a seperate node entirely. Initial analysis at the aggregate dollar levels for the competition study found did not support the hypothesis.




```r
# 
# FixedPriceGin[[1]]$levels$Comp
# 
# 
# CompFind<-setEvidence(FixedPriceGin, 
#                       nodes=c("Comp"),
#                       states=c("Comp.")
#                       )
# 
# getEvidence(CompFind)
# 
# 
# 
# 
# CompDF<-FixedPriceHypothesisTester(CompFind,"Comp.")
# CompDF<-rbind(CompDF,
#               FixedPriceHypothesisTester("Comp.","Population")
#               )
# 
# 
# #Remove redundant tests
# CompDF<-subset(CompDF,
#                !Control %in% unique(CompDF$Hypothesis))
# #Remove the No Comp for the offer related iVariables
# CompDF<-subset(CompDF,
#                !(dVariable %in% c("1","Average Number of Offers")&
#                      Hypothesis=="Not Comp."))
# 
# 
# #Single Offer Competition
# ggplot(subset(CompDF,dVariable=="% Single Offer Competition"),
#        aes(x=Control,y=pFixedCostMargin)
#        )+
#     geom_point()+
#     facet_grid(dVariable~iVariable)+ coord_flip()+theme(legend.position="bottom",axis.text.x = element_text(angle = 90, hjust = 1))+ scale_y_continuous(labels = percent_format())+geom_hline(aes(yintercept=0))  
# 
# 
# #Average Number of Offers
# ggplot(subset(CompDF,dVariable=="Average Number of Offers for Competed Contracts"),
#        aes(x=Control,y=avgFixedCostMargin)
#        )+
#     geom_point()+
#     facet_grid(dVariable~iVariable)+ coord_flip()+theme(legend.position="bottom",axis.text.x = element_text(angle = 90, hjust = 1))+ scale_y_continuous(labels = percent_format())+geom_hline(aes(yintercept=0))  
# 
# #Average Number of Changes
# ggplot(subset(CompDF,dVariable=="Average Number of Change Orders"),
#        aes(x=Control,color=Hypothesis,shape=Hypothesis,y=avgFixedCostMargin)
#        )+
#     geom_point()+
#     facet_grid(dVariable~iVariable)+ coord_flip()+theme(legend.position="bottom",axis.text.x = element_text(angle = 90, hjust = 1))+ scale_y_continuous(labels = percent_format())+geom_hline(aes(yintercept=0))  
# 
# 
# #Ceiling Raising Change Orders %
# ggplot(subset(CompDF,dVariable=="Ceiling Raising Change Orders %"),
#        aes(x=Control,color=Hypothesis,shape=Hypothesis,y=avgFixedCostMargin)
#        )+
#     geom_point()+
#     facet_grid(dVariable~iVariable)+ coord_flip()+theme(legend.position="bottom",axis.text.x = element_text(angle = 90, hjust = 1))+ scale_y_continuous(labels = percent_format())+geom_hline(aes(yintercept=0))  
# 
# 
# 
# #Terminations
# ggplot(subset(CompDF,dVariable=="Terminated"),
#        aes(x=Control,color=Hypothesis,shape=Hypothesis,y=pFixedCostMargin)
#        )+
#     geom_point()+
#     facet_grid(dVariable~iVariable)+ coord_flip()+theme(legend.position="bottom",axis.text.x = element_text(angle = 90, hjust = 1))+ scale_y_continuous(labels = percent_format())+geom_hline(aes(yintercept=0))  
# 
# 
# 
# rm(CompFind,NotCompFind)
# 
```






##H1: Large Research and Development (R&D) contracts will encounter challenges under fixed-price contracts. (On Contracting for Uncertain R&D) 

*    Dependent Variables:
*	Single offer competition, contract termination, number of change orders, and cost of change orders.
*	Initial Taxonomy:
*	The first five phases of R&D as defined by product or service code will be included. This step excludes R&D management, support and operational system development. 
*	The sample will include all contracts with a value of at least $25 million in either initial annualized base and all options amount of initial base and exercised options amount.

##Changes: 
The study team chose to use different contract size breakdowns than those used in past CSIS work (Contract_Ceiling). As a result, the closest available dividing point in the model is $30M. In this case, the $25M has no firmer theoretical basis and the $30M dividing line has the advantage of representing the weighted average contract ceiling. Thus the study team is updating the hypothesis to use the $30M standard instead.

For this example, we're still using usual R&D rather than excluding R&d7

##Results
The hypothesis is supported with regard to number of offers. Larger fixed-price contracts are more likely to experience single offer competition and are less likely to experience competition with three or more offers. Combination tracks more closely with fixed-price than cost-plus.



```r
FixedPriceGin[[1]]$levels$Ceil
```

```
## [1] "[0,15k)"    "[100k,1m)"  "[10m,75m)"  "[15k,100k)" "[1m,10m)"  
## [6] "[75m+]"
```

```r
RnDfind<- setEvidence(FixedPriceGin, 
                      nodes=c("PSR"),
                      states=c("R&D")
                      )



getEvidence(RnDfind)
```

```
## Finding: 
## PSR: R&D
## Pr(Finding)= 0.009186456
```

```r
# debug(ChiSquaredFixedPriceCostBased)
# debug(FixedPriceComparisonTable)
RnDlongDF<-FixedPriceComparisonTable(subset(ModelSummary,PSR=="R&D"),
                                "R&D")




# debug(PointRowWrapper)
#Single Offer Competition
PointRowWrapper(NULL,#VAR.main.label,
                "R&D Contracts\n(Overall and Controls)",          #VAR.row.label,
                "% of Contracts Receiving Single Offer Competition",          #VAR.data.label,
                "Contract Pricing and Significance of Fixed-Price vs. Cost-Based Difference", #VAR.legend.label
                
                Coloration,#VAR.Coloration
    subset(RnDlongDF,!is.na(Significance)&dVariable=="% Single Offer Competition" & Control!="Comp." & FxCb!="Combination or Other"),
                          "Control",
                          "p",
                          "FxCb",
                          "Significance",
                          "iVariable")
```

```
## Joining by: variable
```

![](fixed_price_hypothesis_testing_files/figure-html/H1LargeR&Dabsolute-1.png) 

```r
#Average Number of Offers
PointRowWrapper(NULL,#VAR.main.label,
                "R&D Contracts\n(Overall and Controls)",          #VAR.row.label,
                "Approximate Average Number of Offers",          #VAR.data.label,
                "Contract Pricing and Significance of Fixed-Price vs. Cost-Based Difference", #VAR.legend.label
                
                Coloration,#VAR.Coloration
    subset(RnDlongDF,!is.na(Significance)&dVariable=="Average Number of Offers for Competed Contracts" & Control!="Comp." & FxCb!="Combination or Other"),
                          "Control",
                          "Average",
                          "FxCb",
                          "Significance",
                          "iVariable")
```

```
## Joining by: variable
```

![](fixed_price_hypothesis_testing_files/figure-html/H1LargeR&Dabsolute-2.png) 

```r
#Average Number of Changes
PointRowWrapper(NULL,#VAR.main.label,
                "R&D Contracts\n(Overall and Controls)",          #VAR.row.label,
                "Approximate Average Number of Change Orders",          #VAR.data.label,
                "Contract Pricing and Significance of Fixed-Price vs. Cost-Based Difference", #VAR.legend.label
                
                Coloration,#VAR.Coloration
    subset(RnDlongDF,!is.na(Significance)&dVariable=="Average Number of Change Orders"  & FxCb!="Combination or Other"),
                          "Control",
                          "Average",
                          "FxCb",
                          "Significance",
                          "iVariable")
```

```
## Joining by: variable
```

![](fixed_price_hypothesis_testing_files/figure-html/H1LargeR&Dabsolute-3.png) 

```r
#Ceiling Raising Change Orders %
PointRowWrapper(NULL,#VAR.main.label,
                "R&D Contracts\n(Overall and Controls)",          #VAR.row.label,
                "Approximate Average Extent of Ceiling Breaches",          #VAR.data.label,
                "Contract Pricing and Significance of Fixed-Price vs. Cost-Based Difference", #VAR.legend.label
                
                Coloration,#VAR.Coloration
    subset(RnDlongDF,!is.na(Significance)&dVariable=="Ceiling Raising Change Orders %"  & FxCb!="Combination or Other"),
                          "Control",
                          "Average",
                          "FxCb",
                          "Significance",
                          "iVariable",
    Percentage=TRUE)
```

```
## Joining by: variable
```

![](fixed_price_hypothesis_testing_files/figure-html/H1LargeR&Dabsolute-4.png) 

```r
#Terminations
PointRowWrapper(NULL,#VAR.main.label,
                "R&D Contracts\n(Overall and Controls)",          #VAR.row.label,
                "Contract Termination Rate",          #VAR.data.label,
                "Contract Pricing and Significance of Fixed-Price vs. Cost-Based Difference", #VAR.legend.label
                
                Coloration,#VAR.Coloration
    subset(RnDlongDF,!is.na(Significance)&dVariable=="Terminated"  & FxCb!="Combination or Other"),
                          "Control",
                          "p",
                          "FxCb",
                          "Significance",
                          "iVariable")
```

```
## Joining by: variable
```

![](fixed_price_hypothesis_testing_files/figure-html/H1LargeR&Dabsolute-5.png) 

```r
rm(RnDfind)
```



```r
# debug(FixedPriceCast)
RnDwideDF<-FixedPriceCast(rbind(RnDlongDF,PopulationLongDF))
```

```
## Warning: attributes are not identical across measure variables; they will
## be dropped
```

```r
#Remove redundant tests
RnDwideDF<-subset(RnDwideDF,
                  !Control %in% unique(RnDwideDF$Hypothesis))
RnDwideDF$Hypothesis<-factor(RnDwideDF$Hypothesis,levels=c("R&D","Population"),ordered=TRUE)

#Single Offer Competition
PointRowWrapper(NULL,#VAR.main.label,
                "R&D Contracts\n(Overall and Controls)",          #VAR.row.label,
                "Fixed-to-Cost Ratio for Single Offer Competition Rate",          #VAR.data.label,
                "Contract Sample and Significance of Fixed-Price vs. Cost-Based Difference",
                
                Coloration,#VAR.Coloration
    subset(RnDwideDF,!is.na(Cost.Based_Significance)&dVariable=="% Single Offer Competition"),
                          "Control",
                          "pFixedCostMargin",
                          "Hypothesis",
                          "Cost.Based_Significance",
                          "iVariable")+geom_hline(aes(yintercept=0))+geom_hline(aes(yintercept=0))  
```

```
## Joining by: variable
```

```
## Warning in loop_apply(n, do.ply): Removed 2 rows containing missing values
## (geom_point).
```

![](fixed_price_hypothesis_testing_files/figure-html/H1LargeR&D-1.png) 

```r
#Average Number of Offers
PointRowWrapper(NULL,#VAR.main.label,
                "R&D Contracts\n(Overall and Controls)",          #VAR.row.label,
                "Fixed-to-Cost Ratio for Approximate Average Number of Offers",   #VAR.data.label,
                "Contract Sample and Significance of Fixed-Price vs. Cost-Based Difference",
                
                Coloration,#VAR.Coloration
    subset(RnDwideDF,!is.na(Cost.Based_Significance)&dVariable=="Average Number of Offers for Competed Contracts"),
                          "Control",
                          "avgFixedCostMargin",
                          "Hypothesis",
                          "Cost.Based_Significance",
                          "iVariable",
    Percentage=TRUE)+geom_hline(aes(yintercept=0))
```

```
## Joining by: variable
```

```
## Warning in loop_apply(n, do.ply): Removed 1 rows containing missing values
## (geom_point).
```

![](fixed_price_hypothesis_testing_files/figure-html/H1LargeR&D-2.png) 

```r
#Average Number of Changes
PointRowWrapper(NULL,#VAR.main.label,
                "R&D Contracts\n(Overall and Controls)",          #VAR.row.label,
                "Fixed-to-Cost Ratio for Approximate Average Number of Change Orders",          #VAR.data.label,
                "Contract Sample and Significance of Fixed-Price vs. Cost-Based Difference",
                
                Coloration,#VAR.Coloration
    subset(RnDwideDF,!is.na(Cost.Based_Significance)&dVariable=="Average Number of Change Orders"),
                          "Control",
                          "avgFixedCostMargin",
                          "Hypothesis",
                          "Cost.Based_Significance",
                          "iVariable",
    Percentage=TRUE)+geom_hline(aes(yintercept=0))
```

```
## Joining by: variable
```

```
## Warning in loop_apply(n, do.ply): Removed 2 rows containing missing values
## (geom_point).
```

![](fixed_price_hypothesis_testing_files/figure-html/H1LargeR&D-3.png) 

```r
#Ceiling Raising Change Orders %
PointRowWrapper(NULL,#VAR.main.label,
                "R&D Contracts\n(Overall and Controls)",          #VAR.row.label,
                "Fixed-to-Cost Ratio for Approximate Average Extent of Ceiling Breaches",          #VAR.data.label,
                "Contract Sample and Significance of Fixed-Price vs. Cost-Based Difference",
                
                Coloration,#VAR.Coloration
    subset(RnDwideDF,!is.na(Cost.Based_Significance)&dVariable=="Ceiling Raising Change Orders %"),
                          "Control",
                          "avgFixedCostMargin",
                          "Hypothesis",
                          "Cost.Based_Significance",
                          "iVariable",
    low=-10,
    high=10,
    Percentage=TRUE
    )+geom_hline(aes(yintercept=0))
```

```
## Joining by: variable
```

```
## Warning in loop_apply(n, do.ply): Removed 2 rows containing missing values
## (geom_point).
```

```
## Warning in loop_apply(n, do.ply): Removed 1 rows containing missing values
## (geom_point).
```

![](fixed_price_hypothesis_testing_files/figure-html/H1LargeR&D-4.png) 

```r
RnDoutliersDF<-ListOutliers(RnDwideDF,"avgFixedCostMargin",-10,10)


#Terminations
PointRowWrapper(NULL,#VAR.main.label,
                "R&D Contracts\n(Overall and Controls)",          #VAR.row.label,
                "Fixed-to-Cost Ratio for Termination Rate",          #VAR.data.label,
                "Contract Sample and Significance of Fixed-Price vs. Cost-Based Difference",
                
                Coloration,#VAR.Coloration
    subset(RnDwideDF,!is.na(Cost.Based_Significance)&dVariable=="Terminated"),
                          "Control",
                          "pFixedCostMargin",
                          "Hypothesis",
                          "Cost.Based_Significance",
                          "iVariable",
    high=10
    )+geom_hline(aes(yintercept=0))
```

```
## Joining by: variable
```

```
## Warning in loop_apply(n, do.ply): Removed 4 rows containing missing values
## (geom_point).
```

```
## Warning in loop_apply(n, do.ply): Removed 1 rows containing missing values
## (geom_point).
```

```
## Warning in loop_apply(n, do.ply): Removed 1 rows containing missing values
## (geom_point).
```

![](fixed_price_hypothesis_testing_files/figure-html/H1LargeR&D-5.png) 

```r
RnDoutliersDF<-rbind(RnDoutliersDF,ListOutliers(RnDwideDF,"pFixedCostMargin",NA,10))



write.csv(RnDwideDF,paste("Output\\",
                          paste("RnD"
                                ,"Fixed_Price"
                                ,"Hypothesis_Testing"
                                ,"2007-2013"
                                ,sep="_"
                                )
                          ,".csv",
                          sep=""))
```


##H2: Due to design uncertainty Pre-Milestone B Major Defense Acquisition Programs (MDAPs) are potentially more likely to encounter problems than other fixed-price contracts
*	Prerequisites for use of fixed-price: "Design content is established and the components are mature technologies. There are no signifi???cant unresolved design issues, no major integration risk, the external interfaces are well defined, and no serious risk exists of unknowns surfacing in developmental testing and causing major redesign." (Frank Kendall, Use of Fixed-Price Incentive Firm (FPIF) Contracts in Development and Production. Defense AT&L, (March-April 2013) pp 2-4.) {Emphasis added}
*	Dependent Variables:
*	Single offer competition, contract termination, number of change orders, and cost of change orders.
*	Initial Taxonomy:
*	Using past CSIS research on Major Defense Acquisition Programs, CSIS will include in the sample System Equipment Codes for MDAPs that also include Pre-Milestone B spending. 
*	Contracts will be included if greater than 50% of obligations are classified with a relevant System Equipment Code and/or if they are manually labeled as belonging to that MDAP.
*	Note: This criteria will capture a similar sample to the prior R&D hypothesis. The study team would like to also include Major Automated Information Systems, but cannot because they are not labeled in FPDS.

#Temporary changes and next steps
MDAP specific classification is not yet included because it represents such a small portion of total contracts. It is presently planned to be incorporated through the Interlinked Variable. As an intermediate step.

#Initial results
The hypothesis was not supported with regard to number of offers, but this should be treated as a soft negative finding until the more precise question is tested.




```r
MDAPFind<- setEvidence(FixedPriceGin, 
                       nodes=c("MDAP"),
                       states=c( "Labeled MDAP" ))

MDAPlongDF<-FixedPriceComparisonTable(subset(ModelSummary,MDAP=="Labeled MDAP"),
                                "MDAP")



#Remove redundant tests
MDAPlongDF<-subset(MDAPlongDF,
                   !Control %in% unique(MDAPlongDF$Hypothesis))
MDAPlongDF$Hypothesis<-factor(MDAPlongDF$Hypothesis,levels=c("MDAP","Population"),ordered=TRUE)

#Single Offer Competition
PointRowWrapper(NULL,#VAR.main.label,
                "MDAP Contracts\n(Overall and Controls)",          #VAR.row.label,
                "% of Contracts Receiving Single Offer Competition",          #VAR.data.label,
                "Contract Pricing and Significance of Fixed-Price vs. Cost-Based Difference", #VAR.legend.label
                
                Coloration,#VAR.Coloration
    subset(MDAPlongDF,!is.na(Significance)&dVariable=="% Single Offer Competition" & Control!="Comp." & FxCb!="Combination or Other"),
                          "Control",
                          "p",
                          "FxCb",
                          "Significance",
                          "iVariable")
```

```
## Joining by: variable
```

![](fixed_price_hypothesis_testing_files/figure-html/H2MDAPabsolute-1.png) 

```r
#Average Number of Offers
PointRowWrapper(NULL,#VAR.main.label,
                "MDAP Contracts\n(Overall and Controls)",          #VAR.row.label,
                "Approximate Average Number of Offers",          #VAR.data.label,
                "Contract Pricing and Significance of Fixed-Price vs. Cost-Based Difference", #VAR.legend.label
                
                Coloration,#VAR.Coloration
    subset(MDAPlongDF,!is.na(Significance)&dVariable=="Average Number of Offers for Competed Contracts" & Control!="Comp." & FxCb!="Combination or Other"),
                          "Control",
                          "Average",
                          "FxCb",
                          "Significance",
                          "iVariable")
```

```
## Joining by: variable
```

![](fixed_price_hypothesis_testing_files/figure-html/H2MDAPabsolute-2.png) 

```r
#Average Number of Changes
PointRowWrapper(NULL,#VAR.main.label,
                "MDAP Contracts\n(Overall and Controls)",          #VAR.row.label,
                "Approximate Average Number of Change Orders",          #VAR.data.label,
                "Contract Pricing and Significance of Fixed-Price vs. Cost-Based Difference", #VAR.legend.label
                
                Coloration,#VAR.Coloration
    subset(MDAPlongDF,!is.na(Significance)&dVariable=="Average Number of Change Orders"  & FxCb!="Combination or Other"),
                          "Control",
                          "Average",
                          "FxCb",
                          "Significance",
                          "iVariable")
```

```
## Joining by: variable
```

![](fixed_price_hypothesis_testing_files/figure-html/H2MDAPabsolute-3.png) 

```r
#Ceiling Raising Change Orders %
PointRowWrapper(NULL,#VAR.main.label,
                "MDAP Contracts\n(Overall and Controls)",          #VAR.row.label,
                "Approximate Average Extent of Ceiling Breaches",          #VAR.data.label,
                "Contract Pricing and Significance of Fixed-Price vs. Cost-Based Difference", #VAR.legend.label
                
                Coloration,#VAR.Coloration
    subset(MDAPlongDF,!is.na(Significance)&dVariable=="Ceiling Raising Change Orders %"   & FxCb!="Combination or Other"),
                          "Control",
                          "Average",
                          "FxCb",
                          "Significance",
                          "iVariable",
    Percentage=TRUE)
```

```
## Joining by: variable
```

![](fixed_price_hypothesis_testing_files/figure-html/H2MDAPabsolute-4.png) 

```r
#Terminations
PointRowWrapper(NULL,#VAR.main.label,
                "MDAP Contracts\n(Overall and Controls)",          #VAR.row.label,
                "Contract Termination Rate",          #VAR.data.label,
                "Contract Pricing and Significance of Fixed-Price vs. Cost-Based Difference", #VAR.legend.label
                
                Coloration,#VAR.Coloration
    subset(MDAPlongDF,!is.na(Significance)&dVariable=="Terminated"  & FxCb!="Combination or Other"),
                          "Control",
                          "p",
                          "FxCb",
                          "Significance",
                          "iVariable")
```

```
## Joining by: variable
```

![](fixed_price_hypothesis_testing_files/figure-html/H2MDAPabsolute-5.png) 



```r
MDAPwideDF<-FixedPriceCast(rbind(MDAPlongDF,PopulationLongDF))
```

```
## Warning: attributes are not identical across measure variables; they will
## be dropped
```

```r
MDAPwideDF$Hypothesis<-factor(MDAPwideDF$Hypothesis,levels=c("MDAP","Population"),ordered=TRUE)

#Remove redundant tests
MDAPwideDF<-subset(MDAPwideDF,
                   !Control %in% unique(MDAPwideDF$Hypothesis))


#Single Offer Competition
PointRowWrapper(NULL,#VAR.main.label,
                "MDAP Contracts\n(Overall and Controls)",          #VAR.row.label,
                "Fixed-to-Cost Ratio for Single Offer Competition Rate",          #VAR.data.label,
                "Contract Sample and Significance of Fixed-Price vs. Cost-Based Difference",
                
                Coloration,#VAR.Coloration
    subset(MDAPwideDF,!is.na(Cost.Based_Significance)&dVariable=="% Single Offer Competition"),
                          "Control",
                          "pFixedCostMargin",
                          "Hypothesis",
                          "Cost.Based_Significance",
                          "iVariable")+geom_hline(aes(yintercept=0))  
```

```
## Joining by: variable
```

![](fixed_price_hypothesis_testing_files/figure-html/H2MDAP-1.png) 

```r
#Average Number of Offers
PointRowWrapper(NULL,#VAR.main.label,
                "MDAP Contracts\n(Overall and Controls)",          #VAR.row.label,
                "Fixed-to-Cost Ratio for Approximate Average Number of Offers",   #VAR.data.label,
                "Contract Sample and Significance of Fixed-Price vs. Cost-Based Difference",
                
                Coloration,#VAR.Coloration
    subset(MDAPwideDF,!is.na(Cost.Based_Significance)&dVariable=="Average Number of Offers for Competed Contracts"),
                          "Control",
                          "avgFixedCostMargin",
                          "Hypothesis",
                          "Cost.Based_Significance",
                          "iVariable",
    Percentage=TRUE)+geom_hline(aes(yintercept=0))
```

```
## Joining by: variable
```

![](fixed_price_hypothesis_testing_files/figure-html/H2MDAP-2.png) 

```r
#Average Number of Changes
PointRowWrapper(NULL,#VAR.main.label,
                "MDAP Contracts\n(Overall and Controls)",          #VAR.row.label,
                "Fixed-to-Cost Ratio for Approximate Average Number of Change Orders",          #VAR.data.label,
                "Contract Sample and Significance of Fixed-Price vs. Cost-Based Difference",
                
                Coloration,#VAR.Coloration
    subset(MDAPwideDF,!is.na(Cost.Based_Significance)&dVariable=="Average Number of Change Orders"),
                          "Control",
                          "avgFixedCostMargin",
                          "Hypothesis",
                          "Cost.Based_Significance",
                          "iVariable",
    Percentage=TRUE)+geom_hline(aes(yintercept=0))
```

```
## Joining by: variable
```

![](fixed_price_hypothesis_testing_files/figure-html/H2MDAP-3.png) 

```r
#Ceiling Raising Change Orders %
PointRowWrapper(NULL,#VAR.main.label,
                "MDAP Contracts\n(Overall and Controls)",          #VAR.row.label,
                "Fixed-to-Cost Ratio for Approximate Average Extent of Ceiling Breaches",          #VAR.data.label,
                "Contract Sample and Significance of Fixed-Price vs. Cost-Based Difference",
                
                Coloration,#VAR.Coloration
    subset(MDAPwideDF,!is.na(Cost.Based_Significance)&dVariable=="Ceiling Raising Change Orders %"),
                          "Control",
                          "avgFixedCostMargin",
                          "Hypothesis",
                          "Cost.Based_Significance",
                          "iVariable",
#     low=-10,
    high=10,
    Percentage=TRUE
    )+geom_hline(aes(yintercept=0))
```

```
## Joining by: variable
```

![](fixed_price_hypothesis_testing_files/figure-html/H2MDAP-4.png) 

```r
MDAPoutliersDF<-ListOutliers(MDAPwideDF,"avgFixedCostMargin",NA,10)


#Terminations
PointRowWrapper(NULL,#VAR.main.label,
                "MDAP Contracts\n(Overall and Controls)",          #VAR.row.label,
                "Fixed-to-Cost Ratio for Termination Rate",          #VAR.data.label,
                "Contract Sample and Significance of Fixed-Price vs. Cost-Based Difference",
                
                Coloration,#VAR.Coloration
    subset(MDAPwideDF,!is.na(Cost.Based_Significance)&dVariable=="Terminated"),
                          "Control",
                          "pFixedCostMargin",
                          "Hypothesis",
                          "Cost.Based_Significance",
                          "iVariable",
#     low=-10,
    high=10
    )+geom_hline(aes(yintercept=0))
```

```
## Joining by: variable
```

```
## Warning in loop_apply(n, do.ply): Removed 1 rows containing missing values
## (geom_point).
```

![](fixed_price_hypothesis_testing_files/figure-html/H2MDAP-5.png) 

```r
MDAPoutliersDF<-rbind(MDAPoutliersDF,ListOutliers(MDAPwideDF,"pFixedCostMargin",NA,10))

write.csv(MDAPwideDF,paste("Output\\",
                          paste("MDAP"
                                ,"Fixed_Price"
                                ,"Hypothesis_Testing"
                                ,"2007-2013"
                                ,sep="_"
                                )
                          ,".csv",
                          sep=""))
```



##H3: Contracts with longer schedules will encounter greater problems with fixed-price contracts.
*	"But price redetermination might be used whenever contingency charges otherwise would be included in a contract price due to such factors as prolonged delivery schedules, unstable market conditions for material or labor, or uncertainty as to cost of performance." (Fixed-prices and Price Redetermination in Defense Contracts)
*	Dependent Variables:
*	Single offer competition, contract termination, number of change orders, and cost of change orders.
*	Initial Taxonomy:
*	Period of performance will be measured by expected contract duration in 4-week months (rounded up).

#Placeholder variable and next steps
This was initially expected to be tested with a continuous value for length rather than a discrete value. In addition the test would benefit from having more granularity at the high end of contract lengths. The next step will be to expand Dur's granularity in the greater than 1 year category.

#Initial results
The hypothesis regarding number of offers was not supported at the greater than 1 year level. Further subdivision into 2 year plus or other breakdowns might show a different result. Combination resembled fixed-price, but did not do as well with 5 or more offers.




```r
    FixedPriceGin[[1]]$levels$Dur
```

```
## [1] "(~1 year,~2 years]"    "(~2 years+]"           "[~2 months,~7 months)"
## [4] "[~7 months-~1 year]"   "[0 months,~2 months)"
```

```r
    LongDurFind<- setEvidence(FixedPriceGin, 
                              nodes=c("Dur"),
                              states=c("(~2 years+]")
                              )
    
    getEvidence(LongDurFind)
```

```
## Finding: 
## Dur: (~2 years+]
## Pr(Finding)= 0.006950983
```

```r
    LongDurLongDF<-FixedPriceComparisonTable(subset(ModelSummary,Dur=="(~2 years+]"),
                                    "2+ Year Dur.")
    
    
    
    
    LongDurLongDF$Hypothesis<-factor(LongDurLongDF$Hypothesis,levels=c("2+ Year Dur.","Population"),ordered=TRUE)
    
    #Remove redundant tests
    LongDurLongDF<-subset(LongDurLongDF,
                      !Control %in% c(unique(LongDurLongDF$Hypothesis),"2+ Year Dur.","<2 Year Dur."))
    
    
    #Single Offer Competition
    PointRowWrapper(NULL,#VAR.main.label,
                    "Long Duration Contracts\n(Overall and Controls)",          #VAR.row.label,
                    "% of Contracts Receiving Single Offer Competition",          #VAR.data.label,
                    "Contract Pricing and Significance of Fixed-Price vs. Cost-Based Difference", #VAR.legend.label
                    
                    Coloration,#VAR.Coloration
        subset(LongDurLongDF,!is.na(Significance)&dVariable=="% Single Offer Competition" & Control!="Comp." & FxCb!="Combination or Other"),
                              "Control",
                              "p",
                              "FxCb",
                              "Significance",
                              "iVariable")
```

```
## Joining by: variable
```

![](fixed_price_hypothesis_testing_files/figure-html/H3LongDurAbsolute-1.png) 

```r
    #Average Number of Offers
    PointRowWrapper(NULL,#VAR.main.label,
                    "Long Duration Contracts\n(Overall and Controls)",          #VAR.row.label,
                    "Approximate Average Number of Offers",          #VAR.data.label,
                    "Contract Pricing and Significance of Fixed-Price vs. Cost-Based Difference", #VAR.legend.label
                    
                    Coloration,#VAR.Coloration
        subset(LongDurLongDF,!is.na(Significance)&dVariable=="Average Number of Offers for Competed Contracts" & Control!="Comp." & FxCb!="Combination or Other"),
                              "Control",
                              "Average",
                              "FxCb",
                              "Significance",
                              "iVariable")
```

```
## Joining by: variable
```

![](fixed_price_hypothesis_testing_files/figure-html/H3LongDurAbsolute-2.png) 

```r
    #Average Number of Changes
    PointRowWrapper(NULL,#VAR.main.label,
                    "Long Duration Contracts\n(Overall and Controls)",          #VAR.row.label,
                    "Approximate Average Number of Change Orders",          #VAR.data.label,
                    "Contract Pricing and Significance of Fixed-Price vs. Cost-Based Difference", #VAR.legend.label
                    
                    Coloration,#VAR.Coloration
        subset(LongDurLongDF,!is.na(Significance)&dVariable=="Average Number of Change Orders"  & FxCb!="Combination or Other"),
                              "Control",
                              "Average",
                              "FxCb",
                              "Significance",
                              "iVariable")
```

```
## Joining by: variable
```

![](fixed_price_hypothesis_testing_files/figure-html/H3LongDurAbsolute-3.png) 

```r
    #Ceiling Raising Change Orders %
    PointRowWrapper(NULL,#VAR.main.label,
                    "Long Duration Contracts\n(Overall and Controls)",          #VAR.row.label,
                    "Approximate Average Extent of Ceiling Breaches",          #VAR.data.label,
                    "Contract Pricing and Significance of Fixed-Price vs. Cost-Based Difference", #VAR.legend.label
                    
                    Coloration,#VAR.Coloration
        subset(LongDurLongDF,!is.na(Significance)&dVariable=="Ceiling Raising Change Orders %"  & FxCb!="Combination or Other"),
                              "Control",
                              "Average",
                              "FxCb",
                              "Significance",
                              "iVariable",
    Percentage=TRUE)
```

```
## Joining by: variable
```

![](fixed_price_hypothesis_testing_files/figure-html/H3LongDurAbsolute-4.png) 

```r
    #Terminations
    PointRowWrapper(NULL,#VAR.main.label,
                    "Long Duration Contracts\n(Overall and Controls)",          #VAR.row.label,
                    "Contract Termination Rate",          #VAR.data.label,
                    "Contract Pricing and Significance of Fixed-Price vs. Cost-Based Difference", #VAR.legend.label
                    
                    Coloration,#VAR.Coloration
        subset(LongDurLongDF,!is.na(Significance)&dVariable=="Terminated"  & FxCb!="Combination or Other"),
                              "Control",
                              "p",
                              "FxCb",
                              "Significance",
                              "iVariable")
```

```
## Joining by: variable
```

![](fixed_price_hypothesis_testing_files/figure-html/H3LongDurAbsolute-5.png) 

```r
    rm(LongDurFind)
```


```r
LongDurWideDF<-FixedPriceCast(rbind(LongDurLongDF,PopulationLongDF))
```

```
## Warning: attributes are not identical across measure variables; they will
## be dropped
```

```r
LongDurWideDF$Hypothesis<-factor(LongDurWideDF$Hypothesis,levels=c("2+ Year Dur.","Population"),ordered=TRUE)

#Remove redundant tests
LongDurWideDF<-subset(LongDurWideDF,
                  !Control %in% c(unique(LongDurWideDF$Hypothesis),"2+ Year Dur."))


#Single Offer Competition
PointRowWrapper(NULL,#VAR.main.label,
                "Long Duration Contracts\n(Overall and Controls)",          #VAR.row.label,
                "Fixed-to-Cost Ratio for Single Offer Competition Rate",          #VAR.data.label,
                "Contract Sample and Significance of Fixed-Price vs. Cost-Based Difference",
                
                Coloration,#VAR.Coloration
    subset(LongDurWideDF,!is.na(Cost.Based_Significance)&dVariable=="% Single Offer Competition"),
                          "Control",
                          "pFixedCostMargin",
                          "Hypothesis",
                          "Cost.Based_Significance",
                          "iVariable")+geom_hline(aes(yintercept=0))  
```

```
## Joining by: variable
```

![](fixed_price_hypothesis_testing_files/figure-html/H3LongDur-1.png) 

```r
#Average Number of Offers
PointRowWrapper(NULL,#VAR.main.label,
                "Long Duration Contracts\n(Overall and Controls)",          #VAR.row.label,
                "Fixed-to-Cost Ratio for Approximate Average Number of Offers",   #VAR.data.label,
                "Contract Sample and Significance of Fixed-Price vs. Cost-Based Difference",
                
                Coloration,#VAR.Coloration
    subset(LongDurWideDF,!is.na(Cost.Based_Significance)&dVariable=="Average Number of Offers for Competed Contracts"),
                          "Control",
                          "avgFixedCostMargin",
                          "Hypothesis",
                          "Cost.Based_Significance",
                          "iVariable",
    Percentage=TRUE)+geom_hline(aes(yintercept=0))
```

```
## Joining by: variable
```

![](fixed_price_hypothesis_testing_files/figure-html/H3LongDur-2.png) 

```r
#Average Number of Changes
PointRowWrapper(NULL,#VAR.main.label,
                "Long Duration Contracts\n(Overall and Controls)",          #VAR.row.label,
                "Fixed-to-Cost Ratio for Approximate Average Number of Change Orders",          #VAR.data.label,
                "Contract Sample and Significance of Fixed-Price vs. Cost-Based Difference",
                
                Coloration,#VAR.Coloration
    subset(LongDurWideDF,!is.na(Cost.Based_Significance)&dVariable=="Average Number of Change Orders"),
                          "Control",
                          "avgFixedCostMargin",
                          "Hypothesis",
                          "Cost.Based_Significance",
                          "iVariable",
    Percentage=TRUE)+geom_hline(aes(yintercept=0))
```

```
## Joining by: variable
```

![](fixed_price_hypothesis_testing_files/figure-html/H3LongDur-3.png) 

```r
#Ceiling Raising Change Orders %
PointRowWrapper(NULL,#VAR.main.label,
                "Long Duration Contracts\n(Overall and Controls)",          #VAR.row.label,
                "Fixed-to-Cost Ratio for Approximate Average Extent of Ceiling Breaches",          #VAR.data.label,
                "Contract Sample and Significance of Fixed-Price vs. Cost-Based Difference",
                
                Coloration,#VAR.Coloration
    subset(LongDurWideDF,!is.na(Cost.Based_Significance)&dVariable=="Ceiling Raising Change Orders %"),
                          "Control",
                          "avgFixedCostMargin",
                          "Hypothesis",
                          "Cost.Based_Significance",
                          "iVariable",
#     low=-10,
    high=10,
    Percentage=TRUE
    )+geom_hline(aes(yintercept=0))
```

```
## Joining by: variable
```

![](fixed_price_hypothesis_testing_files/figure-html/H3LongDur-4.png) 

```r
LongDurOutliersDF<-ListOutliers(LongDurWideDF,"avgFixedCostMargin",NA,10)


#Terminations
PointRowWrapper(NULL,#VAR.main.label,
                "Long Duration Contracts\n(Overall and Controls)",          #VAR.row.label,
                "Fixed-to-Cost Ratio for Termination Rate",          #VAR.data.label,
                "Contract Sample and Significance of Fixed-Price vs. Cost-Based Difference",
                
                Coloration,#VAR.Coloration
    subset(LongDurWideDF,!is.na(Cost.Based_Significance)&dVariable=="Terminated"),
                          "Control",
                          "pFixedCostMargin",
                          "Hypothesis",
                          "Cost.Based_Significance",
                          "iVariable")+geom_hline(aes(yintercept=0))
```

```
## Joining by: variable
```

```
## Warning in loop_apply(n, do.ply): Removed 1 rows containing missing values
## (geom_point).
```

![](fixed_price_hypothesis_testing_files/figure-html/H3LongDur-5.png) 

```r
write.csv(LongDurWideDF,paste("Output\\",
                          paste("LongDur"
                                ,"Fixed_Price"
                                ,"Hypothesis_Testing"
                                ,"2007-2013"
                                ,sep="_"
                                )
                          ,".csv",
                          sep=""))
```



##H4: Fixed-price contracts with the potential competition for multiple offers will encounter fewer challenges.
*	"[Principals] would prefer a fixed-price contract when the number of bidders increases." (On Contracting for Uncertain R&D)
*	Complicating this relationship, are recent revisions to the DFAR that reduced the emphasis on seeking additional offers as long as a fair competitive price can be verified. (DFAR 215.371)
*	Dependent Variables:
*	Contract termination, number of change orders, and cost of change orders.
*	Initial Taxonomy:
*	Due to limitations in available data, CSIS will use whether or not a contract is initially competed as a broad proxy for whether competition with multiple offers is available. CSIS will follow standard DoD procedures in identifying competed contracts.
*	Note: Testing for this hypothesis is constrained by a lack of relevant data in  FPDS. The number of offers requested field was dispensed with when the DoD contract system was folded into FPDS. (See "Hypotheses which raise causality concerns" (slide 21-22) for further discussion.) 

#Next steps.

This hypothesis is requires incorporation of the next two dependent variables into the Bayesian Network.



```r
FixedPriceGin[[1]]$levels$Comp
```

```
## [1] "Comp."     "No Comp."  "Unlabeled"
```

```r
CompFind<-setEvidence(FixedPriceGin, 
                      nodes=c("Comp"),
                      states=c("Comp.")
                      )

getEvidence(CompFind)
```

```
## Finding: 
## Comp: Comp.
## Pr(Finding)= 0.8009173
```

```r
CompLongDF<-FixedPriceComparisonTable(subset(ModelSummary,Comp=="Comp."),
                                      "Comp.")




#Remove redundant tests
CompLongDF<-subset(CompLongDF,
               !Control %in% c(unique(CompLongDF$Hypothesis),"Not Comp."))
#Remove the  iVariables not covered in hypothesis
CompLongDF<-subset(CompLongDF,
               !dVariable %in% c("1","Average Number of Offers"))

    
    
    #Average Number of Changes
    PointRowWrapper(NULL,#VAR.main.label,
                    "Competed Contracts\n(Overall and Controls)",          #VAR.row.label,
                    "Approximate Average Number of Change Orders",          #VAR.data.label,
                    "Contract Pricing and Significance of Fixed-Price vs. Cost-Based Difference", #VAR.legend.label
                    
                    Coloration,#VAR.Coloration
        subset(CompLongDF,!is.na(Significance)&dVariable=="Average Number of Change Orders"  & FxCb!="Combination or Other"),
                              "Control",
                              "Average",
                              "FxCb",
                              "Significance",
                              "iVariable")
```

```
## Joining by: variable
```

![](fixed_price_hypothesis_testing_files/figure-html/H4CompAbsolute-1.png) 

```r
    #Ceiling Raising Change Orders %
    PointRowWrapper(NULL,#VAR.main.label,
                    "Competed Contracts\n(Overall and Controls)",          #VAR.row.label,
                    "Approximate Average Extent of Ceiling Breaches",          #VAR.data.label,
                    "Contract Pricing and Significance of Fixed-Price vs. Cost-Based Difference", #VAR.legend.label
                    
                    Coloration,#VAR.Coloration
        subset(CompLongDF,!is.na(Significance)&dVariable=="Ceiling Raising Change Orders %"   & FxCb!="Combination or Other"),
                              "Control",
                              "Average",
                              "FxCb",
                              "Significance",
                              "iVariable",
    Percentage=TRUE)
```

```
## Joining by: variable
```

![](fixed_price_hypothesis_testing_files/figure-html/H4CompAbsolute-2.png) 

```r
    #Terminations
    PointRowWrapper(NULL,#VAR.main.label,
                    "Competed Contracts\n(Overall and Controls)",          #VAR.row.label,
                    "Contract Termination Rate",          #VAR.data.label,
                    "Contract Pricing and Significance of Fixed-Price vs. Cost-Based Difference", #VAR.legend.label
                    
                    Coloration,#VAR.Coloration
        subset(CompLongDF,!is.na(Significance)&dVariable=="Terminated"  & FxCb!="Combination or Other"),
                              "Control",
                              "p",
                              "FxCb",
                              "Significance",
                              "iVariable")
```

```
## Joining by: variable
```

![](fixed_price_hypothesis_testing_files/figure-html/H4CompAbsolute-3.png) 

```r
rm(CompFind)
```



```r
CompWideDF<-FixedPriceCast(rbind(CompLongDF,
                                    PopulationLongDF))
```

```
## Warning: attributes are not identical across measure variables; they will
## be dropped
```

```r
CompWideDF$Hypothesis<-factor(CompWideDF$Hypothesis,levels=c("Comp.","Population"),ordered=TRUE)

#Remove redundant tests
CompWideDF<-subset(CompWideDF,
               !Control %in% c(unique(CompWideDF$Hypothesis),"Not Comp."))
#Remove the  iVariables not covered in hypothesis
CompWideDF<-subset(CompWideDF,
               !dVariable %in% c("1","Average Number of Offers"))



#Average Number of Changes
PointRowWrapper(NULL,#VAR.main.label,
                "Competed Contracts\n(Overall and Controls)",          #VAR.row.label,
                "Fixed-to-Cost Ratio for Approximate Average Number of Change Orders",          #VAR.data.label,
                "Contract Sample and Significance of Fixed-Price vs. Cost-Based Difference",
                
                Coloration,#VAR.Coloration
    subset(CompWideDF,!is.na(Cost.Based_Significance)&dVariable=="Average Number of Change Orders"),
                          "Control",
                          "avgFixedCostMargin",
                          "Hypothesis",
                          "Cost.Based_Significance",
                          "iVariable",
    Percentage=TRUE)+geom_hline(aes(yintercept=0))
```

```
## Joining by: variable
```

![](fixed_price_hypothesis_testing_files/figure-html/H4Comp-1.png) 

```r
#Ceiling Raising Change Orders %
# debug(PointRowWrapper)
PointRowWrapper(NULL,#VAR.main.label,
                "Competed Contracts\n(Overall and Controls)",          #VAR.row.label,
                "Fixed-to-Cost Ratio for Approximate Average Extent of Ceiling Breaches",          #VAR.data.label,
                "Contract Sample and Significance of Fixed-Price vs. Cost-Based Difference",
                
                Coloration,#VAR.Coloration
    subset(CompWideDF,!is.na(Cost.Based_Significance)&dVariable=="Ceiling Raising Change Orders %"),
                          "Control",
                          "avgFixedCostMargin",
                          "Hypothesis",
                          "Cost.Based_Significance",
                          "iVariable",
    low=-10,
    high=10,
    Percentage=TRUE
    )+geom_hline(aes(yintercept=0))
```

```
## Joining by: variable
```

![](fixed_price_hypothesis_testing_files/figure-html/H4Comp-2.png) 

```r
CompOutliersDF<-ListOutliers(CompWideDF,"avgFixedCostMargin",-10,10)

#Terminations
PointRowWrapper(NULL,#VAR.main.label,
                "Competed Contracts\n(Overall and Controls)",          #VAR.row.label,
                "Fixed-to-Cost Ratio for Termination Rate",          #VAR.data.label,
                "Contract Sample and Significance of Fixed-Price vs. Cost-Based Difference",
                
                Coloration,#VAR.Coloration
    subset(CompWideDF,!is.na(Cost.Based_Significance)&dVariable=="Terminated"),
                          "Control",
                          "pFixedCostMargin",
                          "Hypothesis",
                          "Cost.Based_Significance",
                          "iVariable",
#     low=-10,
    high=10
    )+geom_hline(aes(yintercept=0))
```

```
## Joining by: variable
```

```
## Warning in loop_apply(n, do.ply): Removed 1 rows containing missing values
## (geom_point).
```

![](fixed_price_hypothesis_testing_files/figure-html/H4Comp-3.png) 

```r
CompOutliersDF<-rbind(CompOutliersDF,ListOutliers(CompWideDF,"pFixedCostMargin",NA,10))

write.csv(CompWideDF,paste("Output\\",
                          paste("Comp"
                                ,"Fixed_Price"
                                ,"Hypothesis_Testing"
                                ,"2007-2013"
                                ,sep="_"
                                )
                          ,".csv",
                          sep=""))
```

##H5: Fixed-price are preferred by vendors for larger software contracts
*	"We hypothesize that the vendor's ability to leverage information asymmetry about capabilities and experiences translates into the vendor preferring Fixed-Price contract to secure larger information rents. Our results support this hypothesis and suggest that the vendor would prefer the FP contract for larger and longer projects with larger teams. However, vendors would prefer a [Time and Materials] contract when the risk of employee attrition from the project team is high." (Research Note: On Vendor Preferences for Contract Types in Offshore Software Projects: The Case of fixed-price vs. Time and Materials Contracts)
*	Dependent Variables:
*	Single offer competition and contract termination. 
*	Initial Taxonomy:
*	Past CSIS research has found that software contracts are not all contained within the Automated Data Processing category of the product or service codes. CSIS tentatively plans to include contracts with greater than 50% by value content in the Information and Communication Technology services category
*	Note: This hypothesis tests commercial vendor practices, but contradicts other government-focused observations. This is because this study does not seek to construct only consistent hypotheses, but instead to test a range of ideas from the literature.
Hypotheses regarding general contract performance that may be used as controls
Contracts for aircraft have more challenges than other contracts

#Placeholder and next steps
Software is typically classified as a service under PSR, though also could be under R&D, and is especially prominent in electronics in communication under What. Given that all sorts of platforms have software, PSR would be the best place to incorporate it under the present model, possibly linking software services and software R&D. NAICS codes may also assist. Software is not particularly easy to identify in FPDS as it is often incorporated into larger contracts, but there should be a number of clearcut cases.

#Initial results

Electronics and communications services do have a slight preference for fixed-price contracts when it comes to number of offers. However, when R&D is also included, the advantage switches to Cost-based.


```r
FixedPriceGin[[1]]$levels$Soft
```

```
## [1] "Not Software Eng."      "Possible Software Eng."
```

```r
SoftwareFind<- setEvidence(FixedPriceGin, 
                           nodes=c("Soft"),
                           states=c("Possible Software Eng."))


getEvidence(SoftwareFind)
```

```
## Finding: 
## Soft: Possible Software Eng.
## Pr(Finding)= 0.003934603
```

```r
SoftwareLongDF<-FixedPriceComparisonTable(subset(ModelSummary,Soft=="Possible Software Eng."),
                                      "Software Dev.")




SoftwareLongDF$Hypothesis<-factor(SoftwareLongDF$Hypothesis,levels=c("Comp.","Software Dev."),ordered=TRUE)

#Remove redundant tests
SoftwareLongDF<-subset(SoftwareLongDF,
                   !Control %in% unique(SoftwareLongDF$Hypothesis))

#Single Offer Competition
PointRowWrapper(NULL,#VAR.main.label,
                "Software Contracts\n(Overall and Controls)",          #VAR.row.label,
                "% of Contracts Receiving Single Offer Competition",          #VAR.data.label,
                "Contract Pricing and Significance of Fixed-Price vs. Cost-Based Difference", #VAR.legend.label
                
                Coloration,#VAR.Coloration
    subset(SoftwareLongDF,!is.na(Significance)&dVariable=="% Single Offer Competition" & Control!="Comp." & FxCb!="Combination or Other"),
                          "Control",
                          "p",
                          "FxCb",
                          "Significance",
                          "iVariable")
```

```
## Joining by: variable
```

![](fixed_price_hypothesis_testing_files/figure-html/H5SoftwareAbsolute-1.png) 

```r
#Average Number of Offers
PointRowWrapper(NULL,#VAR.main.label,
                "Software Contracts\n(Overall and Controls)",          #VAR.row.label,
                "Approximate Average Number of Offers",          #VAR.data.label,
                "Contract Pricing and Significance of Fixed-Price vs. Cost-Based Difference", #VAR.legend.label
                
                Coloration,#VAR.Coloration
    subset(SoftwareLongDF,!is.na(Significance)&dVariable=="Average Number of Offers for Competed Contracts" & Control!="Comp." & FxCb!="Combination or Other"),
                          "Control",
                          "Average",
                          "FxCb",
                          "Significance",
                          "iVariable")
```

```
## Joining by: variable
```

![](fixed_price_hypothesis_testing_files/figure-html/H5SoftwareAbsolute-2.png) 

```r
#Average Number of Changes
PointRowWrapper(NULL,#VAR.main.label,
                "Software Contracts\n(Overall and Controls)",          #VAR.row.label,
                "Approximate Average Number of Change Orders",          #VAR.data.label,
                "Contract Pricing and Significance of Fixed-Price vs. Cost-Based Difference", #VAR.legend.label
                
                Coloration,#VAR.Coloration
    subset(SoftwareLongDF,!is.na(Significance)&dVariable=="Average Number of Change Orders"  & FxCb!="Combination or Other"),
                          "Control",
                          "Average",
                          "FxCb",
                          "Significance",
                          "iVariable")
```

```
## Joining by: variable
```

![](fixed_price_hypothesis_testing_files/figure-html/H5SoftwareAbsolute-3.png) 

```r
#Ceiling Raising Change Orders %
PointRowWrapper(NULL,#VAR.main.label,
                "Software Contracts\n(Overall and Controls)",          #VAR.row.label,
                "Approximate Average Extent of Ceiling Breaches",          #VAR.data.label,
                "Contract Pricing and Significance of Fixed-Price vs. Cost-Based Difference", #VAR.legend.label
                
                Coloration,#VAR.Coloration
    subset(SoftwareLongDF,!is.na(Significance)&dVariable=="Ceiling Raising Change Orders %"  & FxCb!="Combination or Other"),
                          "Control",
                          "Average",
                          "FxCb",
                          "Significance",
                          "iVariable",
    Percentage=TRUE)
```

```
## Joining by: variable
```

![](fixed_price_hypothesis_testing_files/figure-html/H5SoftwareAbsolute-4.png) 

```r
#Terminations
PointRowWrapper(NULL,#VAR.main.label,
                "Software Contracts\n(Overall and Controls)",          #VAR.row.label,
                "Contract Termination Rate",          #VAR.data.label,
                "Contract Pricing and Significance of Fixed-Price vs. Cost-Based Difference", #VAR.legend.label
                
                Coloration,#VAR.Coloration
    subset(SoftwareLongDF,!is.na(Significance)&dVariable=="Terminated"  & FxCb!="Combination or Other"),
                          "Control",
                          "p",
                          "FxCb",
                          "Significance",
                          "iVariable")
```

```
## Joining by: variable
```

![](fixed_price_hypothesis_testing_files/figure-html/H5SoftwareAbsolute-5.png) 

```r
rm(SoftwareFind)
```



```r
SoftwareWideDF<-FixedPriceCast(rbind(SoftwareLongDF,PopulationLongDF))
```

```
## Warning: attributes are not identical across measure variables; they will
## be dropped
```

```r
SoftwareWideDF$Hypothesis<-factor(SoftwareWideDF$Hypothesis,levels=c("Software Dev.","Population"),ordered=TRUE)

#Remove redundant tests
SoftwareWideDF<-subset(SoftwareWideDF,
                   !Control %in% unique(SoftwareWideDF$Hypothesis))

#Single Offer Competition
PointRowWrapper(NULL,#VAR.main.label,
                "Software Contracts\n(Overall and Controls)",          #VAR.row.label,
                "Fixed-to-Cost Ratio for Single Offer Competition Rate",          #VAR.data.label,
                "Contract Sample and Significance of Fixed-Price vs. Cost-Based Difference",
                
                Coloration,#VAR.Coloration
    subset(SoftwareWideDF,!is.na(Cost.Based_Significance)&dVariable=="% Single Offer Competition"),
                          "Control",
                          "pFixedCostMargin",
                          "Hypothesis",
                          "Cost.Based_Significance",
                          "iVariable")+geom_hline(aes(yintercept=0))  
```

```
## Joining by: variable
```

```
## Warning in loop_apply(n, do.ply): Removed 1 rows containing missing values
## (geom_point).
```

```
## Warning in loop_apply(n, do.ply): Removed 1 rows containing missing values
## (geom_point).
```

![](fixed_price_hypothesis_testing_files/figure-html/H5Software-1.png) 

```r
#Average Number of Offers
PointRowWrapper(NULL,#VAR.main.label,
                "Software Contracts\n(Overall and Controls)",          #VAR.row.label,
                "Fixed-to-Cost Ratio for Approximate Average Number of Offers",   #VAR.data.label,
                "Contract Sample and Significance of Fixed-Price vs. Cost-Based Difference",
                
                Coloration,#VAR.Coloration
    subset(SoftwareWideDF,!is.na(Cost.Based_Significance)&dVariable=="Average Number of Offers for Competed Contracts"),
                          "Control",
                          "avgFixedCostMargin",
                          "Hypothesis",
                          "Cost.Based_Significance",
                          "iVariable",
    Percentage=TRUE)  +geom_hline(aes(yintercept=0))
```

```
## Joining by: variable
```

```
## Warning in loop_apply(n, do.ply): Removed 1 rows containing missing values
## (geom_point).
```

![](fixed_price_hypothesis_testing_files/figure-html/H5Software-2.png) 

```r
#Average Number of Changes
PointRowWrapper(NULL,#VAR.main.label,
                "Software Contracts\n(Overall and Controls)",          #VAR.row.label,
                "Fixed-to-Cost Ratio for Approximate Average Number of Change Orders",          #VAR.data.label,
                "Contract Sample and Significance of Fixed-Price vs. Cost-Based Difference",
                
                Coloration,#VAR.Coloration
    subset(SoftwareWideDF,!is.na(Cost.Based_Significance)&dVariable=="Average Number of Change Orders"),
                          "Control",
                          "avgFixedCostMargin",
                          "Hypothesis",
                          "Cost.Based_Significance",
                          "iVariable",
#     low=-10,
    high=10,
    Percentage=TRUE
    )+geom_hline(aes(yintercept=0))
```

```
## Joining by: variable
```

```
## Warning in loop_apply(n, do.ply): Removed 1 rows containing missing values
## (geom_point).
```

```
## Warning in loop_apply(n, do.ply): Removed 1 rows containing missing values
## (geom_point).
```

![](fixed_price_hypothesis_testing_files/figure-html/H5Software-3.png) 

```r
SoftwareOutliersDF<-rbind(ListOutliers(SoftwareWideDF,"pFixedCostMargin",NA,10))


#Ceiling Raising Change Orders %
PointRowWrapper(NULL,#VAR.main.label,
                "Software Contracts\n(Overall and Controls)",          #VAR.row.label,
                "Fixed-to-Cost Ratio for Approximate Average Extent of Ceiling Breaches",          #VAR.data.label,
                "Contract Sample and Significance of Fixed-Price vs. Cost-Based Difference",
                
                Coloration,#VAR.Coloration
    subset(SoftwareWideDF,!is.na(Cost.Based_Significance)&dVariable=="Ceiling Raising Change Orders %"),
                          "Control",
                          "avgFixedCostMargin",
                          "Hypothesis",
                          "Cost.Based_Significance",
                          "iVariable",
    low=-10,
    high=10,
    Percentage=TRUE
    )+geom_hline(aes(yintercept=0))
```

```
## Joining by: variable
```

```
## Warning in loop_apply(n, do.ply): Removed 1 rows containing missing values
## (geom_point).
```

```
## Warning in loop_apply(n, do.ply): Removed 1 rows containing missing values
## (geom_point).
```

```
## Warning in loop_apply(n, do.ply): Removed 2 rows containing missing values
## (geom_point).
```

![](fixed_price_hypothesis_testing_files/figure-html/H5Software-4.png) 

```r
SoftwareOutliersDF<-rbind(SoftwareOutliersDF,ListOutliers(SoftwareOutliersDF,"avgFixedCostMargin",-10,10))


#Terminations
PointRowWrapper(NULL,#VAR.main.label,
                "Software Contracts\n(Overall and Controls)",          #VAR.row.label,
                "Fixed-to-Cost Ratio for Termination Rate",          #VAR.data.label,
                "Contract Sample and Significance of Fixed-Price vs. Cost-Based Difference",
                
                Coloration,#VAR.Coloration
    subset(SoftwareWideDF,!is.na(Cost.Based_Significance)&dVariable=="Terminated"),
                          "Control",
                          "pFixedCostMargin",
                          "Hypothesis",
                          "Cost.Based_Significance",
                          "iVariable",
#     low=-10,
    high=10
    )+geom_hline(aes(yintercept=0))
```

```
## Joining by: variable
```

```
## Warning in loop_apply(n, do.ply): Removed 3 rows containing missing values
## (geom_point).
```

```
## Warning in loop_apply(n, do.ply): Removed 2 rows containing missing values
## (geom_point).
```

```
## Warning in loop_apply(n, do.ply): Removed 1 rows containing missing values
## (geom_point).
```

```
## Warning in loop_apply(n, do.ply): Removed 1 rows containing missing values
## (geom_point).
```

![](fixed_price_hypothesis_testing_files/figure-html/H5Software-5.png) 

```r
SoftwareOutliersDF<-rbind(SoftwareOutliersDF,ListOutliers(SoftwareWideDF,"pFixedCostMargin",NA,10))


write.csv(SoftwareWideDF,paste("Output\\",
                          paste("Software"
                                ,"Fixed_Price"
                                ,"Hypothesis_Testing"
                                ,"2007-2013"
                                ,sep="_"
                                )
                          ,".csv",
                          sep=""))
```

