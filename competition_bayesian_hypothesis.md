

```
## Bioconductor version 3.0 (BiocInstaller 1.16.2), ?biocLite for help
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
## Loading required package: knitr
```

Contracts are classified using a mix of numerical and categorical variables. While the changes in numerical variables are easy to grasp and summarize, a contract may have one line item that is competed and another that is not. As is detailed in the [exploration on R&D](RnD_1to5_exploration.md), we are only considering information available prior to contract start. The percentage of contract obligations that were competed is a valuable benchmark, but is highly influenced by factors that occured after contract start..

##Studying contract vehicle within the sample.
Describe contract vehicle here.

After the team developed the initial list and a small number of elements accidentally left off were corrected for, the next step was to do a preliminary run through with Bayesian learning. 



Noteably this initial run through produced two warnings of challenging relationships IDV to Dur and IDV to FxCb. After consulting within the team, the choice was made to add whitelisted relationships that went against those automatically produced.

IDV (IsIDV) new white lists:
* Whitelist to FxCb (FixedOrCost) because the pricing mechanism of most IDVs are set. Thus, once the choice of which IDV to use is made, the pricing mechanism would be set automatically. Furthermore, Fixed-price is often the mechanism because the IDV allows for multiple instances of a single contract reducing the need for the flexabilit Cost-Based contracts offer.
* Whitelist to Dur (Duration) because the use of the IDV mechanism typically means multiple shorter contractors under one IDV rather than a single longer contract. 

With those two connections in hand, all three Bayesian Learning algorith were applied.

To the study teams dismay, the bayesian learning results were not consistent across different methods. The unoptimized versions of the learning algorithms do better at catching statistical oddities at the cost of requiring more comparisons. After analyzing the unoptimized and optimized versions, the team chose to adopt one arc that appeared in the both unoptimized versions, namely duration to offers. Seperate analysis shown in the Contract_Competition found that longer contracts did have fewer offers even after controlling for contract ceiling.

Similarly, the grow shrink algorithm included a link from Ceil to FxCb. Analysis shown in Contract_Pricing holds up that connection, even after controlling for PSR. This does not appear to be true of duration or competition. There may well be a link between IDV and Fixed-price, but there's enough complexity in the causality in that relationship that the team chose not to force it.

* Whitelist Ceil (Ceil) to FxCb because higher ceiling contracts are more likely to be cost-based or combination.

Dur (Duration) to Number of Offers received (Offr) was considered, but this ended up resulting in the Ceil to Dur being lost. Of the two, Ceil has the more straightforward relationship, so it was kept. 


The other alternative within the package, a min-max parents and childrens approach, 



```r
require(gRain)
# cad.cpt <- extractCPT(ContractModel, as.graphNEL(CompetitionNetwork), smooth = 0.001)
# compGin <- grain(compileCPT(cad.cpt))

CompGin<-load("data/CompGin.rData")
summary(compGin)
```

```
## Independence network: Compiled: FALSE Propagated: FALSE 
##  Nodes : Named chr [1:11] "IDV" "FxCb" "Comp" "Link" "Who" "What" ...
##  - attr(*, "names")= chr [1:11] "IDV" "FxCb" "Comp" "Link" ...
```

```r
QueryCrossSectionOnNode<-function(
    gin,
    PivotNode,
    QueryNode
    )
    {
    looper<-NULL
    for(x in unlist(compGin$universe$levels[PivotNode])){
        looper<-rbind(looper,list(evidence=getEvidence(setEvidence(gin,
                                                                   nodes = c(PivotNode),
                                                                   states = c(x)
                                                                   
                                                                   )),
                                  result=querygrain(setEvidence(gin,
                                                                nodes = c(PivotNode),
                                                                states = c(x))
                                                    , nodes = c(QueryNode)
                                                    )
                                  )
                      )
        }
    looper
    }
```



##C1 Aircraft encounter

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
AircraftFind<- setEvidence(compGin, 
                           nodes=c("What"),
                           states=c("Aircraft and Drones")
                           )


getEvidence(AircraftFind)
```

```
## Finding: 
## What: Aircraft and Drones
## Pr(Finding)= 0.09659616
```

```r
querygrain(AircraftFind,"Offr")
```

```
## $Offr
## Offr
##         1         2       3-4        5+ 
## 0.5354625 0.1840005 0.1297155 0.1508214
```

```r
NotAircraftFind<- setEvidence(compGin, 
                              nodes=c("What"),
                              states=c(list(
                                  unlist(compGin$universe$levels["What"])[unlist(compGin$universe$levels["What"]) != 
                                                                              "Aircraft and Drones"]))
                              )

getEvidence(NotAircraftFind)
```

```
## Finding: 
## What: Electronics and Communications, Facilities and Construction, Land Vehicles, Missile and Space Systems, Other, Ships & Submarines, Weapons and Ammunition
## Pr(Finding)= 0.9034038
```

```r
querygrain(NotAircraftFind,"Offr")
```

```
## $Offr
## Offr
##         1         2       3-4        5+ 
## 0.3296511 0.1717030 0.2053938 0.2932521
```


##C2: Undefinitized contract actions (UCAs) in development will encounter more problems
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

##Contracts that are initially expected to be very large contracts will encounter more problems.
*	For overall DoD contracting in 2013, contracts with an annual value of $500 or more saw the lowest rate of competition with three or more offers- 20 percent. The next lowest rate for contracts between $100 million and $500 million (26 percent). (CSIS Analysis of FPDS Data).
*	Dependent Variables:
*	Single offer competition, contract termination, number of change orders, and cost of change orders.
*	Initial Taxonomy:
*	The sample will include all contracts with an initial annualized base and all options amount of at least $500 million, or a initial base and exercised options amount of at least $500 million.

###Placeholder and next steps.
$500M contracts is not a level of granularity yet available in the Ceil variable, so the lower $30M threshold will be used for the initial analysis. In addition, the criteria we are using is base and all values alone, but that should always match or exceed the base and exercised value. 


###Initial results.
At the $30M ceil level the results were ambigious. For all contracts, single offer is slightly more common among large contracts but competition with 3-4 and 5+. For contracts that were competed, large contracts actually received more offers.


```r
unlist(compGin$universe$levels["Ceil"])
```

```
##        Ceil1        Ceil2        Ceil3        Ceil4        Ceil5 
##    "[0,15k)" "[15k,100k)"  "[100k,1m)"   "[1m,30m)"     "[30m+]"
```

```r
LargeCeilFind<- setEvidence(compGin, 
                            nodes=c("Ceil"),
                            states=c("[30m+]")
                            )

LargeCeilCompFind<- setEvidence(LargeCeilFind, 
                                nodes=c("Comp"),
                                states=c("Comp.")
                                )


getEvidence(LargeCeilFind)
```

```
## Finding: 
## Ceil: [30m+]
## Pr(Finding)= 0.0009290387
```

```r
querygrain(LargeCeilFind,"Offr")
```

```
## $Offr
## Offr
##         1         2       3-4        5+ 
## 0.3551266 0.1404792 0.1760890 0.3283052
```

```r
getEvidence(LargeCeilCompFind)
```

```
## Finding: 
## Ceil: [30m+]
## Comp: Comp.
## Pr(Finding)= 0.0006982732
```

```r
querygrain(LargeCeilCompFind,"Offr")
```

```
## $Offr
## Offr
##         1         2       3-4        5+ 
## 0.1565562 0.1836192 0.2279471 0.4318775
```

```r
NotLargeCeilFind<- setEvidence(compGin, 
                               nodes=c("Ceil"),
                               states=c(list(
                                   unlist(compGin$universe$levels["Ceil"])[unlist(compGin$universe$levels["Ceil"])!=
                                                                               "[30m+]"]))
                               )

getEvidence(NotLargeCeilFind)
```

```
## Finding: 
## Ceil: [0,15k), [15k,100k), [100k,1m), [1m,30m)
## Pr(Finding)= 0.999071
```

```r
querygrain(NotLargeCeilFind,"Offr")
```

```
## $Offr
## Offr
##         1         2       3-4        5+ 
## 0.3495265 0.1729210 0.1981040 0.2794485
```

```r
NotLargeCeilCompFind<- setEvidence(NotLargeCeilFind, 
                                   nodes=c("Comp"),
                                   states=c("Comp.")
                                   )


getEvidence(NotLargeCeilCompFind)
```

```
## Finding: 
## Ceil: [0,15k), [15k,100k), [100k,1m), [1m,30m)
## Comp: Comp.
## Pr(Finding)= 0.8013511
```

```r
querygrain(NotLargeCeilCompFind,"Offr")
```

```
## $Offr
## Offr
##         1         2       3-4        5+ 
## 0.1963060 0.2134839 0.2445513 0.3456588
```

```r
output<-data.frame(rbind(
    c(Control="LargeCeil-All",unlist(querygrain(LargeCeilFind,"Offr"))),
    c(Control="!LargeCeil-All",unlist(querygrain(NotLargeCeilFind,"Offr"))),
    c(Control="LargeCeil-Comp",unlist(querygrain(LargeCeilCompFind,"Offr"))),
    c(Control="!LargeCeil-Comp",unlist(querygrain(NotLargeCeilCompFind,"Offr")))
    ))
output$Offr.1<-as.numeric(as.character(output$Offr.1))
output$Offr.2<-as.numeric(as.character(output$Offr.2))
output$Offr.3.4<-as.numeric(as.character(output$Offr.3.4))
output$Offr.5.<-as.numeric(as.character(output$Offr.5.))
kable(output,digits=4)
```



Control            Offr.1   Offr.2   Offr.3.4   Offr.5.
----------------  -------  -------  ---------  --------
LargeCeil-All      0.3551   0.1405     0.1761    0.3283
!LargeCeil-All     0.3495   0.1729     0.1981    0.2794
LargeCeil-Comp     0.1566   0.1836     0.2279    0.4319
!LargeCeil-Comp    0.1963   0.2135     0.2446    0.3457

##Contracts initially expected to have smaller values will receive more offers.
*	For overall DoD contracting in 2013, contracts with an annual value of less than $250,000 saw the highest rate of competition with two or more offers (62 percent), and that rate generally declines as size of contract increases. (CSIS Analysis of FPDS Data). This phenomenon is reversed with services contracts which have less competition when the size is smaller.
*	Dependent Variables:
*	Single offer competition.
*	Initial Taxonomy:
*	The independent variable will be the base and all exercised options amount for the contract. This value will not be annualized, as the steady rate of exercising options means that the initial value has severe limitations when predicting total contract value.

For small contracts, the pattern 

###Initial results
The hypothesis does hold weakly for contracts under $15k, although larger contracts have more 5+ offer. The hypothesis does not hold for contracts under $100k and again 5+ offer contracts are remain more common among larger contracts.  If the dividing line is instead $1 million, then the analysis does hold, with contracts of $1m and lower having more competition at every level of analysis.



```r
unlist(compGin$universe$levels["Ceil"])
```

```
##        Ceil1        Ceil2        Ceil3        Ceil4        Ceil5 
##    "[0,15k)" "[15k,100k)"  "[100k,1m)"   "[1m,30m)"     "[30m+]"
```

```r
SmallCeilFind<- setEvidence(compGin, 
                            nodes=c("Ceil"),
                            states=c(list(c("[0,15k)","[15k,100k)")))
                            )

SmallCeilCompFind<- setEvidence(SmallCeilFind, 
                                nodes=c("Comp"),
                                states=c("Comp.")
                                )


getEvidence(SmallCeilFind)
```

```
## Finding: 
## Ceil: [0,15k), [15k,100k)
## Pr(Finding)= 0.880919
```

```r
querygrain(SmallCeilFind,"Offr")
```

```
## $Offr
## Offr
##         1         2       3-4        5+ 
## 0.3428907 0.1734741 0.2002586 0.2833767
```

```r
getEvidence(SmallCeilCompFind)
```

```
## Finding: 
## Ceil: [0,15k), [15k,100k)
## Comp: Comp.
## Pr(Finding)= 0.709844
```

```r
querygrain(SmallCeilCompFind,"Offr")
```

```
## $Offr
## Offr
##         1         2       3-4        5+ 
## 0.1918834 0.2132486 0.2460865 0.3487815
```

```r
NotSmallCeilFind<- setEvidence(compGin, 
                               nodes=c("Ceil"),
                               states=c(list(
                                   unlist(compGin$universe$levels["Ceil"])[!unlist(compGin$universe$levels["Ceil"]) %in%
                                                                               c("[0,15k)","[15k,100k)")]))
                               )

getEvidence(NotSmallCeilFind)
```

```
## Finding: 
## Ceil: [100k,1m), [1m,30m), [30m+]
## Pr(Finding)= 0.119081
```

```r
querygrain(NotSmallCeilFind,"Offr")
```

```
## $Offr
## Offr
##         1         2       3-4        5+ 
## 0.3986593 0.1685766 0.1819940 0.2507701
```

```r
NotSmallCeilCompFind<- setEvidence(NotSmallCeilFind, 
                                   nodes=c("Comp"),
                                   states=c("Comp.")
                                   )


getEvidence(NotSmallCeilCompFind)
```

```
## Finding: 
## Ceil: [100k,1m), [1m,30m), [30m+]
## Comp: Comp.
## Pr(Finding)= 0.09220538
```

```r
querygrain(NotSmallCeilCompFind,"Offr")
```

```
## $Offr
## Offr
##         1         2       3-4        5+ 
## 0.2300526 0.2150690 0.2326066 0.3222719
```

```r
rm(SmallCeilFind,NotSmallCeilFind)
rm(SmallCeilCompFind,NotSmallCeilCompFind)


VerySmallCeilFind<- setEvidence(compGin, 
                                nodes=c("Ceil"),
                                states=c("[0,15k)")
                                )

VerySmallCeilCompFind<- setEvidence(VerySmallCeilFind, 
                                    nodes=c("Comp"),
                                    states=c("Comp.")
                                    )


getEvidence(VerySmallCeilFind)
```

```
## Finding: 
## Ceil: [0,15k)
## Pr(Finding)= 0.5742986
```

```r
querygrain(VerySmallCeilFind,"Offr")
```

```
## $Offr
## Offr
##         1         2       3-4        5+ 
## 0.3377189 0.1818385 0.2087960 0.2716467
```

```r
getEvidence(VerySmallCeilCompFind)
```

```
## Finding: 
## Ceil: [0,15k)
## Comp: Comp.
## Pr(Finding)= 0.4641208
```

```r
querygrain(VerySmallCeilCompFind,"Offr")
```

```
## $Offr
## Offr
##         1         2       3-4        5+ 
## 0.1885442 0.2229925 0.2557982 0.3326651
```

```r
NotVerySmallCeilFind<- setEvidence(compGin, 
                                   nodes=c("Ceil"),
                                   states=c(list(
                                       unlist(compGin$universe$levels["Ceil"])[unlist(compGin$universe$levels["Ceil"])!=c("[0,15k)")]))
                                   )

getEvidence(NotVerySmallCeilFind)
```

```
## Finding: 
## Ceil: [15k,100k), [100k,1m), [1m,30m), [30m+]
## Pr(Finding)= 0.4257014
```

```r
querygrain(NotVerySmallCeilFind,"Offr")
```

```
## $Offr
## Offr
##         1         2       3-4        5+ 
## 0.3654679 0.1608199 0.1836319 0.2900803
```

```r
NotVerySmallCeilCompFind<- setEvidence(NotVerySmallCeilFind, 
                                       nodes=c("Comp"),
                                       states=c("Comp.")
                                       )


getEvidence(NotVerySmallCeilCompFind)
```

```
## Finding: 
## Ceil: [15k,100k), [100k,1m), [1m,30m), [30m+]
## Comp: Comp.
## Pr(Finding)= 0.3379286
```

```r
querygrain(NotVerySmallCeilCompFind,"Offr")
```

```
## $Offr
## Offr
##         1         2       3-4        5+ 
## 0.2068842 0.2003628 0.2290702 0.3636829
```

```r
rm(NotVerySmallCeilFind,VerySmallCeilFind)
rm(NotVerySmallCeilCompFind,VerySmallCeilCompFind)



#1M

Under1mCeilFind<- setEvidence(compGin, 
                              nodes=c("Ceil"),
                              states=c(list(c("[0,15k)","[15k,100k)","[100k,1m)")))
                              )


getEvidence(Under1mCeilFind)
```

```
## Finding: 
## Ceil: [0,15k), [15k,100k), [100k,1m)
## Pr(Finding)= 0.9795632
```

```r
NotUnder1mCeilFind<- setEvidence(compGin, 
                                 nodes=c("Ceil"),
                                 states=c(list(
                                     unlist(compGin$universe$levels["Ceil"])[!unlist(compGin$universe$levels["Ceil"]) %in%
                                                                                 c("[0,15k)","[15k,100k)","[100k,1m)")]))
                                 )

getEvidence(NotUnder1mCeilFind)
```

```
## Finding: 
## Ceil: [1m,30m), [30m+]
## Pr(Finding)= 0.02043682
```

```r
output<-data.frame(rbind(
    c(Control="Under1mCeil",Comp="All",unlist(querygrain(Under1mCeilFind,"Offr"))),
    c(Control="!Under1mCeil",Comp="All",unlist(querygrain(NotUnder1mCeilFind,"Offr")))
    )
    )
output$Control<-as.character(output$Control)
output$Comp<-as.character(output$Comp)
output$Offr.1<-as.numeric(as.character(output$Offr.1))
output$Offr.2<-as.numeric(as.character(output$Offr.2))
output$Offr.3.4<-as.numeric(as.character(output$Offr.3.4))
output$Offr.5.<-as.numeric(as.character(output$Offr.5.))

output<-rbind(output,
              c(Control="Under1mCeil",Comp="Comp",unlist(querygrain(setEvidence(Under1mCeilFind, 
                                                                                 nodes=c("Comp"),
                                                                                 states=c("Comp.")
                                                                                 ),"Offr"))),
              c(Control="!Under1mCeil",Comp="Comp",unlist(querygrain(setEvidence(NotUnder1mCeilFind, 
                                                                                 nodes=c("Comp"),
                                                                                 states=c("Comp.")
                                                                                 ),"Offr")))
              )

output$Offr.1<-as.numeric(as.character(output$Offr.1))
output$Offr.2<-as.numeric(as.character(output$Offr.2))
output$Offr.3.4<-as.numeric(as.character(output$Offr.3.4))
output$Offr.5.<-as.numeric(as.character(output$Offr.5.))

kable(output,digits=4)
```



Control        Comp    Offr.1   Offr.2   Offr.3.4   Offr.5.
-------------  -----  -------  -------  ---------  --------
Under1mCeil    All     0.3479   0.1734     0.1985    0.2801
!Under1mCeil   All     0.4264   0.1465     0.1763    0.2508
Under1mCeil    Comp    0.1956   0.2138     0.2447    0.3459
!Under1mCeil   Comp    0.2333   0.1954     0.2347    0.3366

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
unlist(compGin$universe$levels["IDV"])
```

```
##      IDV1      IDV2 
## "Def/Pur"     "IDV"
```

```r
IDVFind<- setEvidence(compGin, 
                      nodes=c("IDV"),
                      states=c("IDV")
                      )

IDVCompFind<- setEvidence(IDVFind, 
                          nodes=c("Comp"),
                          states=c("Comp.")
                          )


getEvidence(IDVFind)
```

```
## Finding: 
## IDV: IDV
## Pr(Finding)= 0.6461451
```

```r
querygrain(IDVFind,"Offr")
```

```
## $Offr
## Offr
##         1         2       3-4        5+ 
## 0.2859609 0.1938452 0.2047210 0.3154729
```

```r
getEvidence(IDVCompFind)
```

```
## Finding: 
##  IDV: IDV
## Comp: Comp.
## Pr(Finding)= 0.5373753
```

```r
querygrain(IDVCompFind,"Offr")
```

```
## $Offr
## Offr
##         1         2       3-4        5+ 
## 0.1458193 0.2314938 0.2443179 0.3783691
```

```r
NotIDVFind<- setEvidence(compGin, 
                         nodes=c("IDV"),
                         states=c(list(
                             unlist(compGin$universe$levels["IDV"])[unlist(compGin$universe$levels["IDV"]!=
                                                                               "IDV")]))
                         )

getEvidence(NotIDVFind)
```

```
## Finding: 
## IDV: Def/Pur, IDV
## Pr(Finding)= 1
```

```r
querygrain(NotIDVFind,"Offr")
```

```
## $Offr
## Offr
##         1         2       3-4        5+ 
## 0.3495317 0.1728909 0.1980836 0.2794939
```

```r
NotIDVCompFind<- setEvidence(NotIDVFind, 
                             nodes=c("Comp"),
                             states=c("Comp.")
                             )


getEvidence(NotIDVCompFind)
```

```
## Finding: 
##  IDV: Def/Pur, IDV
## Comp: Comp.
## Pr(Finding)= 0.8020494
```

```r
querygrain(NotIDVCompFind,"Offr")
```

```
## $Offr
## Offr
##         1         2       3-4        5+ 
## 0.1962714 0.2134579 0.2445368 0.3457339
```

```r
output<-data.frame(rbind(
    c(Control="IDV-All",unlist(querygrain(IDVFind,"Offr"))),
    c(Control="!IDV-All",unlist(querygrain(NotIDVFind,"Offr"))),
    c(Control="IDV-Comp",unlist(querygrain(IDVCompFind,"Offr"))),
    c(Control="!IDV-Comp",unlist(querygrain(NotIDVCompFind,"Offr")))
    ))
output$Offr.1<-as.numeric(as.character(output$Offr.1))
output$Offr.2<-as.numeric(as.character(output$Offr.2))
output$Offr.3.4<-as.numeric(as.character(output$Offr.3.4))
output$Offr.5.<-as.numeric(as.character(output$Offr.5.))
kable(output,digits=4)
```



Control      Offr.1   Offr.2   Offr.3.4   Offr.5.
----------  -------  -------  ---------  --------
IDV-All      0.2860   0.1938     0.2047    0.3155
!IDV-All     0.3495   0.1729     0.1981    0.2795
IDV-Comp     0.1458   0.2315     0.2443    0.3784
!IDV-Comp    0.1963   0.2135     0.2445    0.3457

##Contracts with longer initially expected periods of performance will encounter more problems.
*	Past CSIS research on cost overruns in major defense acquisition projects have found that cost growth is correlated with contract duration. "If cost increases accrue over time, then programs with an older baseline estimate would tend to accumulate relatively higher cost increases. The data for the analyzed programs show that older programs indeed experience larger overruns... this growth correlation not only provides further evidence for the assertion that cost growth occurs steadily throughout the program lifespan, but it also suggests that younger programs are not performing better than older programs." (David Berteau et al.: Cost and Time Overruns for Major Defense Acquisition Programs (2011))
*	Dependent Variables:
*	Contract termination, number of change orders, and cost of change orders.
*	Initial Taxonomy:
*	Period of performance will be measured as expected contract duration in 4-week months, rounding up.
*	Note: CSIS is including a separate hypothesis (Slide 9) to test whether fixed-price contracts encounter an even greater number of problems under contracts with longer expected periods of performance 



```r
unlist(compGin$universe$levels["Dur"])
```

```
##            Dur1            Dur2            Dur3            Dur4 
## "[    0,   61)" "[   61,  214)" "[  214,  366)" "[  366,33192]"
```

```r
LongDurFind<- setEvidence(compGin, 
                          nodes=c("Dur"),
                          states=c("[  366,33192]")
                          )

getEvidence(LongDurFind)
```

```
## Finding: 
## Dur: [  366,33192]
## Pr(Finding)= 0.05594229
```

```r
querygrain( LongDurFind,"Offr")
```

```
## $Offr
## Offr
##         1         2       3-4        5+ 
## 0.5236394 0.1464318 0.1439375 0.1859913
```

```r
NotLongDurFind<- setEvidence(compGin, 
                             nodes=c("Dur"),
                             states=c(list(
                                 unlist(compGin$universe$levels["Dur"])[unlist(compGin$universe$levels["Dur"]!=
                                                                                   "[  366,33192]")]))
                             )

getEvidence(NotLongDurFind)
```

```
## Finding: 
## Dur: [    0,   61), [   61,  214), [  214,  366), [  366,33192]
## Pr(Finding)= 1
```

```r
querygrain(NotLongDurFind,"Offr")
```

```
## $Offr
## Offr
##         1         2       3-4        5+ 
## 0.3495317 0.1728909 0.1980836 0.2794939
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
unlist(compGin$universe$levels["PSR"])
```

      PSR1       PSR2       PSR3 
"Products"      "R&D" "Services" 

```r
RNodes=c("PSR")
RStates=c("Products")
RVariable=c("Products")

output<-data.frame(rbind(
    c(Variable=RVariable,Control="None",Comp="All",
      pFinding=attributes(getEvidence(setEvidence(compGin, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(compGin, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="None",Comp="Comp",
      pFinding=attributes(getEvidence(setEvidence(compGin, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
      unlist(querygrain(setEvidence(compGin, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    ),"Offr")))
    )
    )


output$Control<-as.character(output$Control)
output$Comp<-as.character(output$Comp)
output$pFinding<-as.character(output$pFinding)
output$Offr.1<-as.numeric(as.character(output$Offr.1))
output$Offr.2<-as.numeric(as.character(output$Offr.2))
output$Offr.3.4<-as.numeric(as.character(output$Offr.3.4))
output$Offr.5.<-as.numeric(as.character(output$Offr.5.))


output<-rbind(output,
    c(Variable=RVariable,Control="Aircraft",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(AircraftFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(AircraftFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="!Aircraft",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(NotAircraftFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,   
      unlist(querygrain(setEvidence(NotAircraftFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr")))
    )
    

output<-rbind(output,
              c(Variable=RVariable,Control="Aircraft",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(AircraftFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(AircraftFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr"))),
              c(Variable=RVariable,Control="!Aircraft",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(NotAircraftFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(NotAircraftFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr")))
              )



output<-rbind(output,
    c(Variable=RVariable,Control="LargeCeil",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(LargeCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(LargeCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="!LargeCeil",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(NotLargeCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,   
      unlist(querygrain(setEvidence(NotLargeCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr")))
    )

output<-rbind(output,
              c(Variable=RVariable,Control="LargeCeil",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(LargeCeilFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(LargeCeilFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr"))),
              c(Variable=RVariable,Control="!LargeCeil",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(NotLargeCeilFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(NotLargeCeilFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr")))
              )

output<-rbind(output,
    c(Variable=RVariable,Control="Under1mCeil",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(Under1mCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(Under1mCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="!Under1mCeil",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(NotUnder1mCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,   
      unlist(querygrain(setEvidence(NotUnder1mCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr")))
    )


output<-rbind(output,
              c(Variable=RVariable,Control="Under1mCeil",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(Under1mCeilFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(Under1mCeilFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr"))),
              c(Variable=RVariable,Control="!Under1mCeil",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(NotUnder1mCeilFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(NotUnder1mCeilFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr")))
              )




output<-rbind(output,
    c(Variable=RVariable,Control="IDV",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(IDVFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(IDVFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="!IDV",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(NotIDVFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,   
      unlist(querygrain(setEvidence(NotIDVFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr")))
    )


output<-rbind(output,
              c(Variable=RVariable,Control="IDV",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(IDVFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(IDVFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr"))),
              c(Variable=RVariable,Control="!IDV",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(NotIDVFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(NotIDVFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr")))
              )


output<-rbind(output,
    c(Variable=RVariable,Control="LongDur",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(LongDurFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(LongDurFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="!LongDur",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(NotLongDurFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,   
      unlist(querygrain(setEvidence(NotLongDurFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr")))
    )


output<-rbind(output,
              c(Variable=RVariable,Control="LongDur",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(LongDurFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(LongDurFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr"))),
              c(Variable=RVariable,Control="!LongDur",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(NotLongDurFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(NotLongDurFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr")))
              )


output$pFinding<-as.numeric(as.character(output$pFinding))
output$Offr.1<-as.numeric(as.character(output$Offr.1))
output$Offr.2<-as.numeric(as.character(output$Offr.2))
output$Offr.3.4<-as.numeric(as.character(output$Offr.3.4))
output$Offr.5.<-as.numeric(as.character(output$Offr.5.))

kable(output,digits=4)
```



Variable   Control        Comp    pFinding   Offr.1   Offr.2   Offr.3.4   Offr.5.
---------  -------------  -----  ---------  -------  -------  ---------  --------
Products   None           All       0.8091   0.3259   0.1766     0.2026    0.2950
Products   None           Comp      0.6634   0.1845   0.2136     0.2449    0.3570
Products   Aircraft       All       0.0933   0.5323   0.1849     0.1292    0.1536
Products   !Aircraft      All       0.7158   0.2989   0.1755     0.2122    0.3134
Products   Aircraft       Comp      0.0569   0.2516   0.2981     0.2063    0.2440
Products   !Aircraft      Comp      0.6065   0.1783   0.2056     0.2485    0.3676
Products   LargeCeil      All       0.0003   0.3664   0.1637     0.1864    0.2836
Products   !LargeCeil     All       0.8087   0.3258   0.1766     0.2026    0.2950
Products   LargeCeil      Comp      0.0002   0.1801   0.2117     0.2406    0.3676
Products   !LargeCeil     Comp      0.6632   0.1845   0.2136     0.2449    0.3570
Products   Under1mCeil    All       0.8017   0.3250   0.1767     0.2028    0.2955
Products   !Under1mCeil   All       0.0074   0.4156   0.1633     0.1808    0.2403
Products   Under1mCeil    Comp      0.6578   0.1840   0.2136     0.2450    0.3574
Products   !Under1mCeil   Comp      0.0057   0.2480   0.2095     0.2323    0.3102
Products   IDV            All       0.5136   0.2586   0.1987     0.2084    0.3343
Products   !IDV           All       0.8091   0.3259   0.1766     0.2026    0.2950
Products   IDV            Comp      0.4359   0.1297   0.2330     0.2441    0.3931
Products   !IDV           Comp      0.6634   0.1845   0.2136     0.2449    0.3570
Products   LongDur        All       0.0267   0.5802   0.1441     0.1255    0.1502
Products   !LongDur       All       0.8091   0.3259   0.1766     0.2026    0.2950
Products   LongDur        Comp      0.0147   0.2539   0.2564     0.2218    0.2678
Products   !LongDur       Comp      0.6634   0.1845   0.2136     0.2449    0.3570


```r
unlist(compGin$universe$levels["PSR"])
```

      PSR1       PSR2       PSR3 
"Products"      "R&D" "Services" 

```r
RNodes=c("PSR")
RStates=c("Services")
RVariable=c("Services")

output<-data.frame(rbind(
    c(Variable=RVariable,Control="None",Comp="All",
      pFinding=attributes(getEvidence(setEvidence(compGin, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(compGin, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="None",Comp="Comp",
      pFinding=attributes(getEvidence(setEvidence(compGin, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
      unlist(querygrain(setEvidence(compGin, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    ),"Offr")))
    )
    )


output$Control<-as.character(output$Control)
output$Comp<-as.character(output$Comp)
output$pFinding<-as.character(output$pFinding)
output$Offr.1<-as.numeric(as.character(output$Offr.1))
output$Offr.2<-as.numeric(as.character(output$Offr.2))
output$Offr.3.4<-as.numeric(as.character(output$Offr.3.4))
output$Offr.5.<-as.numeric(as.character(output$Offr.5.))


output<-rbind(output,
    c(Variable=RVariable,Control="Aircraft",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(AircraftFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(AircraftFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="!Aircraft",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(NotAircraftFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,   
      unlist(querygrain(setEvidence(NotAircraftFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr")))
    )
    

output<-rbind(output,
              c(Variable=RVariable,Control="Aircraft",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(AircraftFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(AircraftFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr"))),
              c(Variable=RVariable,Control="!Aircraft",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(NotAircraftFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(NotAircraftFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr")))
              )



output<-rbind(output,
    c(Variable=RVariable,Control="LargeCeil",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(LargeCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(LargeCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="!LargeCeil",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(NotLargeCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,   
      unlist(querygrain(setEvidence(NotLargeCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr")))
    )

output<-rbind(output,
              c(Variable=RVariable,Control="LargeCeil",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(LargeCeilFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(LargeCeilFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr"))),
              c(Variable=RVariable,Control="!LargeCeil",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(NotLargeCeilFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(NotLargeCeilFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr")))
              )

output<-rbind(output,
    c(Variable=RVariable,Control="Under1mCeil",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(Under1mCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(Under1mCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="!Under1mCeil",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(NotUnder1mCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,   
      unlist(querygrain(setEvidence(NotUnder1mCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr")))
    )


output<-rbind(output,
              c(Variable=RVariable,Control="Under1mCeil",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(Under1mCeilFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(Under1mCeilFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr"))),
              c(Variable=RVariable,Control="!Under1mCeil",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(NotUnder1mCeilFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(NotUnder1mCeilFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr")))
              )




output<-rbind(output,
    c(Variable=RVariable,Control="IDV",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(IDVFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(IDVFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="!IDV",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(NotIDVFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,   
      unlist(querygrain(setEvidence(NotIDVFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr")))
    )


output<-rbind(output,
              c(Variable=RVariable,Control="IDV",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(IDVFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(IDVFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr"))),
              c(Variable=RVariable,Control="!IDV",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(NotIDVFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(NotIDVFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr")))
              )


output<-rbind(output,
    c(Variable=RVariable,Control="LongDur",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(LongDurFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(LongDurFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="!LongDur",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(NotLongDurFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,   
      unlist(querygrain(setEvidence(NotLongDurFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr")))
    )


output<-rbind(output,
              c(Variable=RVariable,Control="LongDur",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(LongDurFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(LongDurFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr"))),
              c(Variable=RVariable,Control="!LongDur",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(NotLongDurFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(NotLongDurFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr")))
              )


output$pFinding<-as.numeric(as.character(output$pFinding))
output$Offr.1<-as.numeric(as.character(output$Offr.1))
output$Offr.2<-as.numeric(as.character(output$Offr.2))
output$Offr.3.4<-as.numeric(as.character(output$Offr.3.4))
output$Offr.5.<-as.numeric(as.character(output$Offr.5.))

kable(output,digits=4)
```



Variable   Control        Comp    pFinding   Offr.1   Offr.2   Offr.3.4   Offr.5.
---------  -------------  -----  ---------  -------  -------  ---------  --------
Services   None           All       0.1819   0.4487   0.1571     0.1812    0.2129
Services   None           Comp      0.1315   0.2478   0.2136     0.2470    0.2916
Services   Aircraft       All       0.0027   0.6415   0.1552     0.1464    0.0568
Services   !Aircraft      All       0.1792   0.4458   0.1572     0.1818    0.2153
Services   Aircraft       Comp      0.0014   0.2904   0.3080     0.2908    0.1108
Services   !Aircraft      Comp      0.1301   0.2474   0.2126     0.2466    0.2934
Services   LargeCeil      All       0.0006   0.3433   0.1238     0.1693    0.3637
Services   !LargeCeil     All       0.1814   0.4490   0.1572     0.1813    0.2125
Services   LargeCeil      Comp      0.0004   0.1330   0.1631     0.2196    0.4843
Services   !LargeCeil     Comp      0.1311   0.2482   0.2138     0.2471    0.2910
Services   Under1mCeil    All       0.1703   0.4504   0.1587     0.1814    0.2095
Services   !Under1mCeil   All       0.0116   0.4242   0.1341     0.1791    0.2626
Services   Under1mCeil    Comp      0.1231   0.2505   0.2156     0.2472    0.2867
Services   !Under1mCeil   Comp      0.0084   0.2081   0.1839     0.2450    0.3631
Services   IDV            All       0.1285   0.3879   0.1754     0.1918    0.2449
Services   !IDV           All       0.1819   0.4487   0.1571     0.1812    0.2129
Services   IDV            Comp      0.0986   0.2125   0.2246     0.2461    0.3168
Services   !IDV           Comp      0.1315   0.2478   0.2136     0.2470    0.2916
Services   LongDur        All       0.0263   0.4722   0.1466     0.1648    0.2163
Services   !LongDur       All       0.1819   0.4487   0.1571     0.1812    0.2129
Services   LongDur        Comp      0.0184   0.2548   0.2063     0.2321    0.3069
Services   !LongDur       Comp      0.1315   0.2478   0.2136     0.2470    0.2916



```r
unlist(compGin$universe$levels["PSR"])
```

      PSR1       PSR2       PSR3 
"Products"      "R&D" "Services" 

```r
RNodes=c("PSR")
RStates=c("R&D")
RVariable=c("R&D")

output<-data.frame(rbind(
    c(Variable=RVariable,Control="None",Comp="All",
      pFinding=attributes(getEvidence(setEvidence(compGin, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(compGin, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="None",Comp="Comp",
      pFinding=attributes(getEvidence(setEvidence(compGin, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
      unlist(querygrain(setEvidence(compGin, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    ),"Offr")))
    )
    )


output$Control<-as.character(output$Control)
output$Comp<-as.character(output$Comp)
output$pFinding<-as.character(output$pFinding)
output$Offr.1<-as.numeric(as.character(output$Offr.1))
output$Offr.2<-as.numeric(as.character(output$Offr.2))
output$Offr.3.4<-as.numeric(as.character(output$Offr.3.4))
output$Offr.5.<-as.numeric(as.character(output$Offr.5.))


output<-rbind(output,
    c(Variable=RVariable,Control="Aircraft",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(AircraftFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(AircraftFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="!Aircraft",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(NotAircraftFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,   
      unlist(querygrain(setEvidence(NotAircraftFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr")))
    )
    

output<-rbind(output,
              c(Variable=RVariable,Control="Aircraft",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(AircraftFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(AircraftFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr"))),
              c(Variable=RVariable,Control="!Aircraft",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(NotAircraftFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(NotAircraftFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr")))
              )



output<-rbind(output,
    c(Variable=RVariable,Control="LargeCeil",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(LargeCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(LargeCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="!LargeCeil",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(NotLargeCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,   
      unlist(querygrain(setEvidence(NotLargeCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr")))
    )

output<-rbind(output,
              c(Variable=RVariable,Control="LargeCeil",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(LargeCeilFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(LargeCeilFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr"))),
              c(Variable=RVariable,Control="!LargeCeil",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(NotLargeCeilFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(NotLargeCeilFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr")))
              )

output<-rbind(output,
    c(Variable=RVariable,Control="Under1mCeil",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(Under1mCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(Under1mCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="!Under1mCeil",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(NotUnder1mCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,   
      unlist(querygrain(setEvidence(NotUnder1mCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr")))
    )


output<-rbind(output,
              c(Variable=RVariable,Control="Under1mCeil",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(Under1mCeilFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(Under1mCeilFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr"))),
              c(Variable=RVariable,Control="!Under1mCeil",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(NotUnder1mCeilFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(NotUnder1mCeilFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr")))
              )




output<-rbind(output,
    c(Variable=RVariable,Control="IDV",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(IDVFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(IDVFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="!IDV",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(NotIDVFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,   
      unlist(querygrain(setEvidence(NotIDVFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr")))
    )


output<-rbind(output,
              c(Variable=RVariable,Control="IDV",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(IDVFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(IDVFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr"))),
              c(Variable=RVariable,Control="!IDV",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(NotIDVFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(NotIDVFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr")))
              )


output<-rbind(output,
    c(Variable=RVariable,Control="LongDur",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(LongDurFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(LongDurFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="!LongDur",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(NotLongDurFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,   
      unlist(querygrain(setEvidence(NotLongDurFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr")))
    )


output<-rbind(output,
              c(Variable=RVariable,Control="LongDur",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(LongDurFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(LongDurFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr"))),
              c(Variable=RVariable,Control="!LongDur",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(NotLongDurFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(NotLongDurFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr")))
              )


output$pFinding<-as.numeric(as.character(output$pFinding))
output$Offr.1<-as.numeric(as.character(output$Offr.1))
output$Offr.2<-as.numeric(as.character(output$Offr.2))
output$Offr.3.4<-as.numeric(as.character(output$Offr.3.4))
output$Offr.5.<-as.numeric(as.character(output$Offr.5.))

kable(output,digits=4)
```



Variable   Control        Comp    pFinding   Offr.1   Offr.2   Offr.3.4   Offr.5.
---------  -------------  -----  ---------  -------  -------  ---------  --------
R&D        None           All       0.0090   0.4735   0.1605     0.1316    0.2344
R&D        None           Comp      0.0071   0.3372   0.2018     0.1653    0.2957
R&D        Aircraft       All       0.0006   0.5387   0.1767     0.1414    0.1432
R&D        !Aircraft      All       0.0084   0.4690   0.1594     0.1309    0.2407
R&D        Aircraft       Comp      0.0004   0.3318   0.2561     0.2046    0.2075
R&D        !Aircraft      Comp      0.0067   0.3375   0.1986     0.1629    0.3010
R&D        LargeCeil      All       0.0001   0.4078   0.1727     0.1844    0.2351
R&D        !LargeCeil     All       0.0090   0.4739   0.1604     0.1313    0.2344
R&D        LargeCeil      Comp      0.0000   0.2467   0.2190     0.2352    0.2992
R&D        !LargeCeil     Comp      0.0071   0.3377   0.2017     0.1648    0.2957
R&D        Under1mCeil    All       0.0076   0.4686   0.1603     0.1318    0.2393
R&D        !Under1mCeil   All       0.0014   0.4991   0.1617     0.1304    0.2089
R&D        Under1mCeil    Comp      0.0060   0.3350   0.2003     0.1646    0.3000
R&D        !Under1mCeil   Comp      0.0011   0.3489   0.2100     0.1688    0.2723
R&D        IDV            All       0.0041   0.5139   0.1655     0.1470    0.1736
R&D        !IDV           All       0.0090   0.4735   0.1605     0.1316    0.2344
R&D        IDV            Comp      0.0028   0.3041   0.2366     0.2099    0.2493
R&D        !IDV           Comp      0.0071   0.3372   0.2018     0.1653    0.2957
R&D        LongDur        All       0.0030   0.4717   0.1659     0.1242    0.2382
R&D        !LongDur       All       0.0090   0.4735   0.1605     0.1316    0.2344
R&D        LongDur        Comp      0.0023   0.3336   0.2090     0.1562    0.3012
R&D        !LongDur       Comp      0.0071   0.3372   0.2018     0.1653    0.2957





```r
unlist(compGin$universe$levels["What"])
```

                           What1                            What2 
           "Aircraft and Drones" "Electronics and Communications" 
                           What3                            What4 
   "Facilities and Construction"                  "Land Vehicles" 
                           What5                            What6 
     "Missile and Space Systems"                          "Other" 
                           What7                            What8 
            "Ships & Submarines"         "Weapons and Ammunition" 

```r
RNodes=c("What")
RStates=c("Aircraft and Drones")
RVariable=c("Air")

output<-data.frame(rbind(
    c(Variable=RVariable,Control="None",Comp="All",
      pFinding=attributes(getEvidence(setEvidence(compGin, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(compGin, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="None",Comp="Comp",
      pFinding=attributes(getEvidence(setEvidence(compGin, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
      unlist(querygrain(setEvidence(compGin, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    ),"Offr")))
    )
    )


output$Control<-as.character(output$Control)
output$Comp<-as.character(output$Comp)
output$pFinding<-as.character(output$pFinding)
output$Offr.1<-as.numeric(as.character(output$Offr.1))
output$Offr.2<-as.numeric(as.character(output$Offr.2))
output$Offr.3.4<-as.numeric(as.character(output$Offr.3.4))
output$Offr.5.<-as.numeric(as.character(output$Offr.5.))


output<-rbind(output,
    c(Variable=RVariable,Control="LargeCeil",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(LargeCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(LargeCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="!LargeCeil",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(NotLargeCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,   
      unlist(querygrain(setEvidence(NotLargeCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr")))
    )

output<-rbind(output,
              c(Variable=RVariable,Control="LargeCeil",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(LargeCeilFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(LargeCeilFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr"))),
              c(Variable=RVariable,Control="!LargeCeil",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(NotLargeCeilFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(NotLargeCeilFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr")))
              )

output<-rbind(output,
    c(Variable=RVariable,Control="Under1mCeil",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(Under1mCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(Under1mCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="!Under1mCeil",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(NotUnder1mCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,   
      unlist(querygrain(setEvidence(NotUnder1mCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr")))
    )


output<-rbind(output,
              c(Variable=RVariable,Control="Under1mCeil",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(Under1mCeilFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(Under1mCeilFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr"))),
              c(Variable=RVariable,Control="!Under1mCeil",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(NotUnder1mCeilFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(NotUnder1mCeilFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr")))
              )




output<-rbind(output,
    c(Variable=RVariable,Control="IDV",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(IDVFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(IDVFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="!IDV",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(NotIDVFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,   
      unlist(querygrain(setEvidence(NotIDVFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr")))
    )


output<-rbind(output,
              c(Variable=RVariable,Control="IDV",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(IDVFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(IDVFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr"))),
              c(Variable=RVariable,Control="!IDV",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(NotIDVFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(NotIDVFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr")))
              )


output<-rbind(output,
    c(Variable=RVariable,Control="LongDur",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(LongDurFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(LongDurFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="!LongDur",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(NotLongDurFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,   
      unlist(querygrain(setEvidence(NotLongDurFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr")))
    )


output<-rbind(output,
              c(Variable=RVariable,Control="LongDur",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(LongDurFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(LongDurFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr"))),
              c(Variable=RVariable,Control="!LongDur",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(NotLongDurFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(NotLongDurFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr")))
              )

output$pFinding<-as.numeric(as.character(output$pFinding))
output$Offr.1<-as.numeric(as.character(output$Offr.1))
output$Offr.2<-as.numeric(as.character(output$Offr.2))
output$Offr.3.4<-as.numeric(as.character(output$Offr.3.4))
output$Offr.5.<-as.numeric(as.character(output$Offr.5.))

kable(output,digits=4)
```



Variable   Control        Comp    pFinding   Offr.1   Offr.2   Offr.3.4   Offr.5.
---------  -------------  -----  ---------  -------  -------  ---------  --------
Air        None           All       0.0966   0.5355   0.1840     0.1297    0.1508
Air        None           Comp      0.0587   0.2530   0.2981     0.2083    0.2406
Air        LargeCeil      All       0.0001   0.5832   0.2018     0.1200    0.0950
Air        !LargeCeil     All       0.0965   0.5354   0.1840     0.1297    0.1509
Air        LargeCeil      Comp      0.0001   0.2039   0.3861     0.2290    0.1810
Air        !LargeCeil     Comp      0.0586   0.2531   0.2980     0.2083    0.2407
Air        Under1mCeil    All       0.0948   0.5334   0.1845     0.1296    0.1525
Air        !Under1mCeil   All       0.0018   0.6430   0.1581     0.1332    0.0657
Air        Under1mCeil    Comp      0.0577   0.2519   0.2981     0.2075    0.2425
Air        !Under1mCeil   Comp      0.0010   0.3228   0.2989     0.2533    0.1250
Air        IDV            All       0.0497   0.6246   0.2387     0.0951    0.0415
Air        !IDV           All       0.0966   0.5355   0.1840     0.1297    0.1508
Air        IDV            Comp      0.0243   0.2359   0.4863     0.1933    0.0844
Air        !IDV           Comp      0.0587   0.2530   0.2981     0.2083    0.2406
Air        LongDur        All       0.0110   0.6576   0.1554     0.0901    0.0968
Air        !LongDur       All       0.0966   0.5355   0.1840     0.1297    0.1508
Air        LongDur        Comp      0.0052   0.2847   0.3259     0.1879    0.2015
Air        !LongDur       Comp      0.0587   0.2530   0.2981     0.2083    0.2406



```r
unlist(compGin$universe$levels["What"])
```

                           What1                            What2 
           "Aircraft and Drones" "Electronics and Communications" 
                           What3                            What4 
   "Facilities and Construction"                  "Land Vehicles" 
                           What5                            What6 
     "Missile and Space Systems"                          "Other" 
                           What7                            What8 
            "Ships & Submarines"         "Weapons and Ammunition" 

```r
RNodes=c("What")
RStates=c("Missile and Space Systems")
RVariable=c("MnS")

output<-data.frame(rbind(
    c(Variable=RVariable,Control="None",Comp="All",
      pFinding=attributes(getEvidence(setEvidence(compGin, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(compGin, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="None",Comp="Comp",
      pFinding=attributes(getEvidence(setEvidence(compGin, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
      unlist(querygrain(setEvidence(compGin, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    ),"Offr")))
    )
    )


output$Control<-as.character(output$Control)
output$Comp<-as.character(output$Comp)
output$pFinding<-as.character(output$pFinding)
output$Offr.1<-as.numeric(as.character(output$Offr.1))
output$Offr.2<-as.numeric(as.character(output$Offr.2))
output$Offr.3.4<-as.numeric(as.character(output$Offr.3.4))
output$Offr.5.<-as.numeric(as.character(output$Offr.5.))


output<-rbind(output,
    c(Variable=RVariable,Control="LargeCeil",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(LargeCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(LargeCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="!LargeCeil",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(NotLargeCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,   
      unlist(querygrain(setEvidence(NotLargeCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr")))
    )

output<-rbind(output,
              c(Variable=RVariable,Control="LargeCeil",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(LargeCeilFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(LargeCeilFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr"))),
              c(Variable=RVariable,Control="!LargeCeil",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(NotLargeCeilFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(NotLargeCeilFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr")))
              )

output<-rbind(output,
    c(Variable=RVariable,Control="Under1mCeil",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(Under1mCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(Under1mCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="!Under1mCeil",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(NotUnder1mCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,   
      unlist(querygrain(setEvidence(NotUnder1mCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr")))
    )


output<-rbind(output,
              c(Variable=RVariable,Control="Under1mCeil",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(Under1mCeilFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(Under1mCeilFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr"))),
              c(Variable=RVariable,Control="!Under1mCeil",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(NotUnder1mCeilFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(NotUnder1mCeilFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr")))
              )




output<-rbind(output,
    c(Variable=RVariable,Control="IDV",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(IDVFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(IDVFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="!IDV",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(NotIDVFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,   
      unlist(querygrain(setEvidence(NotIDVFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr")))
    )


output<-rbind(output,
              c(Variable=RVariable,Control="IDV",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(IDVFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(IDVFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr"))),
              c(Variable=RVariable,Control="!IDV",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(NotIDVFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(NotIDVFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr")))
              )


output<-rbind(output,
    c(Variable=RVariable,Control="LongDur",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(LongDurFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(LongDurFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="!LongDur",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(NotLongDurFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,   
      unlist(querygrain(setEvidence(NotLongDurFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr")))
    )


output<-rbind(output,
              c(Variable=RVariable,Control="LongDur",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(LongDurFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(LongDurFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr"))),
              c(Variable=RVariable,Control="!LongDur",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(NotLongDurFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(NotLongDurFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr")))
              )

output$pFinding<-as.numeric(as.character(output$pFinding))
output$Offr.1<-as.numeric(as.character(output$Offr.1))
output$Offr.2<-as.numeric(as.character(output$Offr.2))
output$Offr.3.4<-as.numeric(as.character(output$Offr.3.4))
output$Offr.5.<-as.numeric(as.character(output$Offr.5.))

kable(output,digits=4)
```



Variable   Control        Comp    pFinding   Offr.1   Offr.2   Offr.3.4   Offr.5.
---------  -------------  -----  ---------  -------  -------  ---------  --------
MnS        None           All       0.0033   0.5137   0.1511     0.1284    0.2067
MnS        None           Comp      0.0022   0.2859   0.2221     0.1861    0.3058
MnS        LargeCeil      All       0.0000   0.5974   0.1117     0.1767    0.1142
MnS        !LargeCeil     All       0.0033   0.5131   0.1514     0.1281    0.2074
MnS        LargeCeil      Comp      0.0000   0.4120   0.1583     0.2615    0.1682
MnS        !LargeCeil     Comp      0.0022   0.2849   0.2226     0.1856    0.3069
MnS        Under1mCeil    All       0.0030   0.5079   0.1502     0.1259    0.2160
MnS        !Under1mCeil   All       0.0003   0.5682   0.1594     0.1523    0.1201
MnS        Under1mCeil    Comp      0.0020   0.2757   0.2213     0.1825    0.3205
MnS        !Under1mCeil   Comp      0.0002   0.3786   0.2295     0.2194    0.1725
MnS        IDV            All       0.0014   0.5243   0.1904     0.1620    0.1234
MnS        !IDV           All       0.0033   0.5137   0.1511     0.1284    0.2067
MnS        IDV            Comp      0.0009   0.3039   0.2803     0.2329    0.1830
MnS        !IDV           Comp      0.0022   0.2859   0.2221     0.1861    0.3058
MnS        LongDur        All       0.0007   0.4573   0.1846     0.1279    0.2302
MnS        !LongDur       All       0.0033   0.5137   0.1511     0.1284    0.2067
MnS        LongDur        Comp      0.0005   0.2842   0.2437     0.1673    0.3048
MnS        !LongDur       Comp      0.0022   0.2859   0.2221     0.1861    0.3058




```r
unlist(compGin$universe$levels["What"])
```

                           What1                            What2 
           "Aircraft and Drones" "Electronics and Communications" 
                           What3                            What4 
   "Facilities and Construction"                  "Land Vehicles" 
                           What5                            What6 
     "Missile and Space Systems"                          "Other" 
                           What7                            What8 
            "Ships & Submarines"         "Weapons and Ammunition" 

```r
RNodes=c("What")
RStates=c("Electronics and Communications")
RVariable=c("EnC")

output<-data.frame(rbind(
    c(Variable=RVariable,Control="None",Comp="All",
      pFinding=attributes(getEvidence(setEvidence(compGin, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(compGin, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="None",Comp="Comp",
      pFinding=attributes(getEvidence(setEvidence(compGin, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
      unlist(querygrain(setEvidence(compGin, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    ),"Offr")))
    )
    )


output$Control<-as.character(output$Control)
output$Comp<-as.character(output$Comp)
output$pFinding<-as.character(output$pFinding)
output$Offr.1<-as.numeric(as.character(output$Offr.1))
output$Offr.2<-as.numeric(as.character(output$Offr.2))
output$Offr.3.4<-as.numeric(as.character(output$Offr.3.4))
output$Offr.5.<-as.numeric(as.character(output$Offr.5.))


output<-rbind(output,
    c(Variable=RVariable,Control="LargeCeil",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(LargeCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(LargeCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="!LargeCeil",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(NotLargeCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,   
      unlist(querygrain(setEvidence(NotLargeCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr")))
    )

output<-rbind(output,
              c(Variable=RVariable,Control="LargeCeil",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(LargeCeilFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(LargeCeilFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr"))),
              c(Variable=RVariable,Control="!LargeCeil",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(NotLargeCeilFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(NotLargeCeilFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr")))
              )

output<-rbind(output,
    c(Variable=RVariable,Control="Under1mCeil",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(Under1mCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(Under1mCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="!Under1mCeil",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(NotUnder1mCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,   
      unlist(querygrain(setEvidence(NotUnder1mCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr")))
    )


output<-rbind(output,
              c(Variable=RVariable,Control="Under1mCeil",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(Under1mCeilFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(Under1mCeilFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr"))),
              c(Variable=RVariable,Control="!Under1mCeil",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(NotUnder1mCeilFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(NotUnder1mCeilFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr")))
              )




output<-rbind(output,
    c(Variable=RVariable,Control="IDV",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(IDVFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(IDVFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="!IDV",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(NotIDVFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,   
      unlist(querygrain(setEvidence(NotIDVFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr")))
    )


output<-rbind(output,
              c(Variable=RVariable,Control="IDV",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(IDVFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(IDVFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr"))),
              c(Variable=RVariable,Control="!IDV",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(NotIDVFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(NotIDVFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr")))
              )


output<-rbind(output,
    c(Variable=RVariable,Control="LongDur",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(LongDurFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(LongDurFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="!LongDur",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(NotLongDurFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,   
      unlist(querygrain(setEvidence(NotLongDurFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr")))
    )


output<-rbind(output,
              c(Variable=RVariable,Control="LongDur",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(LongDurFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(LongDurFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr"))),
              c(Variable=RVariable,Control="!LongDur",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(NotLongDurFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(NotLongDurFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr")))
              )

output$pFinding<-as.numeric(as.character(output$pFinding))
output$Offr.1<-as.numeric(as.character(output$Offr.1))
output$Offr.2<-as.numeric(as.character(output$Offr.2))
output$Offr.3.4<-as.numeric(as.character(output$Offr.3.4))
output$Offr.5.<-as.numeric(as.character(output$Offr.5.))

kable(output,digits=4)
```



Variable   Control        Comp    pFinding   Offr.1   Offr.2   Offr.3.4   Offr.5.
---------  -------------  -----  ---------  -------  -------  ---------  --------
EnC        None           All       0.1530   0.4497   0.1664     0.1886    0.1953
EnC        None           Comp      0.1157   0.2807   0.2175     0.2458    0.2560
EnC        LargeCeil      All       0.0003   0.4519   0.1226     0.2365    0.1889
EnC        !LargeCeil     All       0.1527   0.4497   0.1665     0.1885    0.1953
EnC        LargeCeil      Comp      0.0002   0.2605   0.1638     0.3202    0.2555
EnC        !LargeCeil     Comp      0.1155   0.2807   0.2176     0.2457    0.2560
EnC        Under1mCeil    All       0.1485   0.4488   0.1666     0.1894    0.1952
EnC        !Under1mCeil   All       0.0045   0.4786   0.1590     0.1623    0.2002
EnC        Under1mCeil    Comp      0.1124   0.2801   0.2176     0.2466    0.2557
EnC        !Under1mCeil   Comp      0.0033   0.3016   0.2123     0.2169    0.2693
EnC        IDV            All       0.0808   0.4591   0.1897     0.1806    0.1707
EnC        !IDV           All       0.1530   0.4497   0.1664     0.1886    0.1953
EnC        IDV            Comp      0.0613   0.2967   0.2470     0.2333    0.2231
EnC        !IDV           Comp      0.1157   0.2807   0.2175     0.2458    0.2560
EnC        LongDur        All       0.0150   0.5428   0.1448     0.1499    0.1625
EnC        !LongDur       All       0.1530   0.4497   0.1664     0.1886    0.1953
EnC        LongDur        Comp      0.0095   0.2920   0.2243     0.2305    0.2532
EnC        !LongDur       Comp      0.1157   0.2807   0.2175     0.2458    0.2560



```r
unlist(compGin$universe$levels["What"])
```

                           What1                            What2 
           "Aircraft and Drones" "Electronics and Communications" 
                           What3                            What4 
   "Facilities and Construction"                  "Land Vehicles" 
                           What5                            What6 
     "Missile and Space Systems"                          "Other" 
                           What7                            What8 
            "Ships & Submarines"         "Weapons and Ammunition" 

```r
RNodes=c("What")
RStates=c("Ships & Submarines")
RVariable=c("Vessel")

output<-data.frame(rbind(
    c(Variable=RVariable,Control="None",Comp="All",
      pFinding=attributes(getEvidence(setEvidence(compGin, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(compGin, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="None",Comp="Comp",
      pFinding=attributes(getEvidence(setEvidence(compGin, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
      unlist(querygrain(setEvidence(compGin, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    ),"Offr")))
    )
    )


output$Control<-as.character(output$Control)
output$Comp<-as.character(output$Comp)
output$pFinding<-as.character(output$pFinding)
output$Offr.1<-as.numeric(as.character(output$Offr.1))
output$Offr.2<-as.numeric(as.character(output$Offr.2))
output$Offr.3.4<-as.numeric(as.character(output$Offr.3.4))
output$Offr.5.<-as.numeric(as.character(output$Offr.5.))


output<-rbind(output,
    c(Variable=RVariable,Control="LargeCeil",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(LargeCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(LargeCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="!LargeCeil",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(NotLargeCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,   
      unlist(querygrain(setEvidence(NotLargeCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr")))
    )

output<-rbind(output,
              c(Variable=RVariable,Control="LargeCeil",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(LargeCeilFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(LargeCeilFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr"))),
              c(Variable=RVariable,Control="!LargeCeil",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(NotLargeCeilFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(NotLargeCeilFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr")))
              )

output<-rbind(output,
    c(Variable=RVariable,Control="Under1mCeil",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(Under1mCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(Under1mCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="!Under1mCeil",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(NotUnder1mCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,   
      unlist(querygrain(setEvidence(NotUnder1mCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr")))
    )


output<-rbind(output,
              c(Variable=RVariable,Control="Under1mCeil",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(Under1mCeilFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(Under1mCeilFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr"))),
              c(Variable=RVariable,Control="!Under1mCeil",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(NotUnder1mCeilFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(NotUnder1mCeilFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr")))
              )




output<-rbind(output,
    c(Variable=RVariable,Control="IDV",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(IDVFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(IDVFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="!IDV",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(NotIDVFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,   
      unlist(querygrain(setEvidence(NotIDVFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr")))
    )


output<-rbind(output,
              c(Variable=RVariable,Control="IDV",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(IDVFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(IDVFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr"))),
              c(Variable=RVariable,Control="!IDV",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(NotIDVFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(NotIDVFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr")))
              )


output<-rbind(output,
    c(Variable=RVariable,Control="LongDur",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(LongDurFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(LongDurFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="!LongDur",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(NotLongDurFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,   
      unlist(querygrain(setEvidence(NotLongDurFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr")))
    )


output<-rbind(output,
              c(Variable=RVariable,Control="LongDur",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(LongDurFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(LongDurFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr"))),
              c(Variable=RVariable,Control="!LongDur",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(NotLongDurFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(NotLongDurFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr")))
              )

output$pFinding<-as.numeric(as.character(output$pFinding))
output$Offr.1<-as.numeric(as.character(output$Offr.1))
output$Offr.2<-as.numeric(as.character(output$Offr.2))
output$Offr.3.4<-as.numeric(as.character(output$Offr.3.4))
output$Offr.5.<-as.numeric(as.character(output$Offr.5.))

kable(output,digits=4)
```



Variable   Control        Comp    pFinding   Offr.1   Offr.2   Offr.3.4   Offr.5.
---------  -------------  -----  ---------  -------  -------  ---------  --------
Vessel     None           All       0.0420   0.5404   0.1487     0.1544    0.1565
Vessel     None           Comp      0.0280   0.3200   0.2195     0.2287    0.2319
Vessel     LargeCeil      All       0.0000   0.3091   0.2057     0.1848    0.3003
Vessel     !LargeCeil     All       0.0420   0.5405   0.1487     0.1544    0.1564
Vessel     LargeCeil      Comp      0.0000   0.1599   0.2460     0.2170    0.3772
Vessel     !LargeCeil     Comp      0.0280   0.3200   0.2194     0.2287    0.2319
Vessel     Under1mCeil    All       0.0415   0.5412   0.1489     0.1543    0.1555
Vessel     !Under1mCeil   All       0.0005   0.4704   0.1268     0.1587    0.2441
Vessel     Under1mCeil    Comp      0.0277   0.3208   0.2199     0.2287    0.2305
Vessel     !Under1mCeil   Comp      0.0003   0.2483   0.1788     0.2251    0.3479
Vessel     IDV            All       0.0160   0.4812   0.1688     0.1517    0.1982
Vessel     !IDV           All       0.0420   0.5404   0.1487     0.1544    0.1565
Vessel     IDV            Comp      0.0113   0.2750   0.2342     0.2118    0.2790
Vessel     !IDV           Comp      0.0280   0.3200   0.2195     0.2287    0.2319
Vessel     LongDur        All       0.0017   0.7366   0.0837     0.0840    0.0958
Vessel     !LongDur       All       0.0420   0.5404   0.1487     0.1544    0.1565
Vessel     LongDur        Comp      0.0006   0.2991   0.2149     0.2233    0.2627
Vessel     !LongDur       Comp      0.0280   0.3200   0.2195     0.2287    0.2319




```r
unlist(compGin$universe$levels["What"])
```

                           What1                            What2 
           "Aircraft and Drones" "Electronics and Communications" 
                           What3                            What4 
   "Facilities and Construction"                  "Land Vehicles" 
                           What5                            What6 
     "Missile and Space Systems"                          "Other" 
                           What7                            What8 
            "Ships & Submarines"         "Weapons and Ammunition" 

```r
RNodes=c("What")
RStates=c("Weapons and Ammunition")
RVariable=c("WnA")

output<-data.frame(rbind(
    c(Variable=RVariable,Control="None",Comp="All",
      pFinding=attributes(getEvidence(setEvidence(compGin, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(compGin, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="None",Comp="Comp",
      pFinding=attributes(getEvidence(setEvidence(compGin, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
      unlist(querygrain(setEvidence(compGin, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    ),"Offr")))
    )
    )


output$Control<-as.character(output$Control)
output$Comp<-as.character(output$Comp)
output$pFinding<-as.character(output$pFinding)
output$Offr.1<-as.numeric(as.character(output$Offr.1))
output$Offr.2<-as.numeric(as.character(output$Offr.2))
output$Offr.3.4<-as.numeric(as.character(output$Offr.3.4))
output$Offr.5.<-as.numeric(as.character(output$Offr.5.))


output<-rbind(output,
    c(Variable=RVariable,Control="LargeCeil",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(LargeCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(LargeCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="!LargeCeil",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(NotLargeCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,   
      unlist(querygrain(setEvidence(NotLargeCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr")))
    )

output<-rbind(output,
              c(Variable=RVariable,Control="LargeCeil",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(LargeCeilFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(LargeCeilFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr"))),
              c(Variable=RVariable,Control="!LargeCeil",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(NotLargeCeilFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(NotLargeCeilFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr")))
              )

output<-rbind(output,
    c(Variable=RVariable,Control="Under1mCeil",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(Under1mCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(Under1mCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="!Under1mCeil",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(NotUnder1mCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,   
      unlist(querygrain(setEvidence(NotUnder1mCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr")))
    )


output<-rbind(output,
              c(Variable=RVariable,Control="Under1mCeil",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(Under1mCeilFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(Under1mCeilFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr"))),
              c(Variable=RVariable,Control="!Under1mCeil",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(NotUnder1mCeilFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(NotUnder1mCeilFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr")))
              )




output<-rbind(output,
    c(Variable=RVariable,Control="IDV",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(IDVFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(IDVFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="!IDV",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(NotIDVFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,   
      unlist(querygrain(setEvidence(NotIDVFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr")))
    )


output<-rbind(output,
              c(Variable=RVariable,Control="IDV",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(IDVFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(IDVFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr"))),
              c(Variable=RVariable,Control="!IDV",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(NotIDVFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(NotIDVFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr")))
              )


output<-rbind(output,
    c(Variable=RVariable,Control="LongDur",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(LongDurFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(LongDurFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="!LongDur",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(NotLongDurFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,   
      unlist(querygrain(setEvidence(NotLongDurFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr")))
    )


output<-rbind(output,
              c(Variable=RVariable,Control="LongDur",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(LongDurFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(LongDurFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr"))),
              c(Variable=RVariable,Control="!LongDur",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(NotLongDurFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(NotLongDurFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr")))
              )

output$pFinding<-as.numeric(as.character(output$pFinding))
output$Offr.1<-as.numeric(as.character(output$Offr.1))
output$Offr.2<-as.numeric(as.character(output$Offr.2))
output$Offr.3.4<-as.numeric(as.character(output$Offr.3.4))
output$Offr.5.<-as.numeric(as.character(output$Offr.5.))

kable(output,digits=4)
```



Variable   Control        Comp    pFinding   Offr.1   Offr.2   Offr.3.4   Offr.5.
---------  -------------  -----  ---------  -------  -------  ---------  --------
WnA        None           All       0.0089   0.4343   0.1734     0.1567    0.2356
WnA        None           Comp      0.0067   0.2658   0.2262     0.2018    0.3062
WnA        LargeCeil      All       0.0000   0.4163   0.2757     0.1247    0.1832
WnA        !LargeCeil     All       0.0089   0.4343   0.1731     0.1568    0.2358
WnA        LargeCeil      Comp      0.0000   0.2251   0.3728     0.1598    0.2423
WnA        !LargeCeil     Comp      0.0067   0.2659   0.2258     0.2019    0.3064
WnA        Under1mCeil    All       0.0085   0.4332   0.1703     0.1552    0.2413
WnA        !Under1mCeil   All       0.0005   0.4529   0.2284     0.1831    0.1356
WnA        Under1mCeil    Comp      0.0064   0.2668   0.2215     0.1993    0.3125
WnA        !Under1mCeil   Comp      0.0003   0.2483   0.3143     0.2501    0.1873
WnA        IDV            All       0.0036   0.4674   0.2147     0.1493    0.1687
WnA        !IDV           All       0.0089   0.4343   0.1734     0.1567    0.2356
WnA        IDV            Comp      0.0025   0.2460   0.3065     0.2080    0.2396
WnA        !IDV           Comp      0.0067   0.2658   0.2262     0.2018    0.3062
WnA        LongDur        All       0.0008   0.4781   0.1845     0.1479    0.1895
WnA        !LongDur       All       0.0089   0.4343   0.1734     0.1567    0.2356
WnA        LongDur        Comp      0.0006   0.2786   0.2557     0.2028    0.2629
WnA        !LongDur       Comp      0.0067   0.2658   0.2262     0.2018    0.3062




```r
unlist(compGin$universe$levels["IDV"])
```

     IDV1      IDV2 
"Def/Pur"     "IDV" 

```r
RNodes=c("IDV")
RStates=c("IDV")
RVariable=c("IDV")

output<-data.frame(rbind(
    c(Variable=RVariable,Control="None",Comp="All",
      pFinding=attributes(getEvidence(setEvidence(compGin, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(compGin, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="None",Comp="Comp",
      pFinding=attributes(getEvidence(setEvidence(compGin, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
      unlist(querygrain(setEvidence(compGin, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    ),"Offr")))
    )
    )


output$Control<-as.character(output$Control)
output$Comp<-as.character(output$Comp)
output$pFinding<-as.character(output$pFinding)
output$Offr.1<-as.numeric(as.character(output$Offr.1))
output$Offr.2<-as.numeric(as.character(output$Offr.2))
output$Offr.3.4<-as.numeric(as.character(output$Offr.3.4))
output$Offr.5.<-as.numeric(as.character(output$Offr.5.))


output<-rbind(output,
    c(Variable=RVariable,Control="Aircraft",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(AircraftFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(AircraftFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="!Aircraft",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(NotAircraftFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,   
      unlist(querygrain(setEvidence(NotAircraftFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr")))
    )
    

output<-rbind(output,
              c(Variable=RVariable,Control="Aircraft",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(AircraftFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(AircraftFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr"))),
              c(Variable=RVariable,Control="!Aircraft",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(NotAircraftFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(NotAircraftFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr")))
              )



output<-rbind(output,
    c(Variable=RVariable,Control="LargeCeil",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(LargeCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(LargeCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="!LargeCeil",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(NotLargeCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,   
      unlist(querygrain(setEvidence(NotLargeCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr")))
    )

output<-rbind(output,
              c(Variable=RVariable,Control="LargeCeil",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(LargeCeilFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(LargeCeilFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr"))),
              c(Variable=RVariable,Control="!LargeCeil",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(NotLargeCeilFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(NotLargeCeilFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr")))
              )

output<-rbind(output,
    c(Variable=RVariable,Control="Under1mCeil",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(Under1mCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(Under1mCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="!Under1mCeil",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(NotUnder1mCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,   
      unlist(querygrain(setEvidence(NotUnder1mCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr")))
    )


output<-rbind(output,
              c(Variable=RVariable,Control="Under1mCeil",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(Under1mCeilFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(Under1mCeilFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr"))),
              c(Variable=RVariable,Control="!Under1mCeil",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(NotUnder1mCeilFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(NotUnder1mCeilFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr")))
              )




output<-rbind(output,
    c(Variable=RVariable,Control="IDV",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(IDVFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(IDVFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="!IDV",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(NotIDVFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,   
      unlist(querygrain(setEvidence(NotIDVFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr")))
    )


output<-rbind(output,
              c(Variable=RVariable,Control="IDV",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(IDVFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(IDVFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr"))),
              c(Variable=RVariable,Control="!IDV",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(NotIDVFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(NotIDVFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr")))
              )


output<-rbind(output,
    c(Variable=RVariable,Control="LongDur",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(LongDurFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,
      unlist(querygrain(setEvidence(LongDurFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="!LongDur",Comp="All",
     pFinding=attributes(getEvidence(setEvidence(NotLongDurFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    )))$pFinding,   
      unlist(querygrain(setEvidence(NotLongDurFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr")))
    )


output<-rbind(output,
              c(Variable=RVariable,Control="LongDur",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(LongDurFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(LongDurFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr"))),
              c(Variable=RVariable,Control="!LongDur",Comp="Comp",
                      pFinding=attributes(getEvidence(setEvidence(NotLongDurFind, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    )))$pFinding,
                unlist(querygrain(setEvidence(NotLongDurFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr")))
              )


output$pFinding<-as.numeric(as.character(output$pFinding))
output$Offr.1<-as.numeric(as.character(output$Offr.1))
output$Offr.2<-as.numeric(as.character(output$Offr.2))
output$Offr.3.4<-as.numeric(as.character(output$Offr.3.4))
output$Offr.5.<-as.numeric(as.character(output$Offr.5.))

kable(output,digits=4)
```



Variable   Control        Comp    pFinding   Offr.1   Offr.2   Offr.3.4   Offr.5.
---------  -------------  -----  ---------  -------  -------  ---------  --------
IDV        None           All       0.6461   0.2860   0.1938     0.2047    0.3155
IDV        None           Comp      0.5374   0.1458   0.2315     0.2443    0.3784
IDV        Aircraft       All       0.0497   0.6246   0.2387     0.0951    0.0415
IDV        !Aircraft      All       0.5965   0.2578   0.1901     0.2138    0.3383
IDV        Aircraft       Comp      0.0243   0.2359   0.4863     0.1933    0.0844
IDV        !Aircraft      Comp      0.5130   0.1415   0.2194     0.2467    0.3923
IDV        LargeCeil      All       0.0007   0.3355   0.1427     0.1764    0.3454
IDV        !LargeCeil     All       0.6454   0.2859   0.1939     0.2048    0.3154
IDV        LargeCeil      Comp      0.0006   0.1439   0.1841     0.2245    0.4476
IDV        !LargeCeil     Comp      0.5368   0.1458   0.2315     0.2443    0.3783
IDV        Under1mCeil    All       0.6303   0.2832   0.1948     0.2051    0.3169
IDV        !Under1mCeil   All       0.0158   0.3963   0.1561     0.1883    0.2593
IDV        Under1mCeil    Comp      0.5255   0.1445   0.2321     0.2443    0.3792
IDV        !Under1mCeil   Comp      0.0119   0.2057   0.2048     0.2466    0.3428
IDV        IDV            All       0.6461   0.2860   0.1938     0.2047    0.3155
IDV        !IDV           All       1.0000   0.3495   0.1729     0.1981    0.2795
IDV        IDV            Comp      0.5374   0.1458   0.2315     0.2443    0.3784
IDV        !IDV           Comp      0.8020   0.1963   0.2135     0.2445    0.3457
IDV        LongDur        All       0.0365   0.5198   0.1591     0.1391    0.1821
IDV        !LongDur       All       0.6461   0.2860   0.1938     0.2047    0.3155
IDV        LongDur        Comp      0.0224   0.2281   0.2553     0.2219    0.2947
IDV        !LongDur       Comp      0.5374   0.1458   0.2315     0.2443    0.3784
