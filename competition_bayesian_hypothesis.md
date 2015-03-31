

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
## Loading required package: ggplot2
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

```
##       PSR1       PSR2       PSR3 
## "Products"      "R&D" "Services"
```

```r
RNodes=c("PSR")
RStates=c("Products")
RVariable=c("Products")

output<-data.frame(rbind(
    c(Variable=RVariable,Control="None",Comp="All",
      unlist(querygrain(setEvidence(compGin, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="None",Comp="Comp",
      unlist(querygrain(setEvidence(compGin, 
                                    nodes=c(RNodes,"Comp"),
                                    states=c(RStates, "Comp.")
                                    ),"Offr")))
    )
    )


output$Control<-as.character(output$Control)
output$Comp<-as.character(output$Comp)
output$Offr.1<-as.numeric(as.character(output$Offr.1))
output$Offr.2<-as.numeric(as.character(output$Offr.2))
output$Offr.3.4<-as.numeric(as.character(output$Offr.3.4))
output$Offr.5.<-as.numeric(as.character(output$Offr.5.))


output<-rbind(output,
    c(Variable=RVariable,Control="Aircraft",Comp="All",
      unlist(querygrain(setEvidence(AircraftFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="!Aircraft",Comp="All",
      unlist(querygrain(setEvidence(NotAircraftFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr")))
    )
    

output<-rbind(output,
              c(Variable=RVariable,Control="Aircraft",Comp="Comp",
                unlist(querygrain(setEvidence(AircraftFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr"))),
              c(Variable=RVariable,Control="!Aircraft",Comp="Comp",
                unlist(querygrain(setEvidence(NotAircraftFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr")))
              )


output<-rbind(output,
    c(Variable=RVariable,Control="LargeCeil",Comp="All",
      unlist(querygrain(setEvidence(LargeCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="!LargeCeil",Comp="All",
      unlist(querygrain(setEvidence(NotLargeCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr")))
    )
    

output<-rbind(output,
              c(Variable=RVariable,Control="LargeCeil",Comp="Comp",
                unlist(querygrain(setEvidence(LargeCeilFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr"))),
              c(Variable=RVariable,Control="!LargeCeil",Comp="Comp",
                unlist(querygrain(setEvidence(NotLargeCeilFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr")))
              )

output$Offr.1<-as.numeric(as.character(output$Offr.1))
output$Offr.2<-as.numeric(as.character(output$Offr.2))
output$Offr.3.4<-as.numeric(as.character(output$Offr.3.4))
output$Offr.5.<-as.numeric(as.character(output$Offr.5.))


output<-rbind(output,
    c(Variable=RVariable,Control="Under1mCeil",Comp="All",
      unlist(querygrain(setEvidence(Under1mCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="!Under1mCeil",Comp="All",
      unlist(querygrain(setEvidence(NotUnder1mCeilFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr")))
    )
    

output<-rbind(output,
              c(Variable=RVariable,Control="Under1mCeil",Comp="Comp",
                unlist(querygrain(setEvidence(Under1mCeilFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr"))),
              c(Variable=RVariable,Control="!Under1mCeil",Comp="Comp",
                unlist(querygrain(setEvidence(NotUnder1mCeilFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr")))
              )


output<-rbind(output,
    c(Variable=RVariable,Control="IDV",Comp="All",
      unlist(querygrain(setEvidence(IDVFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="!IDV",Comp="All",
      unlist(querygrain(setEvidence(NotIDVFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr")))
    )
    

output<-rbind(output,
              c(Variable=RVariable,Control="IDV",Comp="Comp",
                unlist(querygrain(setEvidence(IDVFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr"))),
              c(Variable=RVariable,Control="!IDV",Comp="Comp",
                unlist(querygrain(setEvidence(NotIDVFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr")))
              )


output<-rbind(output,
    c(Variable=RVariable,Control="LongDur",Comp="All",
      unlist(querygrain(setEvidence(LongDurFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr"))),
    c(Variable=RVariable,Control="!LongDur",Comp="All",
      unlist(querygrain(setEvidence(NotLongDurFind, 
                                    nodes=RNodes,
                                    states=RStates
                                    ),"Offr")))
    )
    

output<-rbind(output,
              c(Variable=RVariable,Control="LongDur",Comp="Comp",
                unlist(querygrain(setEvidence(LongDurFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr"))),
              c(Variable=RVariable,Control="!LongDur",Comp="Comp",
                unlist(querygrain(setEvidence(NotLongDurFind, 
                                              nodes=c(RNodes,"Comp"),
                                              states=c(RStates,"Comp.")
                                              ),"Offr")))
              )


kable(output,digits=4)
```



Variable   Control        Comp   Offr.1              Offr.2              Offr.3.4            Offr.5.           
---------  -------------  -----  ------------------  ------------------  ------------------  ------------------
Products   None           All    0.32585533604941    0.176572849775149   0.202611606399363   0.294960207776077 
Products   None           Comp   0.184540545483185   0.213556143992667   0.244894224716868   0.357009085807281 
Products   Aircraft       All    0.53234828828525    0.184886201740667   0.129154062733427   0.153611447240655 
Products   !Aircraft      All    0.298942413265389   0.175489342579858   0.212185575773036   0.313382668381717 
Products   Aircraft       Comp   0.25156710157878    0.298149438450373   0.20633270427346    0.243950755697387 
Products   !Aircraft      Comp   0.178253658977912   0.205621551014264   0.248511177707152   0.367613612300672 
Products   LargeCeil      All    0.366404055696933   0.163677001326768   0.186362133936612   0.283556809039687 
Products   !LargeCeil     All    0.325839209457484   0.176577978570516   0.202618068961282   0.294964743010718 
Products   LargeCeil      Comp   0.180129037447518   0.211706408077127   0.240555424897213   0.367609129578143 
Products   !LargeCeil     Comp   0.184542184154049   0.213556831083867   0.244895836379998   0.357005148382085 
Products   Under1mCeil    All    0.325032408418113   0.176695001116595   0.202811303109247   0.295461287356045 
Products   !Under1mCeil   All    0.415562441483184   0.163257167023505   0.180842725240148   0.240337666253163 
Products   Under1mCeil    Comp   0.18399298930086    0.213591538829511   0.245002526391944   0.357412945477685 
Products   !Under1mCeil   Comp   0.248002811637989   0.209453849782817   0.232341960765551   0.310201377813644 
Products   IDV            All    0.258640543508096   0.198679353777492   0.208416032472314   0.334264070242098 
Products   !IDV           All    0.32585533604941    0.176572849775149   0.202611606399363   0.294960207776077 
Products   IDV            Comp   0.129705279842264   0.233022424063301   0.244136653420048   0.393135642674386 
Products   !IDV           Comp   0.184540545483185   0.213556143992667   0.244894224716868   0.357009085807281 
Products   LongDur        All    0.580210242434576   0.144102103108725   0.125499415988157   0.150188238468542 
Products   !LongDur       All    0.32585533604941    0.176572849775149   0.202611606399363   0.294960207776077 
Products   LongDur        Comp   0.253936239215701   0.256393187033641   0.221821252969159   0.267849320781499 
Products   !LongDur       Comp   0.184540545483185   0.213556143992667   0.244894224716868   0.357009085807281 
