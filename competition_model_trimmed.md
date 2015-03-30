

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
```

Contracts are classified using a mix of numerical and categorical variables. While the changes in numerical variables are easy to grasp and summarize, a contract may have one line item that is competed and another that is not. As is detailed in the [exploration on R&D](RnD_1to5_exploration.md), we are only considering information available prior to contract start. The percentage of contract obligations that were competed is a valuable benchmark, but is highly influenced by factors that occured after contract start..

##Studying contract vehicle within the sample.
Describe contract vehicle here.


```r
setwd("K:\\Development\\Fixed-price\\")
filename<-"data\\defense_contract_CSIScontractID_model.csv"



ContractModel  <- read.csv(
    filename,
    header = TRUE, sep = ",", dec = ".", strip.white = TRUE, 
    na.strings = c("NULL","NA",""),
    stringsAsFactors = TRUE
    )
ContractModel<-subset(ContractModel,select=-c(SingleOffer))

nrow(ContractModel)
```

```
## [1] 5993688
```

```r
nrow(subset(ContractModel,complete.cases(ContractModel)))
```

```
## [1] 5908989
```

```r
summary(subset(ContractModel,complete.cases(ContractModel)))
```

```
##       IDV                             FxCb                Comp        
##  Def/Pur:2091106   Combination \nor Other:  55347   Comp.   :4739149  
##  IDV    :3817883   Cost-Based            : 203363   No Comp.:1169840  
##                    Fixed-Price           :5650279                     
##                                                                       
##                                                                       
##                                                                       
##                                                                       
##             Link                    Who         
##      0        :3636072   Air Force    : 632962  
##  [    1,  750): 822788   Army         :1516632  
##  [  750,67263]:1450129   Navy         :1364983  
##                          Other DoD    :2393716  
##                          Uncategorized:    696  
##                                                 
##                                                 
##                              What                        Intl         
##  Other                         :2644912   Any\nInternational: 665870  
##  Facilities and Construction   :1198540   Just U.S.         :5243119  
##  Electronics and Communications: 903876                               
##  Aircraft and Drones           : 570691                               
##  Land Vehicles                 : 269196                               
##  Ships & Submarines            : 248316                               
##  (Other)                       :  73458                               
##        PSR                           Ceil                    Dur         
##  Products:4780329   [0.00e+00,1.50e+04):3393141   [    0,   61):3671391  
##  R&D     :  53455   [1.00e+05,1.00e+06): 583140   [   61,  214):1461977  
##  Services:1075205   [1.00e+06,3.00e+07): 115419   [  214,  366): 445962  
##                     [1.50e+04,1.00e+05):1811788   [  366,33192]: 329659  
##                     [3.00e+07,3.36e+12]:   5501                          
##                                                                          
##                                                                          
##         Offr        
##    1      :2033248  
##    2      :1030469  
##  [  3,  5):1164669  
##  [  5,999]:1680603  
##                     
##                     
## 
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

nrow(ContractModel)
```

```
## [1] 5907181
```

The last stage before Bayesian learning is setting the whitelists and blacklists. The whitelist contains vectors that must existing in the graph while the blacklist contains forbidden vectors. There was some iteration in the development of the white and blacklist, as nonsensical connections were removed and the blacklist was made systematic.

I've listed them below based on the origin piece of evidence.
1. IDV (IsIDV)
* Whitelist to Comp (Competition) as many forms of IDVs don't qualify for full and open.
* Blacklist to PSR (SimpleArea) because what you are buying is picked before how you are buying it.
2. FxCb (FixedOrCost)
* Whitelist to One (SingleOffer) as this is the key study question.
* Blacklist to PSR (SimpleArea) and Intl (AnyInternational) because figuring out what and where come first.
* Blacklist to Ceil (qCeiling) and Dur (qDuration), because project scope should come before project pricing,  although this may not be entirely true when the vendors make proposals.
3. Comp (Competition)  
* Whitelist to One (SingleOffer). We'll see how strong this connection ends up being, but it's really straightforward.
* Blacklist to PSR (SimpleArea),  Intl (AnyInternational), and FxCb (FixedOrCost), as I think those details are set before deciding competition procedures.
4. Intl (AnyInternational) No restrictions or requirements.
5. PSR (SimpleArea)
* Blacklist to Intl (AnyInternational) As I think the place of performance in the U.S. or abroad (but not the vendor location) is sometimes decided at least as early as the  product or service.
6. Ceil (qCeiling)
* Blacklist to PSR (SimpleArea) Intl (AnyInternational) because what and where come before scope.
7. Dur (qDuration)
* Blacklist to PSR (SimpleArea) Intl (AnyInternational) because what and where come before scope.
8. One (SingleOffer) 
* Blacklist to everything not covered by an existing whitelist. While the vendors responses may influence some factors at the margin, this is the dependent variable for this part of the study.


```r
#White list, connections that must occur
ContractWL<-data.frame(from="Comp",to="Offr")
# ContractWL<-rbind(ContractWL,data.frame(from="Comp",to="Offr"))
ContractWL<-rbind(ContractWL,data.frame(from="PSR",to="FxCb"))
ContractWL<-rbind(ContractWL,data.frame(from="PSR",to="Link"))
ContractWL<-rbind(ContractWL,data.frame(from="FxCb",to="Offr"))
ContractWL<-rbind(ContractWL,data.frame(from="What",to="Link"))
# ContractWL<-rbind(ContractWL,data.frame(from="Who",to="Intl"))
ContractWL<-rbind(ContractWL,data.frame(from="Who",to="What"))
ContractWL<-rbind(ContractWL,data.frame(from="IDV",to="Ceil"))
ContractWL<-rbind(ContractWL,data.frame(from="Ceil",to="FxCb"))

bnWhitelist <- empty.graph(nodes = names(ContractModel))
arcs(bnWhitelist)<-ContractWL
graphviz.plot(bnWhitelist,main="Whitelisted Arcs Rd. 1")
```

![](competition_model_trimmed_files/figure-html/BayesianLearningSetup-1.png) 

```r
#Black list, connections that are prohibited
ContractBL<-data.frame(from=c("Offr"),to= c("IDV"))
ContractBL<-rbind(ContractBL,data.frame(from="Offr",to="Who"))
ContractBL<-rbind(ContractBL,data.frame(from="Offr",to="What"))
ContractBL<-rbind(ContractBL,data.frame(from="Offr",to="PSR"))
ContractBL<-rbind(ContractBL,data.frame(from="Offr",to="Intl"))
ContractBL<-rbind(ContractBL,data.frame(from="Offr",to="FxCb"))
ContractBL<-rbind(ContractBL,data.frame(from="Offr",to="Ceil"))
ContractBL<-rbind(ContractBL,data.frame(from="Offr",to="Dur"))
ContractBL<-rbind(ContractBL,data.frame(from="Offr",to="Link"))
# ContractBL<-rbind(ContractBL,data.frame(from="Who",to="Offr"))
ContractBL<-rbind(ContractBL,data.frame(from="Link",to="Who"))
ContractBL<-rbind(ContractBL,data.frame(from="Link",to="Intl"))
ContractBL<-rbind(ContractBL,data.frame(from="Link",to="IDV"))
ContractBL<-rbind(ContractBL,data.frame(from="FxCb",to="Who"))
ContractBL<-rbind(ContractBL,data.frame(from="FxCb",to="What"))
ContractBL<-rbind(ContractBL,data.frame(from="FxCb",to="Intl"))
ContractBL<-rbind(ContractBL,data.frame(from="FxCb",to="Ceil"))
ContractBL<-rbind(ContractBL,data.frame(from="FxCb",to="Dur"))
ContractBL<-rbind(ContractBL,data.frame(from="FxCb",to="Link"))
ContractBL<-rbind(ContractBL,data.frame(from="FxCb",to="Comp"))
ContractBL<-rbind(ContractBL,data.frame(from="Ceil",to="Who"))
ContractBL<-rbind(ContractBL,data.frame(from="Ceil",to="What"))
ContractBL<-rbind(ContractBL,data.frame(from="Ceil",to="PSR"))
ContractBL<-rbind(ContractBL,data.frame(from="Ceil",to="Intl"))
ContractBL<-rbind(ContractBL,data.frame(from="Ceil",to="Link"))
ContractBL<-rbind(ContractBL,data.frame(from="Dur",to="Who"))
ContractBL<-rbind(ContractBL,data.frame(from="Dur",to="What"))
ContractBL<-rbind(ContractBL,data.frame(from="Dur",to="PSR"))
ContractBL<-rbind(ContractBL,data.frame(from="Dur",to="Intl"))
ContractBL<-rbind(ContractBL,data.frame(from="Dur",to="Link"))
ContractBL<-rbind(ContractBL,data.frame(from="Comp",to="Who"))
ContractBL<-rbind(ContractBL,data.frame(from="Comp",to="What"))
ContractBL<-rbind(ContractBL,data.frame(from="Comp",to="PSR"))
ContractBL<-rbind(ContractBL,data.frame(from="Comp",to="Link"))
ContractBL<-rbind(ContractBL,data.frame(from="Comp",to="Intl"))


ContractBL<-rbind(ContractBL,data.frame(from="PSR",to="Who"))
ContractBL<-rbind(ContractBL,data.frame(from="PSR",to="What"))
ContractBL<-rbind(ContractBL,data.frame(from="PSR",to="Intl"))

ContractBL<-rbind(ContractBL,data.frame(from="IDV",to="Who"))
ContractBL<-rbind(ContractBL,data.frame(from="IDV",to="What"))
ContractBL<-rbind(ContractBL,data.frame(from="IDV",to="PSR"))
ContractBL<-rbind(ContractBL,data.frame(from="IDV",to="Intl"))
ContractBL<-rbind(ContractBL,data.frame(from="IDV",to="Comp"))


# ContractBL<-rbind(ContractBL,data.frame(from="Comp",to="Ceil"))
# ContractBL<-rbind(ContractBL,data.frame(from="Comp",to="Dur"))

ContractBL<-rbind(ContractBL,data.frame(from="IDV",to="Link"))


ContractBL<-rbind(ContractBL,data.frame(from="Who",to="Comp"))
ContractBL<-rbind(ContractBL,data.frame(from="Who",to="Offr"))
ContractBL<-rbind(ContractBL,data.frame(from="Who",to="IDV"))
ContractBL<-rbind(ContractBL,data.frame(from="Who",to="FxCb"))
ContractBL<-rbind(ContractBL,data.frame(from="Who",to="Ceil"))
ContractBL<-rbind(ContractBL,data.frame(from="Who",to="Dur"))


df <- data.frame(from=ContractBL$from, to=ContractBL$to, weight=rep(1,nrow(ContractBL)))
g1 <- graphBAM( data.frame(from=ContractBL$from, to=ContractBL$to, weight=rep(1,nrow(ContractBL))),
                edgemode = "directed")
adjacencyMatrix(g1)
```

```
##      Ceil Comp Dur FxCb IDV Intl Link Offr PSR What Who
## Ceil    0    0   0    0   0    1    1    0   1    1   1
## Comp    0    0   0    0   0    1    1    0   1    1   1
## Dur     0    0   0    0   0    1    1    0   1    1   1
## FxCb    1    1   1    0   0    1    1    0   0    1   1
## IDV     0    1   0    0   0    1    1    0   1    1   1
## Intl    0    0   0    0   0    0    0    0   0    0   0
## Link    0    0   0    0   1    1    0    0   0    0   1
## Offr    1    0   1    1   1    1    1    0   1    1   1
## PSR     0    0   0    0   0    1    0    0   0    1   1
## What    0    0   0    0   0    0    0    0   0    0   0
## Who     1    1   1    1   1    0    0    1   0    0   0
```

```r
plot(g1,main="Competition Bayesian Network Blacklist")
```

![](competition_model_trimmed_files/figure-html/BayesianLearningSetup-2.png) 

```r
edgeNames(g1)
```

```
##  [1] "Ceil~Intl" "Ceil~Link" "Ceil~PSR"  "Ceil~What" "Ceil~Who" 
##  [6] "Comp~Intl" "Comp~Link" "Comp~PSR"  "Comp~What" "Comp~Who" 
## [11] "Dur~Intl"  "Dur~Link"  "Dur~PSR"   "Dur~What"  "Dur~Who"  
## [16] "FxCb~Ceil" "FxCb~Comp" "FxCb~Dur"  "FxCb~Intl" "FxCb~Link"
## [21] "FxCb~What" "FxCb~Who"  "IDV~Comp"  "IDV~Intl"  "IDV~Link" 
## [26] "IDV~PSR"   "IDV~What"  "IDV~Who"   "Link~Intl" "Link~Who" 
## [31] "Offr~Ceil" "Offr~Dur"  "Offr~FxCb" "Offr~IDV"  "Offr~Intl"
## [36] "Offr~Link" "Offr~PSR"  "Offr~What" "Offr~Who"  "PSR~Intl" 
## [41] "PSR~What"  "PSR~Who"
```

```r
eAttrs<-list()
eAttrs$color<-c("Ceil~Intl"="red",
                "Ceil~Link"="red",
                "Ceil~PSR"="red",
                "Ceil~What"="red",
                "Ceil~Who"="purple",
                "Comp~Intl"="red",
                "Comp~Link"="red",
                "Comp~PSR"="red",
                "Comp~What"="red",
                "Comp~Who"="purple",
                "Dur~Intl"="red",
                "Dur~Link"="red",
                "Dur~PSR"="red",
                "Dur~What"="red",
                "Dur~Who"="purple",
                "FxCb~Ceil"="red",
                "FxCb~Comp"="red",
                "FxCb~Dur"="red",
                "FxCb~Intl"="red",
                "FxCb~Link"="red",
                "FxCb~What"="red",
                "FxCb~Who"="purple",
                "IDV~Intl"="red",
                "IDV~Link"="purple",
                "IDV~PSR"="red",
                "IDV~What"="red",
                "IDV~Who"="purple",
                "Link~Intl"="red",
                "Link~Who"="red",
                "Offr~Ceil"="red",
                "Offr~Dur"="red",
                "Offr~FxCb"="red",
                "Offr~IDV"="red",
                "Offr~Intl"="red",
                "Offr~Link"="red",
                "Offr~PSR"="red",
                "Offr~What"="red",
                "Offr~Who"="purple",
                "PSR~Intl"="red",
                "PSR~What"="red",
                "PSR~Who"="red"  )

plot(g1,
     main="Competition Bayesian Network Blacklist\nOne-way Blocks in Red, Two-way Blocks in Purple",
     edgeAttrs=eAttrs)
```

![](competition_model_trimmed_files/figure-html/BayesianLearningSetup-3.png) 

After the team developed the initial list and a small number of elements accidentally left off were corrected for, the next step was to do a preliminary run through with Bayesian learning. 



Noteably this initial run through produced two warnings of challenging relationships IDV to Dur and IDV to FxCb. After consulting within the team, the choice was made to add whitelisted relationships that went against those automatically produced.

IDV (IsIDV) new white lists:
* Whitelist to FxCb (FixedOrCost) because the pricing mechanism of most IDVs are set. Thus, once the choice of which IDV to use is made, the pricing mechanism would be set automatically. Furthermore, Fixed-price is often the mechanism because the IDV allows for multiple instances of a single contract reducing the need for the flexabilit Cost-Based contracts offer.
* Whitelist to Dur (Duration) because the use of the IDV mechanism typically means multiple shorter contractors under one IDV rather than a single longer contract. 

With those two connections in hand, all three Bayesian Learning algorith were applied.


```r
#White list, connections that must occur
# ContractWL<-rbind(ContractWL,data.frame(from="IDV",to="Dur"))
# ContractWL<-rbind(ContractWL,data.frame(from="IDV",to="FxCb"))

# ContractWL<-rbind(ContractWL,data.frame(from="Dur",to="Ceil"))
# ContractWL<-rbind(ContractWL,data.frame(from="Ceil",to="Comp"))
# ContractWL<-rbind(ContractWL,data.frame(from="Ceil",to="Dur"))


# 
# 
# highlightWL2 <- list(arcs = ContractWL, col = "blue")
# 
# highlightLists2 <- list(arcs = data.frame(from=c(as.character(ContractWL$from),
#                                                  as.character(ContractBL$to)),
#                                           to=c(as.character(ContractWL$to),
#                                                as.character(ContractBL$from))),
#                         col = "blue")
# 
# 
# 
# 
# iamb_dug<-iamb(ContractModel,blacklist=ContractBL,whitelist=ContractWL,optimized=TRUE, alpha=0.001)
# uiamb_dug<-iamb(ContractModel,blacklist=ContractBL,whitelist=ContractWL,optimized=FALSE, alpha=0.001)
# #Comparing the optimized and unoptimized Incremental Association results
# all.equal(iamb_dug,uiamb_dug)
# 
# interiamb_dug<-inter.iamb(ContractModel,blacklist=ContractBL,whitelist=ContractWL, alpha=0.001)
# uinteriamb_dug<-inter.iamb(ContractModel,blacklist=ContractBL,whitelist=ContractWL,optimized=FALSE, alpha=0.001)
# #They also dont match with interleaved.
# all.equal(interiamb_dug,uinteriamb_dug)
# all.equal(uiamb_dug,uinteriamb_dug)
# 
# mmpc_dug<-mmpc(ContractModel)
# ummpc_dug<-mmpc(ContractModel,blacklist=ContractBL,whitelist=ContractWL,optimized=FALSE)
# #They also dont match with interleaved.
# all.equal(mmpc_dug,ummpc_dug)
# 
# gs_dug<-gs(ContractModel,blacklist=ContractBL,whitelist=ContractWL,alpha=0.001)
# 
# #But the two methods do not producing matching results
# all.equal(iamb_dug,interiamb_dug)
# all.equal(uiamb_dug,uinteriamb_dug)
# all.equal(uiamb_dug,mmpc_dug)
# 
# 
# uinteriamb_dug05<-inter.iamb(ContractModel,blacklist=ContractBL,whitelist=ContractWL,optimized=FALSE,alpha=0.05)
# 
# hc_dug<- hc(ContractModel, score = "aic",blacklist=ContractBL,whitelist=ContractWL,optimized=FALSE)
# graphviz.plot(hc_dug,main="Hill Climbing Whitelist highlighted",highlight=highlightWL2)
# graphviz.plot(hc_dug,main="Hill Climbing Whitelist Interleaved highlighted  ",highlight=list(arcs=arcs(uinteriamb_dug)))
# graphviz.plot(uinteriamb_dug,main="Unoptimized Interleaved Hillclimbing highlighted",highlight=list(arcs=arcs(hc_dug)))
# 
# 
# graphviz.plot(uiamb_dug,main="Unoptimized Incremental Association ",highlight=highlightWL2)
# graphviz.plot(uinteriamb_dug,main="Unoptimized Interleaved Incremental Association ",highlight=highlightWL2)
# graphviz.plot(mmpc_dug,main="MMPC ",highlight=highlightWL2)
# 
# # graphviz.plot(uiamb_dug,main="Unoptimized Incremental Association ",highlight=arcs(uinteriamb_dug))
# 
# graphviz.plot(gs_dug,main="Grow-Shrink 2nd run",highlight=highlightWL2)
# 
# #Comparing the optimized and unoptimized Interleaved Incremental Association results
# 
# uinteriamb_dug00001<-inter.iamb(ContractModel,blacklist=ContractBL,whitelist=ContractWL,optimized=FALSE,alpha=0.000001)
# all.equal(uinteriamb_dug,uinteriamb_dug00001)
# 
# 
# modelstring(uinteriamb_dug00001)
# modelstring(uinteriamb_dug)
# modelstring(interiamb_dug)
# modelstring(uiamb_dug)
# modelstring(iamb_dug)
# modelstring(hc_dug)
# modelstring(gs_dug)
# 
# # [1] "[Who][What|Who][Intl|Who:What][PSR|Who:What:Intl][FxCb|PSR][Comp|What:Intl:PSR][Link|Who:What:Intl:PSR][IDV|Comp:What:Intl:PSR][Ceil|IDV:What:Intl:PSR][Dur|IDV:Comp:Link:What:Intl:PSR:Ceil][Offr|IDV:FxCb:Comp:Link:What:Intl:Ceil]"
# # > modelstring(uinteriamb_dug00001)
# # [1] "[Who][What|Who][Intl|Who:What][PSR|Who:What:Intl][FxCb|PSR][Comp|What:Intl:PSR][Link|Who:What:Intl:PSR][IDV|Comp:What:Intl:PSR][Ceil|IDV:What:Intl:PSR][Dur|IDV:Comp:Link:What:Intl:PSR:Ceil][Offr|IDV:FxCb:Comp:Link:What:Intl:Ceil]"
# # > modelstring(uinteriamb_dug)
# # [1] "[Who][What|Who][Intl|Who:What][PSR|Who:What:Intl][FxCb|PSR][Comp|What:Intl:PSR][Link|Who:What:Intl:PSR][IDV|Comp:What:Intl:PSR][Ceil|IDV:What:Intl:PSR][Dur|IDV:Comp:Link:What:Intl:PSR:Ceil][Offr|IDV:FxCb:Comp:Link:What:Intl:Ceil]"
# # > modelstring(interiamb_dug)
# # [1] "[Who][What|Who][Intl|Who:What][PSR|Who:What:Intl][FxCb|PSR][Comp|What:Intl:PSR][Link|Who:What:Intl:PSR][IDV|Comp:What:Intl:PSR][Ceil|IDV:What:Intl:PSR][Dur|IDV:Comp:Link:What:Intl:PSR:Ceil][Offr|IDV:FxCb:Comp:Link:What:Intl:Ceil:Dur]"
# # > modelstring(uiamb_dug)
# # [1] "[Who][What|Who][Intl|Who:What][PSR|Who:What:Intl][Link|Who:What:Intl:PSR][Comp|Link:What:Intl:PSR][IDV|Comp:What:PSR][Ceil|IDV:What:Intl:PSR][FxCb|PSR:Ceil][Dur|IDV:Comp:Link:What:Intl:PSR:Ceil][Offr|IDV:FxCb:Comp:What:Intl]"
# # > modelstring(iamb_dug)
# # [1] "[Who][What|Who][Intl|Who:What][PSR|Who:What:Intl][Link|Who:What:Intl:PSR][Comp|Link:What:Intl:PSR][IDV|Comp:What:PSR][Ceil|IDV:What:Intl:PSR][FxCb|PSR:Ceil][Dur|IDV:Comp:Link:What:Intl:PSR:Ceil][Offr|IDV:FxCb:Comp:What:Intl:Dur]"
# # > modelstring(hc_dug)
# # [1] "[Who][Intl|Who][What|Who:Intl][PSR|Who:What:Intl][Link|Who:What:Intl:PSR][Comp|Link:What:Intl:PSR][Dur|Comp:Link:What:Intl:PSR][IDV|Comp:What:Intl:PSR:Dur][Ceil|IDV:Comp:Link:What:Intl:PSR:Dur][FxCb|IDV:Comp:Link:What:PSR:Ceil:Dur][Offr|IDV:FxCb:Comp:What:Intl:PSR:Ceil:Dur]"
# # graphviz.plot(uinteriamb_dug,main="Unoptimized Interleaved Incremental 001 Association 00001 highlighted",highlight=list(arcs=arcs(uinteriamb_dug00001)))
# # graphviz.plot(uinteriamb_dug,main="Unoptimized Interleaved Incremental Association Optimized highlighted",highlight=list(arcs=arcs(interiamb_dug)))
# # graphviz.plot(interiamb_dug,main="optimized Interleaved Incremental Association UnOptimized highlighted",highlight=list(arcs=arcs(uinteriamb_dug)))
# 
# graphviz.plot(uinteriamb_dug,main="Unoptimized Interleaved Incremental Association IAMB highlighted",highlight=list(arcs=arcs(uiamb_dug)))
# 
# graphviz.plot(iamb_dug,main="Unoptimized Incremental Association Unoptimized IAMB highlighted",highlight=list(arcs=arcs(uiamb_dug)))
# graphviz.plot(uiamb_dug,main="optimized Incremental Association IAMB highlighted",highlight=list(arcs=arcs(iamb_dug)))
```
To the study teams dismay, the bayesian learning results were not consistent across different methods. The unoptimized versions of the learning algorithms do better at catching statistical oddities at the cost of requiring more comparisons. After analyzing the unoptimized and optimized versions, the team chose to adopt one arc that appeared in the both unoptimized versions, namely duration to offers. Seperate analysis shown in the Contract_Competition found that longer contracts did have fewer offers even after controlling for contract ceiling.

Similarly, the grow shrink algorithm included a link from Ceil to FxCb. Analysis shown in Contract_Pricing holds up that connection, even after controlling for PSR. This does not appear to be true of duration or competition. There may well be a link between IDV and Fixed-price, but there's enough complexity in the causality in that relationship that the team chose not to force it.

* Whitelist Ceil (Ceil) to FxCb because higher ceiling contracts are more likely to be cost-based or combination.

Dur (Duration) to Number of Offers received (Offr) was considered, but this ended up resulting in the Ceil to Dur being lost. Of the two, Ceil has the more straightforward relationship, so it was kept. 


The other alternative within the package, a min-max parents and childrens approach, 



```r
highlightWL3 <- list(arcs = ContractWL, col = "blue")

highlightLists3 <- list(arcs = data.frame(from=c(as.character(ContractWL$from),
                                                 as.character(ContractBL$to)),
                                          to=c(as.character(ContractWL$to),
                                               as.character(ContractBL$from))),
                        col = "blue")

uinteriamb3_dug00001<-inter.iamb(ContractModel,blacklist=ContractBL,whitelist=ContractWL,optimized=FALSE,alpha=0.000001)
# uinteriamb3_dug<-inter.iamb(ContractModel,blacklist=ContractBL,whitelist=ContractWL,optimized=FALSE,alpha=0.001)
# 
# uinteriamb3_dug05<-inter.iamb(ContractModel,blacklist=ContractBL,whitelist=ContractWL,optimized=FALSE,alpha=0.05)
# 
# all.equal(uinteriamb_dug,uinteriamb3_dug00001)
# 
# graphviz.plot(uinteriamb_dug00001,main="Unoptimized Interleaved Incremental Association Whitelist highlighted",highlight=highlightWL3)
# graphviz.plot(uinteriamb_dug00001,main="Unoptimized Interleaved Incremental Association either list highlighted",highlight=highlightLists3)
# graphviz.plot(uinteriamb_dug00001,main="Unoptimized Interleaved Incremental Association IAMB highlighted",highlight=list(arcs=arcs(uiamb_dug)))
# # graphviz.plot(uiamb_dug,main="Unoptimized Interleaved Incremental Association ",highlight=list(arcs=arcs(uinteriamb_dug)))
# graphviz.plot(uinteriamb_dug,main="Unoptimized Interleaved Incremental Association IAMB highlighted",highlight=list(arcs=arcs(uinteriamb3_dug00001)))
# graphviz.plot(uinteriamb3_dug00001,main="Unoptimized Interleaved Incremental Association IAMB highlighted",highlight=list(arcs=arcs(uinteriamb_dug)))
# graphviz.plot(uinteriamb3_dug,main="Unoptimized Interleaved Incremental Association IAMB highlighted",highlight=list(arcs=arcs(uinteriamb_dug)))


# uinteriamb3_dug00001<-set.arc(uinteriamb3_dug00001,"Ceil","Offr")
# uinteriamb3_dug00001<-drop.arc(uinteriamb3_dug00001,"Dur","Offr")

graphviz.plot(uinteriamb3_dug00001,main="Initial Number of Offers Bayesian Network",highlight=highlightWL3)
```

![](competition_model_trimmed_files/figure-html/BayesianExecutionRd3-1.png) 

```r
ContractModel$Offr<-factor(ContractModel$Offr, 
                           levels=c("  1","  2","[  3,  5)","[  5,999]"),
                           labels=c("1","2","3-4","5+"),
                           ordered=TRUE
                           )

ContractModel$Ceil<-factor(ContractModel$Ceil,
                                        levels=c("[0.00e+00,1.50e+04)"
                                                 ,"[1.50e+04,1.00e+05)"
                                                 ,"[1.00e+05,1.00e+06)"
                                                 ,"[1.00e+06,3.00e+07)"
                                                 ,"[3.00e+07,3.36e+12]"
                                                 ),
                                        labels=c("[0,15k)"
                                                 ,"[15k,100k)"
                                                 ,"[100k,1m)"
                                                 ,"[1m,30m)"
                                                 ,"[30m+]"
                                                 ),
                                        ordered=TRUE)
```



```r
CompetitionNetwork<-uinteriamb3_dug00001
modelstring(CompetitionNetwork)
```

```
## [1] "[Who][What|Who][Intl|Who:What][PSR|Who:What:Intl][Comp|What:Intl:PSR][Link|Who:What:Intl:PSR][IDV|Comp:What:Intl:PSR][Ceil|IDV:What:Intl:PSR][FxCb|PSR:Ceil][Dur|IDV:Comp:Link:What:Intl:PSR:Ceil][Offr|IDV:FxCb:Comp:Link:What:Intl:Ceil]"
```

```r
# CompetitionFitted<-bn.fit(x=CompetitionNetwork,data=ContractModel,method="bayes")
# summary(ContractModel)
# cpquery(CompetitionFitted, 
#         event =  (Offr=="1"),
#         evidence = (FxCb=="Fixed-Price") & (Comp=="Comp."))
# cpquery(CompetitionFitted, 
#         event =  (Offr=="1"),
#         evidence = (FxCb=="Cost-Based") & (Comp=="Comp."))
# cpquery(CompetitionFitted, 
#         event =  (Offr=="1"),
#         evidence = (FxCb=="Cost-Based") & (Comp=="Comp.") & (Ceil=="[0,15k)"))
# cpquery(CompetitionFitted, 
#         event =  (Offr=="1"),
#         evidence = (FxCb=="Cost-Based") & (Comp=="Comp.") & (Ceil=="[15k,100k)"))
# cpquery(CompetitionFitted, 
#         event =  (Offr=="1"),
#         evidence = (FxCb=="Cost-Based") & (Comp=="Comp.") & (Ceil=="[100k,1m)"))
# cpquery(CompetitionFitted, 
#         event =  (Offr=="1"),
#         evidence = (FxCb=="Cost-Based") & (Comp=="Comp.") & (Ceil=="[1m,30m)"))
# cpquery(CompetitionFitted, 
#         event =  (Offr=="1"),
#         evidence = (FxCb=="Cost-Based") & (Comp=="Comp.") & (Ceil=="[30m+]"))
# 
# y<-paste("Ceil=='",levels(ContractModel$Ceil),"'",sep="")
# 
# sapply(levels(ContractModel$Ceil),function(x) eval(sprintf('cpquery(CompetitionFitted, 
#         event =  (Offr=="1"),
#         evidence = (FxCb=="Cost-Based") & (Comp=="Comp.") & (Ceil=="%s")',x)))
# 
# sapply(levels(ContractModel$Ceil),FixedPriceTest)

# FixedPriceTest("[30m+]")
# 
# FixedPriceTest<-function(x){
#     cpquery(CompetitionFitted, 
#         event =  (Offr=="1"),
#         evidence = (FxCb=="Cost-Based") & (Comp=="Comp.") & (Ceil==x)
#         )
# }
# 
# eval("cpquery(CompetitionFitted, \n        event =  (Offr==\"1\"),\n        evidence = (FxCb==\"Cost-Based\") & (Comp==\"Comp.\") & (Ceil==\"[30m+]\")")
# levels(ContractModel$Ceil)
# 
# sapply(levels(ContractModel$Ceil),function(x) paste(x,"")) 
# 
# sapply(c(1,2,3),function(x)   cpquery(CompetitionFitted, 
#         event =  (Offr=="1"),
#         evidence = (FxCb=="Cost-Based") & (Comp=="Comp."))) (Ceil==x) &  (Ceil==x)) #& (Ceil==x)
# 

require(gRain)
# cad.cpt <- extractCPT(ContractModel, as.graphNEL(CompetitionNetwork), smooth = 0.001)
# compGin <- grain(compileCPT(cad.cpt))

compGin <- grain(as.graphNEL(CompetitionNetwork), data=ContractModel, smooth = 0.0000001)
```

```
## extractCPT - data.frame
```

```r
summary(compGin)
```

```
## Independence network: Compiled: FALSE Propagated: FALSE 
##  Nodes : Named chr [1:11] "IDV" "FxCb" "Comp" "Link" "Who" "What" ...
##  - attr(*, "names")= chr [1:11] "IDV" "FxCb" "Comp" "Link" ...
```

```r
# 
# str(compGin$universe$levels["FxCb"])
# cost.list <- list(nodes = c("FxCb"),
#                         states = c("Cost-Based"))
# cost.find <- setEvidence(compGin, 
#                         nodes=cost.list$nodes,
#                         states=cost.list$states)
# fixed.list<-list(nodes = c("FxCb"),
#                         states = c("Fixed-Price"))
# fixed.find <- setEvidence(compGin, 
#                         nodes=fixed.list$nodes,
#                         states=fixed.list$states)
# 


QueryCrossSectionOnNode<-function(
                        gin,
                        PivotNode,
                        QueryNode
                        )
    {
    looper<-NULL
    for(x in unlist(gin$universe$levels[PivotNode])){
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
# 
# 
# querygrain(cost.find, nodes = c("Offr"))
# querygrain(fixed.find, nodes = c("Offr"),states=c("1"))
# 
# qCostCeil<-QueryCrossSectionOnNode(cost.find,"Ceil","Offr")
# qFixedCeil<-QueryCrossSectionOnNode(fixed.find,"Ceil","Offr")
# 
# # debug(QueryCrossSectionOnNode)
# qCostWhat<-QueryCrossSectionOnNode(cost.find,"What","Offr")
# qFixedWhat<-QueryCrossSectionOnNode(ContractModel,fixed.find,"What","Offr")
# row(qCostWhat)
# qCostWhat[6,]
# qFixedWhat[6,]
# 
# qCostPSR<-QueryCrossSectionOnNode(cost.find,"PSR","Offr")
# qFixedPSR<-QueryCrossSectionOnNode(fixed.find,"PSR","Offr")
# 
# qCostPSR[2,]
# qFixedPSR[2,]
# 
# for(x in 1:ncol(eCeil)){
#     
#     print(getEvidence(eCeil[,x]))
#     print(querygrain(eCeil[[,x]],nodes=c("Offr")))
#     }
# 
# 
# eCeil<-CompleteEvidenceLists(ContractModel,compGin,"Ceil",gin2.list)
# for(x in 1:ncol(eCeil)){
#     
#     print(getEvidence(eCeil[,x]))
#     print(querygrain(eCeil[[,x]],nodes=c("Offr")))
#     }
# 
# 
# test<-eCeil[,1]
# querygrain(test,nodes=c("Offr"))

# 
# CeilingFrames<-sapply(levels(ContractModel$Ceil),function(x) list(nodes = c(gin2.list$nodes,"Ceil"),
#                         states = c(gin2.list$states, x))
# 
#     
#     
#     data.frame(nodes = c("Comp","FxCb","Ceil"),
#                         states = c( "Comp.","Fixed-Price",x)))
# result<-list()
# for(x in 1:ncol(CeilingFrames )){
#     test <- setFinding(compGin, CeilingFrames[,x])
#     result<-rbind(result,list(CeilingFrames[,x],querygrain(test, nodes = c("Offr"))))
# }
# ncol(CeilingFrames )
# 
# test2<-setEvidence(compGin, CeilingFrames[,2])
# getEvidence(test2)
# # str(test2)
# 
# str(gin2.find)
# # str(eCeil[,1])
# 
# # as.dateCeil[,1]
# querygrain(gin2.find,nodes=c("Offr"))
# # querygrain(eCeil[,1],nodes=c("Offr"))
# # getEvidence(test2)


# 
# 
# ceiltest<-sapply(levels(ContractModel$Ceil),
#                  function(x)  setFinding(compGin, nodes = c("Comp","FxCb","Ceil"),
#                         states = c( "Comp.","Fixed-Price",x)))
# 
# sapply(unlist(ceiltest, function(x) querygrain(x, nodes = c("Offr")))
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
levels(ContractModel$What)
```

```
## [1] "Aircraft and Drones"            "Electronics and Communications"
## [3] "Facilities and Construction"    "Land Vehicles"                 
## [5] "Missile and Space Systems"      "Other"                         
## [7] "Ships & Submarines"             "Weapons and Ammunition"
```

```r
AircraftFind<- setEvidence(compGin, 
                        nodes=c("What"),
                        states=c("Aircraft and Drones")
)

AircraftCompFind<- setEvidence(AircraftFind, 
                        nodes=c("Comp"),
                        states=c("Comp.")
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
getEvidence(AircraftCompFind)
```

```
## Finding: 
## What: Aircraft and Drones
## Comp: Comp.
## Pr(Finding)= 0.05865099
```

```r
querygrain(AircraftCompFind,"Offr")
```

```
## $Offr
## Offr
##         1         2       3-4        5+ 
## 0.2530169 0.2980877 0.2082743 0.2406212
```

```r
NotAircraftFind<- setEvidence(compGin, 
                        nodes=c("What"),
                        states=c(list(
                            levels(ContractModel$What)[levels(ContractModel$What)!=
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

```r
NotAircraftCompFind<- setEvidence(NotAircraftFind, 
                        nodes=c("Comp"),
                        states=c("Comp.")
)


getEvidence(NotAircraftCompFind)
```

```
## Finding: 
## What: Electronics and Communications, Facilities and Construction, Land Vehicles, Missile and Space Systems, Other, Ships & Submarines, Weapons and Ammunition
## Comp: Comp.
## Pr(Finding)= 0.7433984
```

```r
querygrain(NotAircraftCompFind,"Offr")
```

```
## $Offr
## Offr
##         1         2       3-4        5+ 
## 0.1917945 0.2067809 0.2473978 0.3540268
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
levels(ContractModel$Ceil)
```

```
## [1] "[0,15k)"    "[15k,100k)" "[100k,1m)"  "[1m,30m)"   "[30m+]"
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
                            levels(ContractModel$Ceil)[levels(ContractModel$Ceil)!=
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
levels(ContractModel$Ceil)
```

```
## [1] "[0,15k)"    "[15k,100k)" "[100k,1m)"  "[1m,30m)"   "[30m+]"
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
                            levels(ContractModel$Ceil)[!levels(ContractModel$Ceil) %in%
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
                            levels(ContractModel$Ceil)[levels(ContractModel$Ceil)!=c("[0,15k)")]))
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
#1M

Under1mCeilFind<- setEvidence(compGin, 
                        nodes=c("Ceil"),
                        states=c(list(c("[0,15k)","[15k,100k)","[100k,1m)")))
)

Under1mCeilCompFind<- setEvidence(Under1mCeilFind, 
                        nodes=c("Comp"),
                        states=c("Comp.")
)


getEvidence(Under1mCeilFind)
```

```
## Finding: 
## Ceil: [0,15k), [15k,100k), [100k,1m)
## Pr(Finding)= 0.9795632
```

```r
querygrain(Under1mCeilFind,"Offr")
```

```
## $Offr
## Offr
##         1         2       3-4        5+ 
## 0.3479289 0.1734406 0.1985386 0.2800919
```

```r
getEvidence(Under1mCeilCompFind)
```

```
## Finding: 
## Ceil: [0,15k), [15k,100k), [100k,1m)
## Comp: Comp.
## Pr(Finding)= 0.7869142
```

```r
querygrain(Under1mCeilCompFind,"Offr")
```

```
## $Offr
## Offr
##         1         2       3-4        5+ 
## 0.1955597 0.2138058 0.2447258 0.3459087
```

```r
NotUnder1mCeilFind<- setEvidence(compGin, 
                        nodes=c("Ceil"),
                        states=c(list(
                            levels(ContractModel$Ceil)[!levels(ContractModel$Ceil) %in%
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
querygrain(NotUnder1mCeilFind,"Offr")
```

```
## $Offr
## Offr
##         1         2       3-4        5+ 
## 0.4263529 0.1465433 0.1762723 0.2508316
```

```r
NotUnder1mCeilCompFind<- setEvidence(NotUnder1mCeilFind, 
                        nodes=c("Comp"),
                        states=c("Comp.")
)

getEvidence(NotUnder1mCeilCompFind)
```

```
## Finding: 
## Ceil: [1m,30m), [30m+]
## Comp: Comp.
## Pr(Finding)= 0.01513517
```

```r
querygrain(NotUnder1mCeilCompFind,"Offr")
```

```
## $Offr
## Offr
##         1         2       3-4        5+ 
## 0.2332760 0.1953700 0.2347116 0.3366424
```

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
levels(ContractModel$IDV)
```

```
## [1] "Def/Pur" "IDV"
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
                            levels(ContractModel$IDV)[levels(ContractModel$IDV)!=
                                                           "IDV"]))
                        )

getEvidence(NotIDVFind)
```

```
## Finding: 
## IDV: Def/Pur
## Pr(Finding)= 0.3538549
```

```r
querygrain(NotIDVFind,"Offr")
```

```
## $Offr
## Offr
##         1         2       3-4        5+ 
## 0.4656129 0.1346278 0.1859637 0.2137956
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
##  IDV: Def/Pur
## Comp: Comp.
## Pr(Finding)= 0.2646741
```

```r
querygrain(NotIDVCompFind,"Offr")
```

```
## $Offr
## Offr
##         1         2       3-4        5+ 
## 0.2987058 0.1768391 0.2449814 0.2794737
```

##Contracts with longer initially expected periods of performance will encounter more problems.
 *	Past CSIS research on cost overruns in major defense acquisition projects have found that cost growth is correlated with contract duration. "If cost increases accrue over time, then programs with an older baseline estimate would tend to accumulate relatively higher cost increases. The data for the analyzed programs show that older programs indeed experience larger overruns... this growth correlation not only provides further evidence for the assertion that cost growth occurs steadily throughout the program lifespan, but it also suggests that younger programs are not performing better than older programs." (David Berteau et al.: Cost and Time Overruns for Major Defense Acquisition Programs (2011))
 *	Dependent Variables:
 *	Contract termination, number of change orders, and cost of change orders.
 *	Initial Taxonomy:
 *	Period of performance will be measured as expected contract duration in 4-week months, rounding up.
 *	Note: CSIS is including a separate hypothesis (Slide 9) to test whether fixed-price contracts encounter an even greater number of problems under contracts with longer expected periods of performance 



```r
        levels(ContractModel$Dur)
```

```
## [1] "[    0,   61)" "[   61,  214)" "[  214,  366)" "[  366,33192]"
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
        LongDurCompFind<- setEvidence(LongDurFind, 
                                nodes=c("Comp"),
                                states=c("Comp.")
        )
        getEvidence(LongDurCompFind)
```

```
## Finding: 
##  Dur: [  366,33192]
## Comp: Comp.
## Pr(Finding)= 0.03541949
```

```r
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
getEvidence(LongDurCompFind)
```

```
## Finding: 
##  Dur: [  366,33192]
## Comp: Comp.
## Pr(Finding)= 0.03541949
```

```r
querygrain(LongDurCompFind,"Offr")
```

```
## $Offr
## Offr
##         1         2       3-4        5+ 
## 0.2596487 0.2272653 0.2227794 0.2903066
```

```r
NotLongDurFind<- setEvidence(compGin, 
                        nodes=c("Dur"),
                        states=c(list(
                            levels(ContractModel$Dur)[levels(ContractModel$Dur)!=
                                                           "[  366,33192]"]))
                        )

getEvidence(NotLongDurFind)
```

```
## Finding: 
## Dur: [    0,   61), [   61,  214), [  214,  366)
## Pr(Finding)= 0.9440577
```

```r
querygrain(NotLongDurFind,"Offr")
```

```
## $Offr
## Offr
##         1         2       3-4        5+ 
## 0.3392145 0.1744587 0.2012921 0.2850346
```

```r
NotLongDurCompFind<- setEvidence(NotLongDurFind, 
                        nodes=c("Comp"),
                        states=c("Comp.")
)


getEvidence(NotLongDurCompFind)
```

```
## Finding: 
##  Dur: [    0,   61), [   61,  214), [  214,  366)
## Comp: Comp.
## Pr(Finding)= 0.7666299
```

```r
querygrain(NotLongDurCompFind,"Offr")
```

```
## $Offr
## Offr
##         1         2       3-4        5+ 
## 0.1933433 0.2128199 0.2455421 0.3482947
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
levels(ContractModel$Ceil)
```

```
## [1] "[0,15k)"    "[15k,100k)" "[100k,1m)"  "[1m,30m)"   "[30m+]"
```

```r
largeRnDfind<- setEvidence(compGin, 
                        nodes=c("PSR","Ceil"),
                        states=c("R&D","[30m+]")
)

getEvidence(largeRnDfind)
```

```
## Finding: 
##  PSR: R&D
## Ceil: [30m+]
## Pr(Finding)= 5.518707e-05
```

```r
largeRnDcompFind<- setEvidence(largeRnDfind, 
                        nodes=c("Comp"),
                        states=c("Comp.")
)
getEvidence(largeRnDcompFind)
```

```
## Finding: 
##  PSR: R&D
## Ceil: [30m+]
## Comp: Comp.
## Pr(Finding)= 4.248541e-05
```

```r
FxCBresult<-QueryCrossSectionOnNode(largeRnDcompFind,"FxCb","Offr")

#Combination
FxCBresult[1,1]
```

```
## $evidence
## Finding: 
##  PSR: R&D
## Ceil: [30m+]
## Comp: Comp.
## FxCb: Combination 
## or Other
## Pr(Finding)= 4.952287e-06
```

```r
FxCBresult[1,2]
```

```
## $result
## $result$Offr
## Offr
##         1         2       3-4        5+ 
## 0.3145995 0.2564465 0.2496382 0.1793159
```

```r
#Cost-plus
FxCBresult[2,1]
```

```
## $evidence
## Finding: 
##  PSR: R&D
## Ceil: [30m+]
## Comp: Comp.
## FxCb: Cost-Based
## Pr(Finding)= 3.114728e-05
```

```r
FxCBresult[2,2]
```

```
## $result
## $result$Offr
## Offr
##         1         2       3-4        5+ 
## 0.2255373 0.2108130 0.2404177 0.3232319
```

```r
#Fixed-price
FxCBresult[3,1]
```

```
## $evidence
## Finding: 
##  PSR: R&D
## Ceil: [30m+]
## Comp: Comp.
## FxCb: Fixed-Price
## Pr(Finding)= 6.385844e-06
```

```r
FxCBresult[3,2]
```

```
## $result
## $result$Offr
## Offr
##         1         2       3-4        5+ 
## 0.2972096 0.2297503 0.1983109 0.2747292
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
levels(ContractModel$Link)
```

```
## [1] "    0"         "[    1,  750)" "[  750,67263]"
```

```r
HighLinkFind<- setEvidence(compGin, 
                        nodes=c("Link"),
                        states=c("[  750,67263]")
)

getEvidence(HighLinkFind)
```

```
## Finding: 
## Link: [  750,67263]
## Pr(Finding)= 0.2453573
```

```r
HighLinkCompFind<- setEvidence(HighLinkFind, 
                        nodes=c("Comp"),
                        states=c("Comp.")
)
getEvidence(HighLinkCompFind)
```

```
## Finding: 
## Link: [  750,67263]
## Comp: Comp.
## Pr(Finding)= 0.1746073
```

```r
FxCBresult<-QueryCrossSectionOnNode(HighLinkCompFind,"FxCb","Offr")

#Combination
FxCBresult[1,1]
```

```
## $evidence
## Finding: 
## Link: [  750,67263]
## Comp: Comp.
## FxCb: Combination 
## or Other
## Pr(Finding)= 0.001668255
```

```r
FxCBresult[1,2]
```

```
## $result
## $result$Offr
## Offr
##         1         2       3-4        5+ 
## 0.3674903 0.3269367 0.1793328 0.1262401
```

```r
#Cost-plus
FxCBresult[2,1]
```

```
## $evidence
## Finding: 
## Link: [  750,67263]
## Comp: Comp.
## FxCb: Cost-Based
## Pr(Finding)= 0.00692329
```

```r
FxCBresult[2,2]
```

```
## $result
## $result$Offr
## Offr
##         1         2       3-4        5+ 
## 0.3411438 0.2386997 0.1824740 0.2376825
```

```r
#Fixed-price
FxCBresult[3,1]
```

```
## $evidence
## Finding: 
## Link: [  750,67263]
## Comp: Comp.
## FxCb: Fixed-Price
## Pr(Finding)= 0.1660157
```

```r
FxCBresult[3,2]
```

```
## $result
## $result$Offr
## Offr
##         1         2       3-4        5+ 
## 0.2649649 0.2513328 0.2120370 0.2716653
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
        levels(ContractModel$Dur)
```

```
## [1] "[    0,   61)" "[   61,  214)" "[  214,  366)" "[  366,33192]"
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
        LongDurCompFind<- setEvidence(LongDurFind, 
                                nodes=c("Comp"),
                                states=c("Comp.")
        )
        getEvidence(LongDurCompFind)
```

```
## Finding: 
##  Dur: [  366,33192]
## Comp: Comp.
## Pr(Finding)= 0.03541949
```

```r
        FxCBresult<-QueryCrossSectionOnNode(LongDurCompFind,"FxCb","Offr")
        
        #Combination
        FxCBresult[1,1]
```

```
## $evidence
## Finding: 
##  Dur: [  366,33192]
## Comp: Comp.
## FxCb: Combination 
## or Other
## Pr(Finding)= 0.0007496621
```

```r
        FxCBresult[1,2]
```

```
## $result
## $result$Offr
## Offr
##         1         2       3-4        5+ 
## 0.2591922 0.2593155 0.2245120 0.2569803
```

```r
        #Cost-plus
        FxCBresult[2,1]
```

```
## $evidence
## Finding: 
##  Dur: [  366,33192]
## Comp: Comp.
## FxCb: Cost-Based
## Pr(Finding)= 0.003915736
```

```r
        FxCBresult[2,2]
```

```
## $result
## $result$Offr
## Offr
##         1         2       3-4        5+ 
## 0.3142475 0.2432304 0.1710390 0.2714831
```

```r
        #Fixed-price
        FxCBresult[3,1]
```

```
## $evidence
## Finding: 
##  Dur: [  366,33192]
## Comp: Comp.
## FxCb: Fixed-Price
## Pr(Finding)= 0.03075409
```

```r
        FxCBresult[3,2]
```

```
## $result
## $result$Offr
## Offr
##         1         2       3-4        5+ 
## 0.2527080 0.2244513 0.2293250 0.2935157
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
levels(ContractModel$PSR)
```

```
## [1] "Products" "R&D"      "Services"
```

```r
    SoftwareFind<- setEvidence(compGin, 
                            nodes=c("What","PSR"),
                            states=c("Electronics and Communications","Services"))
    
    
    getEvidence(SoftwareFind)
```

```
## Finding: 
## What: Electronics and Communications
##  PSR: Services
## Pr(Finding)= 0.02785525
```

```r
    SoftwareCompFind<- setEvidence(SoftwareFind, 
                            nodes=c("Comp"),
                            states=c("Comp.")
    )
    getEvidence(SoftwareCompFind)
```

```
## Finding: 
## What: Electronics and Communications
##  PSR: Services
## Comp: Comp.
## Pr(Finding)= 0.01825118
```

```r
            FxCBresult<-QueryCrossSectionOnNode(SoftwareCompFind,"FxCb","Offr")
            
            #Combination
            FxCBresult[1,1]
```

```
## $evidence
## Finding: 
## What: Electronics and Communications
##  PSR: Services
## Comp: Comp.
## FxCb: Combination 
## or Other
## Pr(Finding)= 0.000352052
```

```r
            FxCBresult[1,2]
```

```
## $result
## $result$Offr
## Offr
##         1         2       3-4        5+ 
## 0.2301743 0.3592055 0.1569352 0.2536850
```

```r
            #Cost-plus
            FxCBresult[2,1]
```

```
## $evidence
## Finding: 
## What: Electronics and Communications
##  PSR: Services
## Comp: Comp.
## FxCb: Cost-Based
## Pr(Finding)= 0.001732241
```

```r
            FxCBresult[2,2]
```

```
## $result
## $result$Offr
## Offr
##         1         2       3-4        5+ 
## 0.3872955 0.2282474 0.1727175 0.2117397
```

```r
            #Fixed-price
            FxCBresult[3,1]
```

```
## $evidence
## Finding: 
## What: Electronics and Communications
##  PSR: Services
## Comp: Comp.
## FxCb: Fixed-Price
## Pr(Finding)= 0.01616688
```

```r
            FxCBresult[3,2]
```

```
## $result
## $result$Offr
## Offr
##         1         2       3-4        5+ 
## 0.2878610 0.2262571 0.2378161 0.2480658
```

```r
SoftwareFind<- setEvidence(compGin, 
                        nodes=c("What","PSR"),
                        states=c("Electronics and Communications",list(c("Services","R&D")))
)
getEvidence(SoftwareFind)
```

```
## Finding: 
## What: Electronics and Communications
##  PSR: Services, R&D
## Pr(Finding)= 0.02957722
```

```r
SoftwareCompFind<- setEvidence(SoftwareFind, 
                        nodes=c("Comp"),
                        states=c("Comp.")
)

FxCBresult<-QueryCrossSectionOnNode(SoftwareCompFind,"FxCb","Offr")
        
        #Combination
        FxCBresult[1,1]
```

```
## $evidence
## Finding: 
## What: Electronics and Communications
##  PSR: Services, R&D
## Comp: Comp.
## FxCb: Combination 
## or Other
## Pr(Finding)= 0.0004109416
```

```r
        FxCBresult[1,2]
```

```
## $result
## $result$Offr
## Offr
##         1         2       3-4        5+ 
## 0.2448219 0.3419415 0.1573452 0.2558914
```

```r
        #Cost-plus
        FxCBresult[2,1]
```

```
## $evidence
## Finding: 
## What: Electronics and Communications
##  PSR: Services, R&D
## Comp: Comp.
## FxCb: Cost-Based
## Pr(Finding)= 0.002539111
```

```r
        FxCBresult[2,2]
```

```
## $result
## $result$Offr
## Offr
##         1         2       3-4        5+ 
## 0.3945996 0.2331509 0.1529838 0.2192657
```

```r
        #Fixed-price
        FxCBresult[3,1]
```

```
## $evidence
## Finding: 
## What: Electronics and Communications
##  PSR: Services, R&D
## Comp: Comp.
## FxCb: Fixed-Price
## Pr(Finding)= 0.01682046
```

```r
        FxCBresult[3,2]
```

```
## $result
## $result$Offr
## Offr
##         1         2       3-4        5+ 
## 0.2874211 0.2248195 0.2375204 0.2502390
```

