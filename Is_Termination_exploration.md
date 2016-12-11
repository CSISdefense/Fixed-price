 Dod Fixed-Price Study: Is Terminated exploration
============================================================================

```{r echo = TRUE}
##install.packages("ggplot2")

##library("ggplot2")

setwd("K:\\Development\\Fixed-price")

Path<-"K:\\2007-01 PROFESSIONAL SERVICES\\R scripts and data\\"
source(paste(Path,"lookups.r",sep=""))
```

Contracts are classified using a mix of numerical and categorical variables. While the changes in numerical variables are easy to grasp and summarize, a contract may have one line item that is competed and another that is not. As is detailed in the exploration on R&D, we are only considering information available prior to contract start. The percentage of contract obligations that were competed is a valuable benchmark, but is highly influenced by factors that occured after contract start..

## Contract Terminations


Contract terminations and the number of change orders can be calculated for the entire sample.  Contract termination is determined using the *Reason for Modification* field in FPDS.  A contract is considered to be terminated if it has at least one modification with the following values:

* "Terminate for Default (complete or partial)"
* "Terminate for Convenience (complete or partial)"
* "Terminate for Cause"
* "Legal Contract Cancellation"

These four catetegories and the "Close Out" category are used to mark a contract as closed.  Many contracts in FPDS and in the sample are never marked closed.  




```{r echo = TRUE}
setwd("K:\\Development\\Fixed-price")

ContractWeighted  <- read.csv(
    paste("data\\defense_contract_CSIScontractID_sample_15000_SumofObligatedAmount.csv", sep = ""),
    header = TRUE, sep = ",", dec = ".", strip.white = TRUE, 
    na.strings = c("NULL","NA",""),
    stringsAsFactors = TRUE
    )

#These will probably be moved into apply_lookups at some point
#ContractWeighted <- apply_lookups(Path,ContractWeighted)

```

**A Histogram of the IsTerminated data** showing the distribution of whether or not a contract was terminated each year from 2007.  

```{r echo = TRUE}

library("ggplot2")

ContractWeighted<-subset(ContractWeighted, StartFiscal_Year>=2007)

ggplot(
  data = ContractWeighted,
  aes_string(x = "IsTerminated"),
  ) + geom_bar(binwidth=0.5) +
facet_wrap("StartFiscal_Year")


```

