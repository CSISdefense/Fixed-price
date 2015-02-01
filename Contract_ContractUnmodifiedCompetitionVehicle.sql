USE [DIIG]
GO

/****** Object:  View [Contract].[ContractUnmodifiedCompetitionVehicle]    Script Date: 1/28/2015 4:04:43 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO





ALTER VIEW [Contract].[ContractCompetitionVehicle]
AS
select M.CSIScontractID
--NumberOfOffersReceived
,iif(M.MinOfNumberOfOffersReceived=MaxOfNumberOfOffersReceived,
	MaxOfNumberOfOffersReceived
	,NULL) as UnmodifiedNumberOfOffersReceived
--IsFullAndOpen
,iif(M.MinOfIsFullAndOpen=MaxOfIsFullAndOpen,
	MaxOfIsFullAndOpen
	,NULL) as UnmodifiedIsFullAndOpen
--IsSomeCompetition	
,ObligatedAmountIsSomeCompetition
,iif(M.MinOfIsSomeCompetition	=MaxOfIsSomeCompetition	,
	MaxOfIsSomeCompetition	
	,NULL) as UnmodifiedIsSomeCompetition	
--IsOnlyOneSource=MaxOfIsOnlyOneSource
,iif(M.MinOfIsOnlyOneSource=MaxOfIsOnlyOneSource,
	MaxOfIsOnlyOneSource
	,NULL) as UnmodifiedIsOnlyOneSource
--IsFollowonToCompetedAction
,iif(M.MinOfIsFollowonToCompetedAction=MaxOfIsFollowonToCompetedAction,
	MaxOfIsFollowonToCompetedAction
	,NULL) as UnmodifiedIsFollowonToCompetedAction
--multipleorsingleawardidc
,iif(M.MinOfmultipleorsingleawardidc=MaxOfmultipleorsingleawardidc,
	MaxOfmultipleorsingleawardidc
	,NULL) as Unmodifiedmultipleorsingleawardidc
--addmultipleorsingawardidc
,iif(M.MinOfaddmultipleorsingawardidc=MaxOfaddmultipleorsingawardidc,
	MaxOfaddmultipleorsingawardidc
	,NULL) as Unmodifiedaddmultipleorsingawardidc
--AwardOrIDVcontractactiontype
,iif(M.MinOfAwardOrIDVcontractactiontype=MaxOfAwardOrIDVcontractactiontype,
	MaxOfAwardOrIDVcontractactiontype
	,NULL) as UnmodifiedAwardOrIDVcontractactiontype
from (SELECT      
	c.CSIScontractID
     /* ,C.extentcompeted5Category
      ,C.reasonnotcompeted4category
      ,C.extentcompetedtext
	  ,C.ReasonNotCompetedtext*/	
	
	--Number Of Offers
	, Min(C.numberofoffersreceived) AS MinOfNumberOfOffersReceived
	, Max(C.numberofoffersreceived) AS MaxOfNumberOfOffersReceived
	--Competition Binaries IsFullAndOpen
	, Min(convert(int,iif(c.UseFairOpportunity=1
		,0
		,c.ExtentIsFullAndOpen
	))) AS MinOfIsFullAndOpen
	, Max(convert(int,iif(c.UseFairOpportunity=1
		,0
		,c.ExtentIsFullAndOpen
	))) AS MaxOfIsFullAndOpen
	
	--Competition Binaries IsSomeCompetition
	,sum(iif((c.UseFairOpportunity=1 and isnull(c.FairIsSomeCompetition,c.ExtentIsSomeCompetition)=1) or 
			(c.UseFairOpportunity=0 and isnull(c.ExtentIsSomeCompetition,c.FairIsSomeCompetition)=1)
			,obligatedamount
			,NULL) 
	) as ObligatedAmountIsSomeCompetition
	,min(convert(int,iif(c.UseFairOpportunity=1
		,isnull(c.FairIsSomeCompetition,c.ExtentIsSomeCompetition)
		,isnull(c.ExtentIsSomeCompetition,c.FairIsSomeCompetition)
	))) as MinOfIsSomeCompetition	
	,max(convert(int,iif(c.UseFairOpportunity=1
		,isnull(c.FairIsSomeCompetition,c.ExtentIsSomeCompetition)
		,isnull(c.ExtentIsSomeCompetition,c.FairIsSomeCompetition)
	))) as MaxOfIsSomeCompetition	
		--Competition Binaries IsOnlyOneSource
	,min(convert(int,iif(c.UseFairOpportunity=1
		,isnull(c.FairIsonlyonesource,c.is6_302_1exception)
		,isnull(c.is6_302_1exception,c.FairIsonlyonesource)
	))) as MinOfIsOnlyOneSource
	,max(convert(int,iif(c.UseFairOpportunity=1
		,isnull(c.FairIsonlyonesource,c.is6_302_1exception)
		,isnull(c.is6_302_1exception,c.FairIsonlyonesource)
	))) as MaxOfIsOnlyOneSource
	--Competition Binaries IsFollowonToCompetedAction
	,min(convert(int,iif(c.UseFairOpportunity=1
		,isnull(c.FairIsfollowontocompetedaction,c.isfollowontocompetedaction)
		,isnull(c.isfollowontocompetedaction,c.FairIsfollowontocompetedaction)
	))) as MinOfIsFollowonToCompetedAction
	,max(convert(int,iif(c.UseFairOpportunity=1
		,isnull(c.FairIsfollowontocompetedaction,c.isfollowontocompetedaction)
		,isnull(c.isfollowontocompetedaction,c.FairIsfollowontocompetedaction)
	))) as MaxOfIsFollowonToCompetedAction
	--Vehicle
	, Min(C.multipleorsingleawardidc) AS MinOfmultipleorsingleawardidc
	, Max(C.multipleorsingleawardidc) AS MaxOfmultipleorsingleawardidc
	, Min(convert(int,C.addmultipleorsingawardidc)) AS MinOfaddmultipleorsingawardidc
	, Max(convert(int,C.addmultipleorsingawardidc)) AS MaxOfaddmultipleorsingawardidc
	, Min(C.AwardOrIDVcontractactiontype) AS MinOfAwardOrIDVcontractactiontype
	, Max(C.AwardOrIDVcontractactiontype) AS MaxOfAwardOrIDVcontractactiontype
  FROM contract.[ContractCompetitionVehiclePartial] as C
group by CSIScontractID ) as M









