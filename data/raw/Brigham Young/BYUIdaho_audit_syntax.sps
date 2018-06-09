* Encoding: UTF-8.
* Import data to SPSS from file 'raw_BYUI.csv'.
PRESERVE.
SET DECIMAL DOT.

GET DATA  /TYPE=TXT
  /FILE="C:\Users\bjwiggins\Dropbox\raw_BYUI.csv" /*edit file path to conform with local file storage
  /DELIMITERS=","
  /QUALIFIER='"'
  /ARRANGEMENT=DELIMITED
  /FIRSTCASE=71
  /DATATYPEMIN PERCENTAGE=95.0
  /VARIABLES=
  StartDate 374X
  EndDate 241X
  Status 243X
  IPAddress 152X
  Progress 139X
  Durationinseconds 95X
  Finished 132X
  RecordedDate 152X
  ResponseId 162X
  RecipientLastName 32X
  RecipientFirstName 33X
  RecipientEmail 29X
  ExternalReference 36X
  LocationLatitude 31X
  LocationLongitude 32X
  DistributionChannel 34X
  Q12 44X
  Q17_1_A F22.0
  Q15_1_A F22.0
  Q36_4 F22.0
  Q5_1 F21.0
  Q6 F24.0
  Q7_1 F21.0
  Q22_1 F22.0
  Q23_1 F22.0
  Q21_1 F22.0
  Q37_4 F22.0
  Q13_1 F22.0
  Q14 F25.0
  Q15_1 F22.0
  Q18_1 F22.0
  Q19_1 F22.0
  Q17_1 F22.0
  Q38_4 F22.0
  Q16_1 F22.0
  Q14_1 F22.0
  Q39_4 F22.0
  Q6Topics 55X
  /MAP.
RESTORE.
CACHE.
EXECUTE.
DATASET NAME DataSet1 WINDOW=FRONT.

* Rename variables.
 RENAME VARIABLES (Q17_1_A = lkl1).
 RENAME VARIABLES (Q15_1_A  =  imp1).       
 RENAME VARIABLES(Q36_4  =  bad1).
 RENAME VARIABLES(Q5_1  =  lkl2).
 RENAME VARIABLES(Q6  =  endnum1).
 RENAME VARIABLES(Q7_1  =  eff.split1).  
 RENAME VARIABLES(Q22_1  =  count.hard1). 
 RENAME VARIABLES(Q23_1  =  count.eff1). 
 RENAME VARIABLES(Q21_1  =  imp2).
 RENAME VARIABLES(Q37_4  =  bad2).       
 RENAME VARIABLES(Q13_1  =  lkl3).
 RENAME VARIABLES(Q14  =  endnum2).      
 RENAME VARIABLES(Q15_1  =  eff.split2).  
 RENAME VARIABLES(Q18_1  =  count.hard2).
 RENAME VARIABLES(Q19_1  =  count.eff2).  
 RENAME VARIABLES(Q17_1  =  imp3).       
 RENAME VARIABLES(Q38_4  =  bad3).
 RENAME VARIABLES(Q16_1  =  lkl4).       
 RENAME VARIABLES(Q14_1  =  imp4).
 RENAME VARIABLES(Q39_4  =  bad4).     

*Create had.read variable.
IF ((lkl1    >=   0) or (lkl3    >=   0) ) had.read=0.
IF ((lkl2    >=   0) or (lkl4    >=   0) ) had.read=1.
EXECUTE.

*Create lkl variable.
IF  (~MISSING(lkl1)) lkl=lkl1.
IF  (~MISSING(lkl2)) lkl=lkl2.
IF  (~MISSING(lkl3)) lkl=lkl3.
IF  (~MISSING(lkl4)) lkl=lkl4.
EXECUTE.

*Create endnum variable.
IF  (~MISSING(endnum1)) endnum=endnum1.
IF  (~MISSING(endnum2)) endnum=endnum2.
EXECUTE.

*Create endnum variable. 
IF (~MISSING(endnum)) load=1.
IF (MISSING(endnum)) load=0.
EXECUTE.

*Create eff.split variable.
IF  (~MISSING(eff.split1)) eff.split=eff.split1.
IF  (~MISSING(eff.split2)) eff.split=eff.split2.
EXECUTE.

*Create badness variable.
IF  (~MISSING(bad1)) badness=bad1.
IF  (~MISSING(bad2)) badness=bad2.
IF  (~MISSING(bad3)) badness=bad3.
IF  (~MISSING(bad4)) badness=bad4.
EXECUTE.

*Create importance variable.
IF  (~MISSING(imp1)) importance=imp1.
IF  (~MISSING(imp2)) importance=imp2.
IF  (~MISSING(imp3)) importance=imp3.
IF  (~MISSING(imp4)) importance=imp4.
EXECUTE.

*Create count.eff variable.
IF  (~MISSING(count.eff1)) count.eff=count.eff1.
IF  (~MISSING(count.eff2)) count.eff=count.eff2.
EXECUTE.

*Create count.hard variable.
IF  (~MISSING(count.hard1)) count.hard=count.hard1.
IF  (~MISSING(count.hard2)) count.hard=count.hard2.
EXECUTE.

*Create tempt variable.
COMPUTE tempt=  -(had.read-1).
EXECUTE.

*Delete raw data variables.
DELETE VARIABLES lkl1 TO bad4.

*Add casenumbers. 
COMPUTE id=$CASENUM.
EXECUTE.

*Identify cases for exclusion.
IF (MISSING(had.read)) excluded=1.
IF (MISSING(load)) excluded=2.
IF (MISSING(lkl)) excluded=3.
IF (load = 1 and (MISSING(eff.split) or MISSING(count.eff) or MISSING(count.hard))) excluded=4.
IF (endnum >= 561) excluded=5.
IF (eff.split = 0) excluded=6.
IF (MISSING(excluded)) excluded=0.
EXECUTE.

*Compute frequencies of exclusion categories.
FREQUENCIES VARIABLES=excluded
  /ORDER=ANALYSIS.

*Identify excluded cases.
SUMMARIZE
  /TABLES=excluded
  /FORMAT=VALIDLIST CASENUM TOTAL
  /TITLE='Case Summaries'
  /MISSING=VARIABLE
  /CELLS=COUNT.

* Frequency analysis returned 84 cases included, and 6 excluded (5 reporting endnum > 561, 1 reporting eff.split = 0).
* Excluded cases 11, 17, 19, 35, 81, 85

*Delete excluded cases.
SELECT IF excluded=0.
EXECUTE.

*Calculate mean and standard deviation for load, tempt, and lkl variables for comparison to prepared dataset.
DESCRIPTIVES VARIABLES=load tempt lkl
  /STATISTICS=MEAN STDDEV.

*The descriptive statistics matched those computed by the analysis team on their prepared dataset.
