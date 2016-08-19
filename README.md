# Clear
[Under development] R package for running the Comparison of the Laboratory Extreme Abnormality Ratio (CLEAR) algorithm.


Detection of adverse drug reaction signals using an electronic health records database: Comparison of the Laboratory Extreme Abnormality Ratio (CLEAR) algorithm. Yoon D, Park MY, Choi NK, Park BJ, Kim JH, Park RW. Clin Pharmacol Ther. 2012 Mar;91(3):467-74. doi: 10.1038/clpt.2011.248. Epub 2012 Jan 11.

Electronic health records (EHRs) are expected to be a good source of data for pharmacovigilance. However, current quantitative methods are not applicable to EHR data. We propose a novel quantitative postmarketing surveillance algorithm, the Comparison of Laboratory Extreme Abnormality Ratio (CLEAR), for detecting adverse drug reaction (ADR) signals from EHR data. The methodology involves calculating the odds ratio of laboratory abnormalities between a specific drug-exposed group and a matched unexposed group. Using a 10-year EHR data set, we applied the algorithm to test 470 randomly selected drug-event pairs. It was found possible to analyze a single drug-event pair in just 109 ± 159 seconds. In total, 120 of the 150 detected signals corresponded with previously reported ADRs (positive predictive value (PPV) = 0.837 ± 0.113, negative predictive value (NPV) = 0.659 ± 0.180). By quickly and efficiently identifying ADR signals from EHR data, the CLEAR algorithm can significantly contribute to the utilization of EHR data for pharmacovigilance.

Reference: http://www.ncbi.nlm.nih.gov/pubmed/?term=22237257

Getting Started (ONLY CDMv4)
===============
```r
install.packages("devtools")
library(devtools)
install_github("ohdsi/SqlRender", args="--no-multiarch")
install_github("ohdsi/DatabaseConnector", args="--no-multiarch")
install_github("ohdsi/Clear", args="--no-multiarch")

library(SqlRender)
library(DatabaseConnector)
library(Clear)
connectionDetails<-DatabaseConnector::createConnectionDetails(dbms="sql server",
                                                              server="IP",
                                                              port="PORT",
                                                              schema="SCHEMA",
                                                              user="ID",
                                                              password="PW")
connectionDetails$target_database<-"TARGET_DATABASE_NAME"
connectionDetails$cdm_database<-"CDM_DATABASE_NAME"

targetdrug<-createTargetDrugDataFrame(c("CIPROFLOXACIN"),
                                      c("Anatomical Therapeutic Chemical Classification"),
                                      c("J01MA02","S01AX13","S02AA15","S03AA07"))
targetdrug<-addTargetDrugDataFrame(targetdrug,
                                   c("ROSUVASTATIN"),
                                   c("Anatomical Therapeutic Chemical Classification"),
                                   c("C10AA07"))
labtest<-createLabtestDataFrame(c(3018677,3006923,3013721),
                                c("aPTT","ALT","AST"),
                                c("Both","Hyper","Hyper"))
generateClearDataSet(connectionDetails, drug_list=targetdrug, labtest_list=labtest
                     , date_from="2001-01-01", date_to="2010-03-31")

summary<-getClearResultDataSet(connectionDetails)
head(summary)

matched<-runMatching(connectionDetails)
head(matched)

clogit<-runCLogit(matched)
head(clogit)
```
