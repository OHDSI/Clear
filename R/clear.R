
#' Test rendering query
#' 
#' @details
#' for testing
#' 
#' @return
#' a query form string
#' 
#' @export
renderTest<-function(){
  SqlRender::loadRenderTranslateSql("temp.sql",
                                    packageName="Clear",
                                    dbms="sql server")
}

#' Test query database
#' 
#' @details
#' for testing
#' 
#' @param connectionDetails
#' connectionDetails information
#' 
#' @return
#' a data frame
#' 
#' @export
queryTest<-function(connectionDetails){
  conn<-DatabaseConnector::connect(connectionDetails)
  
  data<-DatabaseConnector::querySql(conn, "select top 10 * from [OMOP_5].[dbo].CONCEPT;")
  dbDisconnect(conn)
  data
}

#' Generate Clear dataset
#' 
#' @details
#' initial testing
#' 
#' @return
#' nothing
#' 
#' @export
generateClearDataSet<-function(connectionDetails, drug_list=NA, labtest_list=NA
                               , date_from='2001-01-01', date_to='2010-03-31'){
  conn<-DatabaseConnector::connect(connectionDetails)
  
  if(!is.na(drug_list)){
    DatabaseConnector::insertTable(conn, "TARGET_DRUG", drug_list)
  }
  if(!is.na(labtest_list)){
    DatabaseConnector::insertTable(conn, "LABTEST_LIST", labtest_list)
  }
  
  renderedSql<-SqlRender::loadRenderTranslateSql("CLEAR_0.2_CDMv4_Formatted.sql",
                                                 packageName="Clear",
                                                 dbms=connectionDetails$dbms,
                                                 target_database=connectionDetails$target_database,
                                                 cdm_database=connectionDetails$cdm_database,
                                                 date_from=date_from,
                                                 date_to=date_to)
  
  DatabaseConnector::executeSql(conn, renderedSql)
  dbDisconnect(conn)
}

#' Get Clear result dataset
#' 
#' @details
#' initial testing
#' 
#' @return
#' a data frame from summary table
#' 
#' @export
getClearResultDataSet<-function(connectionDetails){
  conn<-DatabaseConnector::connect(connectionDetails)
  
  renderedSql<-SqlRender::loadRenderTranslateSql("CLEAR_0.2_CDMv4_Summary_Formatted.sql",
                                                 packageName="Clear",
                                                 dbms=connectionDetails$dbms,
                                                 target_database=connectionDetails$target_database,
                                                 cdm_database=connectionDetails$cdm_database)
  data<-DatabaseConnector::querySql(conn, renderedSql)
  dbDisconnect(conn)
  data
}

#' Get Clear result dataset for matching
#' 
#' @details
#' initial testing
#' 
#' @return
#' a data frame from summary table for matching
#' 
#' @export
getDataForMatching<-function(connectionDetails){
  conn<-DatabaseConnector::connect(connectionDetails)
  
  renderedSql<-SqlRender::loadRenderTranslateSql("CLEAR_0.2_CDMv4_Match_Formatted.sql",
                                                 packageName="Clear",
                                                 dbms=connectionDetails$dbms,
                                                 target_database=connectionDetails$target_database,
                                                 cdm_database=connectionDetails$cdm_database)
  data<-DatabaseConnector::querySql(conn, renderedSql)
  dbDisconnect(conn)
  data
}

#' Run exact 1:4 matching
#' 
#' @details
#' initial testing
#' 
#' @return
#' a matched data frame
#' 
#' @export
runMatching<-function(connectionDetails){
  match<-getDataForMatching(connectionDetails)
  
  match[is.na(match)]<-"NA"
  matched.data<-plyr::ddply(match[!(match$DRUG_NAME%in%("NA")),], .(DRUG_NAME), function(x){
    x<-rbind(x,match[match$DRUG_NAME%in%("NA"),])

    plyr::ddply(x, .(LAB_NAME,RESULT_TYPE), function(x){
      matched<-MatchIt::matchit(!(DRUG_NAME%in%c("NA")) ~ as.numeric(AGE) + as.factor(SEX)
                                , distance="logit", method="nearest", caliper=0.1, ratio=4
                                , data=x)
      cbind(IDX_DRUG=x[!(x$DRUG_NAME%in%c("NA")),c("DRUG_NAME")][1]
            , match.data(matched))
    })
  })
}

#' Run exact 1:4 matching with data
#' 
#' @details
#' initial testing
#' 
#' @return
#' a matched data frame
#' 
#' @export
runMatchingWithData<-function(match){
  match[is.na(match)]<-"NA"
  matched.data<-plyr::ddply(match[!(match$DRUG_NAME%in%("NA")),], .(DRUG_NAME), function(x){
    x<-rbind(x,match[match$DRUG_NAME%in%("NA"),])
    
    plyr::ddply(x, .(LAB_NAME,RESULT_TYPE), function(x){
      matched<-MatchIt::matchit(!(DRUG_NAME%in%c("NA")) ~ as.numeric(AGE) + as.factor(SEX)
                                , distance="logit", method="nearest", caliper=0.1, ratio=4
                                , data=x)
      cbind(IDX_DRUG=x[!(x$DRUG_NAME%in%c("NA")),c("DRUG_NAME")][1]
            , match.data(matched))
    })
  })
}

#' Run Conditional Logistic regression
#' 
#' @details
#' initial testing
#' 
#' @return
#' a outcome of regression
#' 
#' @export
runCLogit<-function(matched.data){
  plyr::ddply(matched.data, .(IDX_DRUG), function(x){
    x$DRUG_NAME<-factor(x$DRUG_NAME)
    x$DRUG_NAME<-relevel(x$DRUG_NAME, "NA")
    plyr::ddply(x, .(LAB_NAME,RESULT_TYPE), function(x){
      broom::tidy(clog<-survival::clogit(JUDGE%in%c("NORMAL")~DRUG_NAME+strata(distance), data=x))
    })
  })
}

#' Create data frame for set a target drug
#' 
#' @details
#' initial testing
#' 
#' @return
#' nothing
#' 
#' @export
createTargetDrugDataFrame<-function(name,class,code){
  df<-data.frame(DRUG_NAME=c(name),DRUG_CLASS=c(class),DRUG_CODE=c(code))
}

#' Add into TargetDrugDataFrame
#' 
#' @details
#' initial testing
#' 
#' @return
#' nothing
#' 
#' @export
addTargetDrugDataFrame<-function(origin,name,class,code){
  df<-data.frame(DRUG_NAME=c(name),DRUG_CLASS=c(class),DRUG_CODE=c(code))
  rbind(origin,df)
}

#' Create data frame for set a target labtest
#' 
#' @details
#' initial testing
#' 
#' @return
#' nothing
#' 
#' @export
createLabtestDataFrame<-function(id,name,type){
  df<-data.frame(LAB_ID=c(id),LAB_NAME=c(name),ABNORM_TYPE=c(type))
}

