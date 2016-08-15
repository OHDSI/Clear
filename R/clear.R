
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

generateClearDataSet<-function(connectionDetails, drug_list=NA, labtest_list=NA
                               , date_from='2001-01-01', date_to='2010-03-31'){
  conn<-DatabaseConnector::connect(connectionDetails)
  
  if(!is.na(drug_list)){
    DatabaseConnector::insertTable(conn, "TARGET_DRUG", drug_list)
  }
  if(!is.na(labtest_list)){
    DatabaseConnector::insertTable(conn, "LABTEST_LIST", labtest_list)
  }
  
  renderedSql<-SqlRender::loadRenderTranslateSql("CLEAR_0.1_CDMv4_Formatted.sql",
                                                 packageName="Clear",
                                                 dbms=connectionDetails$dbms,
                                                 target_database=connectionDetails$target_database,
                                                 cdm_database=connectionDetails$cdm_database,
                                                 date_from=date_from,
                                                 date_to=date_to)
  
  DatabaseConnector::executeSql(conn, renderedSql)
  dbDisconnect(conn)
}

getClearResultDataSet<-function(connectionDetails){
  conn<-DatabaseConnector::connect(connectionDetails)
  
  renderedSql<-SqlRender::loadRenderTranslateSql("CLEAR_0.1_CDMv4_Summary_Formatted.sql",
                                                 packageName="Cert",
                                                 dbms=connectionDetails$dbms,
                                                 target_database=connectionDetails$target_database,
                                                 cdm_database=connectionDetails$cdm_database)
  data<-DatabaseConnector::querySql(conn, renderedSql)
  dbDisconnect(conn)
  data
}


createTargetDrugDataFrame<-function(name,class,code){
  df<-data.frame(DRUG_NAME=c(name),DRUG_CLASS=c(class),DRUG_CODE=c(code))
}

addTargetDrugDataFrame<-function(origin,name,class,code){
  df<-data.frame(DRUG_NAME=c(name),DRUG_CLASS=c(class),DRUG_CODE=c(code))
  rbind(origin,df)
}

createLabtestDataFrame<-function(id,name,type){
  df<-data.frame(LAB_ID=c(id),LAB_NAME=c(name),ABNORM_TYPE=c(type))
}

