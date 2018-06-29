AnnualReports_RK <- function(symbol,Req_Start_Date,Req_End_Date,type) {
  
  options(stringsAsFactors = FALSE)
  url <- paste0("http://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=", 
                  symbol, "&type=",type,"&dateb=&owner=exclude&count=100")
 
  filings <- xml2::read_html(url)
  
  ##   Generic function to extract info
  ExtractInfo <- function(html.node) {
    info <-
      filings %>%
      rvest::html_nodes(html.node) %>%
      rvest::html_text()
    return(info)
  }
  
  ##   Acquire filing name
  filing.name <- ExtractInfo("#seriesDiv td:nth-child(1)")
  
  ##   Error message for function
  if(length(filing.name) == 0) {
    stop("invalid company symbol or foreign logical")
  }
  
  ##   Acquire filing date
  filing.date <- ExtractInfo(".small+ td")
  
  ##   Acquire accession number
  accession.no.raw <- ExtractInfo(".small")
  
  accession.no <-
    gsub("^.*Acc-no: ", "", accession.no.raw) %>%
    substr(1, 20)
  
  ##   Create dataframe
  info.df <- data.frame(filing.name = filing.name, filing.date = filing.date, 
                        accession.no = accession.no)
  
  info.df$filing.date<-as.Date(info.df$filing.date)
  info.df<-info.df[info.df$filing.date>=Req_Start_Date&info.df$filing.date<=Req_End_Date,]
  return(info.df)
  
}



################################################
#####Function GetFinancial
################################################
GetFinancial_RK <- function(statement.type, symbol,accession.no.raw) {
  
  ##   This is here to please R CMD check
  description <- NULL
  roleId <- NULL
  labelRole <- NULL
  labelString <- NULL
  unitId <- NULL
  fact <- NULL
  contextId <- NULL
  startDate <- NULL
  endDate <- NULL
  
  accession.no= gsub("-", "" , accession.no.raw)
  CIK <- CompanyInfo(symbol)
  CIK <- as.numeric(CIK$CIK)
  lower.symbol=tolower(symbol)
  report.period <- ReportPeriod_RK(symbol, CIK, accession.no, accession.no.raw)
  rep_per<-report.period
  report.period<-gsub("-", "" , report.period)
  inst.url <- paste0("https://www.sec.gov/Archives/edgar/data/", CIK, "/", 
                       accession.no, "/", lower.symbol, "-", report.period, ".xml")

  
  ##   Function to download Instance Document
  GetInstFile <- function(url) {
    XBRL::xbrlDoAll(url, cache.dir="xbrl.Cache", prefix.out ="out", verbose=FALSE, delete.cached.inst = TRUE)
  }
  
  ##   Check if url exits
  
  check <- tryCatch(is.list(httr::GET(inst.url)), error = function(e) {return(FALSE)})
  if(check == FALSE) {
    stop("no XBRL-format filings detected")
  }
  
  ##   Download Instance Document
  instFile <- GetInstFile(inst.url)
  
  ##   Clear Cache Dir
  file.remove("out_calculations.csv", "out_contexts.csv", "out_definitions.csv", 
              "out_elements.csv", "out_facts.csv", "out_footnotes.csv", 
              "out_labels.csv", "out_presentations.csv", "out_roles.csv", "out_units.csv")
  
  unlink("XBRLcache", recursive = TRUE)
  
  ##   Get Role ID from Instance Document
  role.df <- instFile$role %>%
    filter(toupper(description) %in% statement.type)
  
  role.id <- as.character(role.df$roleId)
  
  ##   Create statement template from Presentation Linkbase
  statement.skeleton <-
    instFile$presentation %>%
    filter(roleId == role.id)
  
  rowid <- c(1:nrow(statement.skeleton))
  statement.skeleton <- mutate(statement.skeleton, rowid = rowid)
  
  ##   Merge with Label Linkbase
  statement <-
    merge(statement.skeleton, instFile$label, by.x = "toElementId", 
          by.y = "elementId") %>%
    filter(labelRole == "http://www.xbrl.org/2003/role/label")
  
  ##   Merge with Fact Linkbase
  statement <- merge(statement, instFile$fact, by.x = "toElementId", 
                     by.y = "elementId")
  
  ##   Merge with Context Linkbase
  statement <- merge(statement, instFile$context, by.x = "contextId", 
                     by.y = "contextId") %>%
  arrange(rowid)
  
  ##Subsetting Required data
  statement$endDate<-as.Date(statement$endDate)
  statement$startDate<-as.Date(statement$startDate)
  statement<-statement[statement$endDate==rep_per,]
  
  
  ##   Clean combined table
  statement <- subset(statement, is.na(statement$dimension1))
  
  clean.statement <- select(statement, labelString, unitId, fact, contextId, 
                            startDate, endDate, rowid)
  clean.statement <- select(clean.statement, -contextId)
  
  colnames(clean.statement)[1] <- "Metric"
  colnames(clean.statement)[2] <- "Units"
  colnames(clean.statement)[3] <- "Amount"
  
  clean.statement <- arrange(clean.statement, rowid)
  clean.statement <- select(clean.statement, -rowid)
  return(clean.statement)
}



ReportPeriod_RK <- function(symbol, CIK, accession.no, accession.no.raw) {
  
  url <- paste0("https://www.sec.gov/Archives/edgar/data/", CIK, "/", 
                accession.no, "/", accession.no.raw, "-index.htm")
  search.result <- xml2::read_html(url)
  
  ##   Generic function to extract info
  ExtractInfo <- function(html.node) {
    info <-
      search.result %>%
      rvest::html_nodes(html.node) %>%
      rvest::html_text()
    return(info)
  }
  
  report.period <- ExtractInfo(".formGrouping+ .formGrouping .info:nth-child(2)")
  return(report.period)
}

################################################
#####Function GetIncome
################################################
GetIncome_RK <- function(income.descriptions,symbol, accession.no.raw) {
  
  GetFinancial_RK(income.descriptions, symbol,accession.no.raw)

}


################################################
#####Function GetCashflow
################################################
GetCashFlow_RK <- function(cash.flow.descriptions,symbol,accession.no.raw) {
  
  GetFinancial_RK(cash.flow.descriptions, symbol,accession.no.raw)

}


################################################
#####Function GetBalanceSheet
################################################
GetBalanceSheet_RK <- function(balance.sheet.descriptions,symbol,accession.no.raw) {
  
  GetFinancial_RK(balance.sheet.descriptions, symbol, accession.no.raw)
  
}














