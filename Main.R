###Clearing the Environment
rm(list=ls())
library(finreportr)
library(dplyr)
library(magrittr)
library(xlsx)
setwd("K:/HE Project - FAMA/Scraping Code")
source('Source Functions.R')
winDialog("Please enter the Ticker and Year information",type=c('ok'))
symbol=winDialogString("Company Ticker:",default="")
year=as.numeric(winDialogString("Year:",default=""))
foreign=FALSE
Req_Start_Date=paste0(year,"-04-01")
Req_End_Date=paste0(year+1,"-03-31")
Req_Start_Date<-as.Date(Req_Start_Date)
Req_End_Date<-as.Date(Req_End_Date)
info_df=CompanyInfo(symbol)
Reports_Qtr=AnnualReports_RK(symbol,Req_Start_Date,Req_End_Date,"10-Q")
Reports_Year=AnnualReports_RK(symbol,Req_Start_Date,Req_End_Date,"10-K")
if(nrow(Reports_Qtr)>0&nrow(Reports_Year)>0)
{
  Reports=rbind(Reports_Qtr,Reports_Year)
}else{
      if(nrow(Reports_Qtr)==0)
      {
       Reports=Reports_Year
      }else{
            if(nrow(Reports_Year)==0)
            {
             Reports=Reports_Qtr
            }
           }
    }
Reports=Reports[Reports$filing.name%in%c("10-Q","10-K"),]

if(nrow(Reports)!=0)
{
income.descriptions <- c("CONSOLIDATED STATEMENTS OF INCOME",
                         "CONSOLIDATED STATEMENTS OF INCOME (UNAUDITED)",
                         "CONSOLIDATED STATEMENT OF INCOME", 
                         "CONSOLIDATED STATEMENTS OF OPERATIONS", 
                         "CONSOLIDATED STATEMENT OF OPERATIONS", 
                         "CONSOLIDATED STATEMENT OF EARNINGS", 
                         "CONSOLIDATED STATEMENTS OF EARNINGS",
                         "INCOME STATEMENTS", 
                         "CONSOLIDATED RESULTS OF OPERATIONS",
                         "CONDENSED CONSOLIDATED STATEMENTS OF INCOME",
                         "CONDENSED CONSOLIDATED STATEMENTS OF INCOME AND COMPREHENSIVE INCOME (UNAUDITED)",
                         "CONDENSED CONSOLIDATED STATEMENTS OF OPERATIONS (UNAUDITED)",
                         "CONSOLIDATED STATEMENTS OF OPERATIONS (UNAUDITED)",
                         "CONDENSED CONSOLIDATED STATEMENTS OF OPERATIONS",
                         "UNAUDITED CONSOLIDATED STATEMENT OF OPERATIONS",
                         "CONDENSED CONSOLIDATED INCOME STATEMENTS",
                         "CONDENSED CONSOLIDATED STATEMENT OF INCOME",
                         "CONDENSED CONSOLIDATED STATEMENTS OF EARNINGS (UNAUDITED)")

cash.flow.descriptions <- c("CONSOLIDATED STATEMENT OF CASH FLOWS", 
                            "CONSOLIDATED STATEMENTS OF CASH FLOWS",
                            "CASH FLOWS STATEMENTS",
                            "CONSOLIDATED STATEMENT OF CASH FLOW",
                            "CONDENSED CONSOLIDATED STATEMENTS OF CASH FLOWS (UNAUDITED)",
                            "CONDENSED CONSOLIDATED STATEMENTS OF CASH FLOWS",
                            "CONSOLIDATED STATEMENTS OF CASH FLOWS (UNAUDITED)",
                            "UNAUDITED CONSOLIDATED STATEMENT OF CASH FLOWS",
                            "CONDENSED CONSOLIDATED STATEMENT OF CASH FLOWS")


balance.sheet.descriptions <- c("CONSOLIDATED BALANCE SHEET", 
                                "CONSOLIDATED BALANCE SHEETS", 
                                "CONSOLIDATED STATEMENT OF FINANCIAL POSITION", 
                                "CONSOLIDATED STATEMENTS OF FINANCIAL POSITION",
                                "BALANCE SHEETS",
                                "CONSOLIDATED FINANCIAL POSITION",
                                "CONDENSED CONSOLIDATED BALANCE SHEETS",
                                "CONDENSED CONSOLIDATED BALANCE SHEETS (UNAUDITED)",
                                "CONSOLIDATED BALANCE SHEETS (UNAUDITED)",
                                "UNAUDITED CONSOLIDATED BALANCE SHEET",
                                "CONDENSED CONSOLIDATED BALANCE SHEET")


#Income Statement Extraction
for(i in 1:nrow(Reports)){
  accession.no.raw=Reports$accession.no[i]
  df=GetIncome_RK(income.descriptions,symbol,accession.no.raw)
  if(i==1){
    Combined_Income_Statements=df
  }else{
    Combined_Income_Statements=rbind(Combined_Income_Statements,df)
  }
  print(i)
}


#Balance Statement Extraction
for(i in 1:nrow(Reports)){
  accession.no.raw=Reports$accession.no[i]
  df=GetBalanceSheet_RK(balance.sheet.descriptions,symbol,accession.no.raw)
  if(i==1){
    Combined_Balance_Statements=df
  }else{
    Combined_Balance_Statements=rbind(Combined_Balance_Statements,df)
  }
  print(i)
}

#Cashflow Statement Extraction
for(i in 1:nrow(Reports)){
  accession.no.raw=Reports$accession.no[i]
  df=GetCashFlow_RK(cash.flow.descriptions,symbol,accession.no.raw)
  if(i==1){
    Combined_CashFlow_Statements=df
  }else{
    Combined_CashFlow_Statements=rbind(Combined_CashFlow_Statements,df)
  }
  print(i)
}

Combined_Balance_Statements$Amount<-as.numeric(Combined_Balance_Statements$Amount)
write.xlsx(Combined_Balance_Statements,paste0("Output/",toupper(symbol),"_Financial_Statements.xls"),sheetName = "Balance_Sheet",row.names=FALSE,showNA = F)

Combined_Income_Statements$Amount<-as.numeric(Combined_Income_Statements$Amount)
write.xlsx(Combined_Income_Statements,paste0("Output/",toupper(symbol),"_Financial_Statements.xls"),sheetName = "Income_Statement",row.names=FALSE,showNA = F,append = T)

Combined_CashFlow_Statements$Amount<-as.numeric(Combined_CashFlow_Statements$Amount)
write.xlsx(Combined_CashFlow_Statements,paste0("Output/",toupper(symbol),"_Financial_Statements.xls"),sheetName = "Cash_Flows_Statement",row.names=FALSE,showNA = F,append=T)
}else{
  
  winDialog("No Reports found for given Ticker, Please Check",type=c('ok'))

}
