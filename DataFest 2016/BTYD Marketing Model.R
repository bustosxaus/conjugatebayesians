library(data.table)
library(dplyr)
library(BTYD)

source("/home/grad/sh360/DataFest2016/PNBD_Fix.R")

purchase_data <- data.frame(fread("/home/grad/sh360/data-purchases.csv"))


event_log <- purchase_data[,c("purch_party_lkup_id", "sales_ord_tran_dt", "tickets_purchased_qty")] %>%
  arrange(sales_ord_tran_dt)

colnames(event_log) <- c("cust", "date", "sales")
event_log2 <- dc.MergeTransactionsOnSameDate(event_log)
elog.full <- event_log2

end.of.cal.period <- as.Date("2012-01-01")
elog.cal <- event_log2[which(event_log2[,2] <= end.of.cal.period),]
elog.test <- event_log2[which(event_log2[,2] > end.of.cal.period),]


elog.cal.final <- group_by(elog.full, cust) %>%
  summarize("x" = length(sales)-1, 
            "t.x" = as.numeric(difftime(max(date),min(date), units = "weeks"))/4, 
            "T.cal" = as.numeric(difftime("2016-03-09", min(date), units = "weeks"))/4)

elog2 <- data.frame(elog.cal.final)

rownames(elog2) <- elog2[,1]
elog2 <- elog2[,-1]

params <- bgnbd.EstimateParameters(elog2)
params2 <- params
params3 <- params

x <- elog2[,"x"]
t.x <- elog2[,"t.x"]
T.cal <- elog2[,"T.cal"]

E.x <- bgnbd.ConditionalExpectedTransactions(params, T.star = 6, x, t.x, T.cal)

Cond.Exp <- data.frame(E.x)
cust.names <- cbind("cust" = rownames(elog2), Cond.Exp)
rownames(Cond.Exp) <- cust.names

elog.cal.mean <- group_by(event_log2, cust) %>% 
  summarize("Tickets_Mean" = mean(sales))

elog.cal.mean <- data.frame(elog.cal.mean)

final.elog <- inner_join(elog.cal.mean, cust.names) 

final.elog2 <- mutate(final.elog, "Expected_Tickets" = Tickets_Mean*E.x) %>%
  arrange(desc(Expected_Tickets))





elog.test[which(elog.test[,1] == "aa4929389254f09c4014"),]

final.elog2[sample(1:2000,3),]

write.csv(final.elog2, file = "/home/grad/sh360/Expected_Transactions.csv")
final.elog2[47800:47809,]
