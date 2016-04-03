library(data.table)
library(dplyr)
library(BTYD)

purchase_data <- data.frame(fread("/Users/Sanjay/Desktop/DataFest 2016/Data and Codebook/approved_data_purchase-v5.csv"))
ga_data <- data.frame(fread("/Users/Sanjay/Desktop/DataFest 2016/Data and Codebook/approved_ga_data_v2.csv"))
adwords_data <- data.frame(fread("/Users/Sanjay/Desktop/DataFest 2016/Data and Codebook/approved_adwords_v3.csv"))

event_log <- purchase_data[,c("purch_party_lkup_id", "sales_ord_tran_dt", "tickets_purchased_qty")] %>%
            arrange(sales_ord_tran_dt)

colnames(event_log) <- c("cust", "date", "sales")


event_log2 <- dc.MergeTransactionsOnSameDate(event_log)

end.of.cal.period <- as.Date("2012-01-01")
elog.cal <- event_log2[which(event_log2[,2] <= end.of.cal.period),]



elog.cal.final <- group_by(elog.cal[1:100,], cust) %>%
                  summarize("x" = sum(sales), 
                            "t.x" = as.numeric(difftime(max(date),min(date), units = "weeks")), 
                            "T.cal" = as.numeric(difftime("2012-01-01", min(date))))

elog.cal.final <- data.frame(elog.cal.final)

rownames(elog.cal.final) <- elog.cal.final[,1]
elog.cal.final <- elog.cal.final[,1]



Trans_Mat <- group_by(purchase_data[,c("purch_party_lkup_id","primary_act_name")], 
                      purch_party_lkup_id, primary_act_name)  %>%
            distinct() %>%
            data.frame()

Trans_Mat2 <- group_by(purchase_data[1:1000,c("purch_party_lkup_id","primary_act_name")], 
                       purch_party_lkup_id)  %>%
  distinct() %>%
  data.frame()

n <- length(unique(purchase_data[,"primary_act_name"]))
ACTS_TRANSITION <- matrix(0, nrow = n, ncol = n)
colnames(ACTS_TRANSITION) <- unique(purchase_data[,"primary_act_name"])
rownames(ACTS_TRANSITION) <- unique(purchase_data[,"primary_act_name"])


Trans_Mat3 <- Trans_Mat[c(280, 281, 282, 368),]

Trans_Mat4 <- group_by(Trans_Mat3) %>%
              
  
Sum_Mat <- group_by(purch_party_lkup_id) %>%
          summarize(length(primary_act_name))


sampled <- filter(purchase_data, la_event_type_cat == "CONCERTS") %>%
            filter(venue_city == "NEW YORK")


act_names <- factor(sampled$primary_act_name)

radian.rescale <- function(x, start=0, direction=1) {
  c.rotate <- function(x) (x + start) %% (2 * pi) * direction
  c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
}


load("/Users/Sanjay/Desktop/DataFest 2016/ACTS_TRANSITION_PROP.rdata")

Purchase_User <- "f8e5fe21dd7a47b5b745"

Top_Three_Artists <- function(Purchase_User){

    First_Act <- purchase_data[which(purchase_data[,"purch_party_lkup_id"] == Purchase_User),"primary_act_name"]
    Weights <- table(First_Act)/length(First_Act)
    
    Final_List <- c()

    for (i in 1:length(Weights)){
      Artist <- rownames(Weights)[i]
      Artist_Weight <- Weights[i]
      
      Top <- sort(ACTS_TRANSITION_PROP[Artist,], decreasing = TRUE)
      
      
      Final_List <- c(Final_List, Top*Artist_Weight)
      
    }
    
    if ("Beyonc\xe9" %in% rownames(Weights)){
      rownames(Weights)[which(rownames(Weights) == "Beyonc\xe9")] = "Beyonc<e9>"
    }
    
    Final_List <- Final_List[-which(names(Final_List) %in% rownames(Weights))]
    
    Top_Three <- head(unique(names(sort(Final_List, decreasing = TRUE))),3)
    
    return(Top_Three)
    
}

