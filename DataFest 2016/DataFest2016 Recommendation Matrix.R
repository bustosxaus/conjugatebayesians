library(dplyr)
library(data.table)

purchase_data = data.frame(fread("approved_data_purchase-v5.csv"))

# Filtering out junk events
non_junk = purchase_data %>%
  filter(la_valid_tkt_event_flg == "Y ")

# counting number of purchase events per purchaser
user_purchases = purchase_data %>%
  group_by(purch_party_lkup_id) %>%
  summarise(times_purchased = n()) 

# Making a dataset of only those people who made more than one purchase
unique_multitime_purchases = unique(user_purchases$purch_party_lkup_id[user_purchases$times_purchased>1])

more_than_1_purchase = non_junk %>%
  filter(purch_party_lkup_id %in% unique_multitime_purchases)

# Removing all variables that we do not care about
only_acts = more_than_1_purchase %>%
  select(purch_party_lkup_id, primary_act_name) %>%
  distinct()

# Sorting the data by purchaser id to split with Gonzalo
only_acts = only_acts %>%
  arrange(purch_party_lkup_id)

# Removing people who bought two of the same ticket only
#only_acts_tmp = only_acts %>%
#  group_by(purch_party_lkup_id) %>%
#  summarise(times_purchased = n())

# Number of primary acts
n = length(unique(only_acts$primary_act_name))

# Unique purchase ids
purchase_ids = as.character(unique(only_acts$purch_party_lkup_id))

# Creating the initial matrix for storage
ACTS_TRANSITION = matrix(0, nrow = n, ncol = n)
colnames(ACTS_TRANSITION) = unique(only_acts$primary_act_name)
rownames(ACTS_TRANSITION) = unique(only_acts$primary_act_name)

i = 0

# For loop to build the matrix
for(p in purchase_ids)
{
  #ACTS_TRANSITION_tmp = matrix(0, nrow = n, ncol = n)
  #colnames(ACTS_TRANSITION_tmp) = unique(only_acts$primary_act_name)
  #rownames(ACTS_TRANSITION_tmp) = unique(only_acts$primary_act_name)
  
  sub = only_acts %>%
    filter(purch_party_lkup_id == p) %>%
    #distinct() %>%
    select(primary_act_name) %>%
    mutate(primary_act_name = as.character(primary_act_name)) %>%
    c() #%>%
  #unlist()
  
  #sub_table = table(sub$primary_act_name, sub$primary_act_name)
  # ACTS_TRANSITION_tmp[sub[[1]], sub[[1]]] = 1
  
  #ACTS_TRANSITION = ACTS_TRANSITION + ACTS_TRANSITION_tmp
  
  ACTS_TRANSITION[sub[[1]], sub[[1]]] = ACTS_TRANSITION[sub[[1]], sub[[1]]] + 1
  
  i = i + 1
  if(i %% 100 ==0) print(i)
}

# save(ACTS_TRANSITION, file = "ACTS_TRANSITION.Rdata")



# Getting row proportions
ACTS_TRANSITION_PROP = ACTS_TRANSITION

# Normalizing by columns
col_norm = ACTS_TRANSITION / colSums(ACTS_TRANSITION)

for(i in 1:nrow(ACTS_TRANSITION))
{
  ACTS_TRANSITION_PROP[i, ] = ACTS_TRANSITION[i, ] / sum(ACTS_TRANSITION[i,])
}

# save(ACTS_TRANSITION_PROP, file = "ACTS_TRANSITION_PROP.Rdata")


# Creating dataframe of three recommendations for each artist
recommendations_matrix = matrix(0, nrow = nrow(ACTS_TRANSITION_PROP), ncol = 4)
recommendations_matrix[ , 1] = rownames(ACTS_TRANSITION_PROP)

for(i in 1:nrow(ACTS_TRANSITION_PROP))
{
  # Sorting row by values
  sorted_row = as.data.frame(ACTS_TRANSITION_PROP[i, ])
  names(sorted_row) = "Prop"
  sorted_row = sorted_row %>%
    mutate(artist = rownames(sorted_row))
  sorted_row = sorted_row[-i, ] %>%
    arrange(desc(Prop))
  
  # Getting the top three recommendations and putting them in our matrix
  recommendations_matrix[i, 2:4] = sorted_row[1:3, 2]
  
  #   sorted_row = ACTS_TRANSITION_PROP[i, ]
  #   sorted_row = sorted_row[-i]
  #   sorted_row = as.data.frame(sorted_row) %>%
  #     arrange(desc(sorted_row))
}

recommendations_matrix = as.data.frame(recommendations_matrix)
names(recommendations_matrix) = c("Artist", "Choice 1", "Choice 2", "Choice 3")
