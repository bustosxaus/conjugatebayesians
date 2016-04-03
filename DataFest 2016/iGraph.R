
# Purchase data
# concerts only
library(data.table)
library(dplyr)
library(igraph)

sampled = purchase_data
#sampled = sample_n(purchase, 100000)
#sampled = filter(sampled, la_event_type_cat == "CONCERTS")
sampled = filter(sampled, venue_city == "NEW YORK")



# make id's into integers
sampled$purch_party_lkup_id = as.numeric(factor(sampled$purch_party_lkup_id))

names = sort(unique(sampled$primary_act_name))
sampled$primary_act_name = as.numeric(factor(sampled$primary_act_name))



# store if more than one ticket was bought
act_list = list()
for (i in 1:max(sampled$purch_party_lkup_id )){
  act = sampled$primary_act_name[which(sampled$purch_party_lkup_id  == i)]
  act_list[[i]] = act
}


# function to get edges of markov pairs
markov_pairs = function(thing){
  n = length(thing)
  pairs = matrix(nrow = n-1, ncol = 2)
  for (i in 1:(n-1)){
    pairs[i,] = c(thing[i], thing[i+1])
  }
  
  nopairs = pairs[-(which(pairs[,1] == pairs[,2])),]
  return(c(t(nopairs)))
}

# get list of all makov pairs
all_pairs = c()
for (i in 1:length(act_list)){
  if (length(act_list[[i]]) > 1){
    edges = act_list[[i]]
    all_pairs = c(all_pairs, markov_pairs(edges))
    if (i%%1000 == 0){print(i)}
  }
}


vertex = unique(sampled$primary_act_name)
with_connections = unique(all_pairs)

label = sort(with_connections)
all_pairs_new = all_pairs

for (i in 1:length(all_pairs_new)){
  all_pairs_new[i] = which(label == all_pairs[i])
}


vertex = c(1:length(with_connections))

names[which(names == "Beyonc\xe9")] = "Beyonce"
names[which(names == "Man\xe1")] = "Man"
names[which(names == "La Arrolladora Banda Lim\xf3n")] = "La Arrolandora Banda Limon"

radian.rescale <- function(x, start=0, direction=1) {
  c.rotate <- function(x) (x + start) %% (2 * pi) * direction
  c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
}

lab.locs = radian.rescale(1:length(with_connections), -1, 0)
g = graph.empty(directed = F)
g = g + vertices(vertex, color = "black", label.cex = 0.5, label.dist = 0.1, label.degree = -pi/2)
g = g + edges(all_pairs_new, color = "blue")
layout <- layout.reingold.tilford(g, circular=T)

plot(g, vertex.size = 2.5, layout = layout, vertex.label = names[label], vertex.label.color = 'red', 
     edge.width = 0.5)


#plot in order of most connections




