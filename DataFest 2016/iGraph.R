
# Purchase data
# concerts only
library(data.table)
library(dplyr)
library(igraph)
purchase = fread("approved_data_purchase-v5.csv")

sampled = purchase
sampled = filter(sampled, la_event_type_cat == "CONCERTS")
sampled = filter(sampled, venue_city == "ATLANTA")
names = sort(unique(sampled$primary_act_name))


# make id's into integers
sampled$purch_party_lkup_id = as.numeric(factor(sampled$purch_party_lkup_id))
sampled$primary_act_name = as.numeric(factor(sampled$primary_act_name))


# store all tickets bought per person
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
	
	matched = which(pairs[,1] == pairs[,2])

	if (length(matched) > 0){
		pairs = pairs[-matched,]
	}
	return(c(t(pairs)))
}

# get list of all makov pairs
# and if bought more than one ticket
all_pairs = c()
#all_connect_tick = c()
for (i in 1:length(act_list)){
	if (length(act_list[[i]]) > 1){
		edge = act_list[[i]]
		#all_connect_tick = c(all_connect_tick, edge)
		all_pairs = c(all_pairs, markov_pairs(edge))
	}
}

weights = (table(all_pairs))
weights = data.frame(weights)$Freq



# labels of connections
with_connections = unique(all_pairs)
label = sort(with_connections)

# re-label the pairs with integers
all_pairs_new = all_pairs
for (i in 1:length(all_pairs_new)){
	all_pairs_new[i] = which(label == all_pairs[i])
}

#weight_label_new = weight_label
#for (i in 1:length(weight_label)){
#	weight_label_new[i] = which(label == weight_label[i])
#}

# re-label the pairs for order of weights
#all_pairs_new_new = all_pairs_new
#for (i in 1:length(all_pairs_new)){
#	all_pairs_new_new[i] = which(all_pairs_new_new[i] == weight_label_new)
#}

label_names = names[label]

vertex = c(1:length(with_connections))

g = graph.empty(directed = FALSE)
g = g + vertices(vertex, color = "cornflower blue", label.cex = 0.35, 
	size = 3.5, label = c(1:length(vertex)), label.dist = 0,
	label.color = "black", font = 2)
g = g + edges(all_pairs_new, color = "medium grey")

# add special names of atlanta on graph
vertex.attributes(g)$label[70] = label_names[70]
vertex.attributes(g)$label.dist[70] = 0.2
vertex.attributes(g)$label.cex[70] = 0.75

vertex.attributes(g)$label[8] = "Beyonce"
vertex.attributes(g)$label.dist[8] = -0.25
vertex.attributes(g)$label.cex[8] = 0.75

vertex.attributes(g)$label[13] = label_names[13]
vertex.attributes(g)$label.dist[13] = 0.2
vertex.attributes(g)$label.cex[13] = 0.75


vertex.attributes(g)$label[109] = label_names[109]
vertex.attributes(g)$label.dist[109] = -0.25
vertex.attributes(g)$label.cex[109] = 0.7


vertex.attributes(g)$label[39] = label_names[39]
vertex.attributes(g)$label.dist[39] = 0.25
vertex.attributes(g)$label.cex[39] = 0.7

# make pdf of graph
pdf("Graph.pdf", width = 8, height = 8)
plot(g, 
	layout = layout.reingold.tilford(g, circular = T))
dev.off()



# names[label]

# GRAPH 2: circle type graph

#vertex = c(1:length(with_connections))

#g = graph.empty(directed = FALSE)
#g = g + vertices(vertex, color = "light blue", label.cex = 0.3)
#g = g + edges(all_pairs_new_new, color = "grey")


#pdf("Graph2.pdf", width = 8, height = 8)
#plot(g, vertex.size = 5, 
#	#layout = layout.reingold.tilford(g, circular = T),
#	layout = layout.circle,
#    vertex.label = c(1:length(vertex)),
#    vertex.label.color = "black", 
#	edge.width = 0.5)
#title("Network of Atlanta Concert Tickets", cex.main = 1.5)
#dev.off()

