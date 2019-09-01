# Jake Flancer
# Rap Network
# June 18, 2018

library(tidyverse)
# library(rvest)
# library(geniusr)
library(ggraph)
library(igraph)
library(tidygraph)

# Gets HTML into a vector
# wiki.solo <- read_html("https://en.wikipedia.org/wiki/List_of_hip_hop_musicians") %>%
#   html_nodes("#mw-content-text :nth-child(1)") %>%
#   html_text()
# 
# # Gets lines with rappers
# wiki.solo <- wiki.solo[41:1648]
# 
# # Clean the data
# wiki.solo.list <- lapply(wiki.solo, FUN = strsplit, split = "[\r\n]")
# wiki.solo.list <- lapply(wiki.solo.list, FUN = unlist)
# wiki.test <- unlist(wiki.solo.list)
# 
# # Removes annotations and headers
# wiki.clean <- wiki.test[substr(wiki.test,1,1) %in% c(letters,LETTERS,'1','2','3','4','5','6','7','8','9')]
# wiki.clean <- wiki.clean[!(wiki.clean %in% LETTERS)]
# wiki.clean <- unique(wiki.clean)
# 
# # Now repeating to get group
# wiki.groups <- read_html("https://en.wikipedia.org/wiki/List_of_hip_hop_groups") %>%
#   html_nodes("#mw-content-text :nth-child(1)") %>%
#   html_text()
# wiki.groups <- wiki.groups[41:796]
# wiki.groups.list <- lapply(wiki.groups, FUN = strsplit, split = "[\r\n]")
# wiki.groups.list <- lapply(wiki.groups.list, FUN = unlist)
# wiki.test.2 <- unlist(wiki.groups.list)
# 
# wiki.clean <- c(wiki.test, wiki.test.2)
# 
# wiki.clean <- wiki.clean[substr(wiki.clean,1,1) %in% c(letters,LETTERS,'1','2','3','4','5','6','7','8','9')]
# wiki.clean <- wiki.clean[!(wiki.clean %in% LETTERS)]
# wiki.clean <- unique(wiki.clean)
# wiki.clean <- sapply(strsplit(wiki.clean, "\\["), FUN = function(x){x[1]})
# 

# Acess Token: ulC2q7eNhlAnuodOFrBsoa1PzHw1fKo5IXX8STrW3mXfeIdfnnwvHsQUSGOCWYWr
# genius_token(force = T)

# search_artist(wiki.clean[1])[1,1]

# This will take a little while
# artist_ids <- sapply(wiki.clean, 
#                      FUN = function(x){
#                        tryCatch(as.character(search_artist(x)[1,1]),
#                                 error = function(e){NA})  
#                      })
# 
# wiki.data <- data.frame(artist_name = as.character(wiki.clean),
#                         artist_id = artist_ids,
#                         stringsAsFactors = F)
# wiki.data <- wiki.data[-268,]
# 
# wiki.data$artist_id <- as.character(unname(wiki.data$artist_id))
# 
# #Will update as I find errors- so far these are the only two
# diff_names <- data.frame(artist_name = c("Jeezy","Ty Dolla $ign"),
#                          artist_id = c(67,42))
# 
# wiki.data <- rbind(wiki.data, diff_names)
# 
# #Writes to file
# write_csv(wiki.data, "~/Documents/R/rap_project/artist_info.csv")
# 
# wiki.data <- read_csv("~/Documents/R/rap_project/artist_info.csv")
# 
# #Formats the artists to correct for different punctuations and spellings
# feature_artists <- wiki.data$artist_name
# feature_artists <- toupper(gsub("[^[:alnum:] ]|\\s","",feature_artists))
# 
# # Takes in artist id and return vector of all feature artists
# get_features <- function(x){
#   song_data <- tryCatch(get_artist_songs(x, include_features = F), error = function(e){NA})
#   if(is.data.frame(song_data)){
#   splits <- strsplit(song_data$song_name, split = "[(]Ft[.][[:space:]]")
#   features <- sapply(splits, FUN = function(x){x[2]})
#   features.split <- unlist(strsplit(features, split = "\\, |\\&"))
#   final <- trimws(gsub("[)]","",features.split), which = "both")
#   final <- toupper(gsub("[^[:alnum:] ]|\\s","",final[!is.na(final)]))
#   final <- final[final %in% feature_artists]
#   return(final)} else {
#     return(NA)
#   }
# }

# a <- Sys.time()
# prev <- readRDS("~/Documents/R/rap_project/features.RData")
# list_of_features <- lapply(wiki.data$artist_id[1851:1851], FUN = get_features)
# names(list_of_features) <- wiki.data$artist_name[1851:1851]
# saveRDS(c(prev,list_of_features), "~/Documents/R/rap_project/features.RData")
# Sys.time() - a

#=========================
# Cleaning/Fixing Data
# features <- readRDS("~/Documents/R/rap_project/features.RData")
# corrections <- read_csv("~/Documents/R/rap_project/corrections.csv")[,1:2]
# 
# # Remove duplicates
# j <- which(duplicated(toupper(gsub("[^[:alnum:] ]|\\s","",names(features)))) == T)
# features.clean <- features[-j]
# 
# # Find typos in corrections artist names
# b <- toupper(gsub("[^[:alnum:] ]|\\s","",names(features.clean)))
# corrections$artist_cleaned <- toupper(gsub("[^[:alnum:] ]|\\s","",corrections$artist_name))
# corrections$feature_ind <- match(corrections$artist_cleaned, b)
# 
# # Remove errors from original entries
# features.clean <- features.clean[-corrections$feature_ind]
# 
# # Rescrape features with correct ids
# correction_features <- lapply(corrections$artist_id, FUN = get_features)
# names(correction_features) <- corrections$artist_name
# 
# features_final <- c(features.clean, correction_features)
# #features_final <- features_final[-1802] #Scarface missed
# 
# write_rds(features_final, "~/Documents/R/rap_project/final_features.RData")

# Converts to data frame
set.seed(1234)
features_final <- read_rds("~/Documents/R/rap_project/final_features.RData")

all.data <- stack(features_final)

renamed.data <- all.data
renamed.data$ind <- toupper(gsub("[^[:alnum:] ]|\\s","",renamed.data$ind))
colnames(renamed.data) <- c("Feature","Primary")

all_artists <- unique(c(renamed.data$Feature, renamed.data$Primary))


#Network Analysis

per_route <- renamed.data %>%
  count(Feature,Primary)

#NODES

nodes <- data_frame(all_artists) %>%
  rowid_to_column("id")

nodes$from_test <- nodes$all_artists %in% renamed.data$Primary

nodes$to_test <- nodes$all_artists %in% renamed.data$Feature
nodes$includes <- nodes$from_test + nodes$to_test

# Use only artists who have featured and have a feature
nodes2 <- nodes[which(nodes$includes == 2),] %>%
  rename('label' = 'all_artists') %>%
  select(id,label)

#Labels
cleaned_names <- data_frame(clean = names(features_final),
                            label = as.character(toupper(gsub("[^[:alnum:] ]|\\s","",names(features_final)))))
#cleaned_names <- cleaned_names[-(1:1903*duplicated(cleaned_names$label)),]

nodes3 <- nodes2 %>%
  left_join(cleaned_names) %>%
  select(id, clean) %>%
  rename(label = clean)

#EDGES

edges <- per_route %>% 
  left_join(nodes2, by = c("Primary" = "label")) %>%
  rename(from = id)
edges <- edges %>% 
  left_join(nodes2, by = c("Feature" = "label")) %>% 
  rename(to = id)

edges <- select(edges, from, to, n) %>%
  rename('weight' = 'n') %>%
  na.omit()
edges <- edges[which(edges$from != edges$to),]


edges2 <- filter(edges)
  
nodes4 <- nodes3[which(nodes3$id %in% c(edges2$from,edges2$to)),]

nodes4 <- nodes4 %>% rowid_to_column("newid")
new_ids <- select(nodes4, newid,id)
edges3 <- edges2 %>%
  left_join(new_ids, by = c("from" = "id")) %>%
  select(-from) %>%
  rename("from" = "newid")
edges3 <- edges3 %>%
  left_join(new_ids, by = c("to" = "id")) %>%
  select(-to) %>%
  rename("to" = "newid")
nodes4 <- select(nodes4, newid, label)

# routes_igraph <- graph_from_data_frame(edges, nodes3, directed = TRUE)
# plot(routes_igraph, 
#      layout = layout_with_graphopt,
#      edge.arrow.size= 0.1, 
#      vertex.size = 0.1)

routes_tidy <- tbl_graph(nodes = nodes4, edges = edges3, directed = F) #%>%
  # activate(nodes) %>%
  # mutate(groups = group_infomap()) %>%
  # arrange(-groups)

#!!!!!!!!!!!!!!!!!
routes_tidy <- routes_tidy %>% 
  activate(nodes) %>%
  mutate(pgrank = centrality_pagerank(),
         betweeness = centrality_betweenness(),
         degree = centrality_degree()#,
         ) %>%
  arrange(-pgrank)


jpeg("network_centerclustersfr.jpeg", width = 30, height = 20, units = 'in', res = 800)
#NOs: lgl, fr, kk
set.seed(1234)
# ggraph(routes_tidy, layout = "graphopt") +
#   geom_node_point(aes(color = factor(groups))) +
#   geom_edge_fan(aes(width = weight), alpha = 0.7) +
#   scale_edge_width(range = c(0.5, 3)) +
#   geom_node_text(aes(label = label, color = factor(groups)), repel = T, size = 3) +
#   labs(title = "Rap Network", subtitle = "Who Features With Who?\nMinimum 11 Features",
#        edge_width = "Number of Features") +
#   scale_color_manual(values = col_vector) +
#   guides(color = F) +
#   theme_graph()
# ggraph(routes_tidy, layout = "graphopt") +
#   geom_edge_fan(aes(width = weight), alpha = 0.6, show.legend = T) +
#   scale_edge_width(range = c(0.25,5)) +
#   geom_node_text(aes(label = label), repel = F, size = 6, alpha = 1, color = "indianred") +
#   labs(title = "The Rap Network", subtitle = "Who Features With Who?",
#        edge_width = "Number of Features") +
#   theme_graph() +
#   theme(plot.title = element_text(size=100), 
#         plot.subtitle = element_text(size=60))

ggraph(routes_tidy, layout = 'fr') + 
  geom_edge_fan(aes(width = weight), 
                alpha = 0.1, 
                show.legend = F) +
  scale_edge_width(range = c(0.1,2)) +
  # geom_node_point(aes(filter = pgrank >= 0.0035),
  #                 color = "lightgrey", 
  #                 size = 0.25,
  #                 alpha = 0.1) + 
  scale_color_continuous(guide = 'legend') +
  geom_node_text(aes(filter = pgrank >= 0.002, label = label), 
                 size = 1,
                 repel = T, 
                 colour = "lightgrey",
                 alpha = 0.6) +  
  labs(title = "The Rap Network", 
       subtitle = "Who Features With Who?") +
  theme_graph(background = "dodgerblue") +
  theme(plot.title = element_text(size=100, hjust = 0.5), 
        plot.subtitle = element_text(size=75, hjust = 0.5))

dev.off()


centrality <- routes_tidy %>%
  activate(nodes) %>%
  as_tibble() %>%
  arrange(desc(pgrank)) %>%
  mutate(percentile = 1- row_number()/n())


# Just gets the top n
get_similar <- function(rapper, n){
  rapperid <- nodes4[which(nodes4$label == rapper),]$newid[1]
  using <- arrange(edges3, -weight) %>%
    filter(from == rapperid | to == rapperid)
  using_from <- using[which(using$from == rapperid),]
  names(using_from) <- c("weight","rapper","feature")
  using_to <- using[which(using$to == rapperid),]
  names(using_to) <- c("weight","feature","rapper")
  all <- bind_rows(using_to, using_from) %>%
    group_by(feature, rapper) %>%
    summarise(weight = sum(weight)) %>%
    arrange(-weight)
  nodes4$label[all$feature[1:n]]
}

library(networkD3)

routes_igraph <- graph_from_data_frame(edges3, nodes4, directed = F)
clusters <- cluster_edge_betweenness(routes_igraph)$membership

nodes_d3 <- mutate(nodes4, newid = newid - 1)
edges_d3 <- mutate(edges3, from = from - 1, to = to - 1)
nodes_d3 <- cbind(nodes_d3, clusters)

network <- forceNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to",
                        NodeID = "label", Group = "clusters", Value = "weight",
                        fontSize = 28, zoom = T, opacityNoHover = 0.9, opacity = 0.4)

saveNetwork(network, "~/Documents/R/rap_project/network.html", selfcontained = T)
