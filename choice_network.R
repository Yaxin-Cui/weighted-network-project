# Codes for choice network estimation and prediction
# For the paper: A Weighted Statistical Network Modeling Approach to Product Competition Analysis
# Yaxin Cui, 2022

# Library -----------------------------------------------------------------
# Include library
library(ggplot2)
library(network)
library(visNetwork)
library(networkD3)
library(ggplot2)
library(statnet)
library(dplyr)
library(FNN)
library(pracma)


# Load in data ------------------------------------------------------------
consider_2013 <- read.csv("consider_data_crossover.csv", header=T)
node_attr <- read.csv("product_data_crossover.csv",header=TRUE)
model_crossover <- read.csv("model_crossover.csv",header=TRUE)

# Get_choice_net_data
get_choice_net_data <- function(considerdata,node_attr){
  
  considerdata <- considerdata[-which(considerdata$model_id %in% node_attr$model_id == FALSE),]
  
  # for (i in unique(considerdata$model_id)){
  #   if (considerdata$model_id %in% node_attr$model_id == FALSE){
  #     considerdata <- considerdata[-which(considerdata$model_id == i),]
  #   }
  # }
  
  # Number of unique car models being purchased
  model_n <- nrow(subset(considerdata,!duplicated(considerdata$model_id)))
  
  model_seqid <- seq(model_n)
  uniquemodel_id <- subset(considerdata,!duplicated(considerdata$model_id))
  uniquemodel_id <- cbind(uniquemodel_id[order(uniquemodel_id$model_id),],as.data.frame(model_seqid))
  uniquemodel_id<-uniquemodel_id[,c("model_id","model_seqid")]
  
  # Rearrange considerdata
  considerdata <- merge(considerdata,uniquemodel_id)
  considerdata <- considerdata[order(considerdata$rspd_id,considerdata$purchase),]
  
  # construct a matrix
  chosen_mat <- matrix(0, model_n, model_n)
  # initial values for counts
  size_i <- 1
  
  # calculate choice frequency
  for (i in 1:nrow(considerdata)){
    id_i <- considerdata$rspd_id[i]
    choice_i <- considerdata$purchase[i]
    
    if (choice_i != 1){
      size_i <- size_i + 1
    }else{
      if(size_i == 2){
        chosen_num =considerdata$model_seqid[i]
        consid_num =considerdata$model_seqid[i-1]
        chosen_mat[consid_num, chosen_num] = chosen_mat[consid_num, chosen_num] + 1
      }else if(size_i == 3){
        chosen_num=considerdata$model_seqid[i]
        consid_num=considerdata$model_seqid[i-2]
        consid2_num=considerdata$model_seqid[i-1]
        chosen_mat[consid_num, chosen_num] = chosen_mat[consid_num, chosen_num] + 1
        chosen_mat[consid2_num, chosen_num] = chosen_mat[consid2_num, chosen_num] + 1
        # consid_mat[consid_num, consid2_num] = consid_mat[consid_num, consid2_num] + 1
      }
      size_i <- 1
    }
  }
  
  colnames(chosen_mat) <- uniquemodel_id$model_id
  rownames(chosen_mat) <- uniquemodel_id$model_id
  return(chosen_mat)
}
choice_net_2013 <- get_choice_net_data(consider_2013,node_attr)

# Construct the network ----------------------------------------------

choice_network <- network(choice_net_2013,directed = TRUE,
                          matrix.type = "adjacency",ignore.eval = FALSE,
                          names.eval = "weight")

# ERGM simulation ---------------------------------------------------------

# Baseline effects of vehicle attributes
set.vertex.attribute(choice_network,list("Price","Fuelconsump","Power","Import"),
                     node_attr[,c("price","fuelconsump","power","import")])
# homophily effects of vehicle attribute mathcing and difference
set.vertex.attribute(choice_network,"Segment",node_attr$segment_num)
set.vertex.attribute(choice_network ,"MakeOrigin",as.integer(factor(node_attr$makeorigin,
                                                                    levels = c("A","B","C",
                                                                               "D","E"))))
# poission distribution without network structural effect
mv_d_poi <- ergm(choice_network ~ sum +
                   nodeifactor("Import") + nodeicov("Price",form = "sum") +
                   nodeicov("Power",form = "sum") + nodeicov("Fuelconsump",form = "sum") +  
                   nodematch("Segment",form = "sum") + 
                   nodematch("MakeOrigin",form = "sum") +
                   nodeifactor("MakeOrigin",form = "sum"),
                 response = "weight", reference = ~Poisson,
                 control = control.ergm(MCMC.interval=1024, MCMC.burnin=20480, 
                                        MCMC.samplesize=10240,MCMLE.maxit=1000,
                                        seed=214,parallel = 8))
summary(mv_d_poi)

# add network structural effect - cyclical weights
date()
mv_d_poi_1 <- ergm(choice_network ~ sum + cyclicalweights + 
                   nodeifactor("Import") + nodeicov("Price",form = "sum") +
                   nodeicov("Power",form = "sum") + nodeicov("Fuelconsump",form = "sum") +  
                   nodematch("Segment",form = "sum") + 
                   nodematch("MakeOrigin",form = "sum") +
                   nodeifactor("MakeOrigin",form = "sum"),
                 response = "weight", reference = ~Poisson,
                 control = control.ergm(MCMC.interval=1024, MCMC.burnin=20480, 
                                        MCMC.samplesize=10240,MCMLE.maxit=1000,
                                        seed=214,parallel = 8))
date()
summary(mv_d_poi_1)

# add network structural effect - transitivity weights
date()
mv_d_poi_2 <- ergm(choice_network ~ sum + transitiveweights + 
                     nodeifactor("Import") + nodeicov("Price",form = "sum") +
                     nodeicov("Power",form = "sum") + nodeicov("Fuelconsump",form = "sum") +  
                     nodematch("Segment",form = "sum") + 
                     nodematch("MakeOrigin",form = "sum") +
                     nodeifactor("MakeOrigin",form = "sum"),
                   response = "weight", reference = ~Poisson,
                   control = control.ergm(MCMC.interval=1024, MCMC.burnin=20480, 
                                          MCMC.samplesize=10240,MCMLE.maxit=1000,
                                          seed=214,parallel = 8))
date()
summary(mv_d_poi_2)


# Simulate new network ----------------------------------------------------

# simulate use baseline model: mv_d_poi
mv_sim_poi <- simulate(choice_network ~ sum +
                         nodeifactor("Import") + nodeicov("Price",form = "sum") +
                         nodeicov("Power",form = "sum") + nodeicov("Fuelconsump",form = "sum") +  
                         nodematch("Segment",form = "sum") + 
                         nodematch("MakeOrigin",form = "sum") +
                         nodeifactor("MakeOrigin",form = "sum"),
                         nsim = 100, coef = mv_d_poi$coef,
                         response = "weight", reference = ~Poisson,seed = 123)
# simulate model 1:
mv_sim_poi_1 <- simulate(choice_network ~ sum + cyclicalweights + 
                           nodeifactor("Import") + nodeicov("Price",form = "sum") +
                           nodeicov("Power",form = "sum") + nodeicov("Fuelconsump",form = "sum") +  
                           nodematch("Segment",form = "sum") + 
                           nodematch("MakeOrigin",form = "sum") +
                           nodeifactor("MakeOrigin",form = "sum"),
                         nsim = 100, coef = mv_d_poi_1$coef,
                         response = "weight", reference = ~Poisson,seed = 123)

# simulate model 2:
mv_sim_poi_2 <- simulate(choice_network ~ sum + transitiveweights + 
                           nodeifactor("Import") + nodeicov("Price",form = "sum") +
                           nodeicov("Power",form = "sum") + nodeicov("Fuelconsump",form = "sum") +  
                           nodematch("Segment",form = "sum") + 
                           nodematch("MakeOrigin",form = "sum") +
                           nodeifactor("MakeOrigin",form = "sum"),
                         nsim = 100, coef = mv_d_poi_2$coef,
                         response = "weight", reference = ~Poisson,seed = 123)


# Model evaluation --------------------------------------------------------

# calculate the market share based on the predicted results
# a function to calculate market share: input: simulate netowrk, output: market share
sim_ms <- function(mv_sim,node_attr,model_crossover){
  simulate_matrix <- rep(0,217,217)
  for (i in (1:100)){
    simulate_matrix <- simulate_matrix + as.matrix(mv_sim[[i]],
                                                   attrname = "weight")
  }
  simulate_matrix <- simulate_matrix / 100
  
  # calcuate the market share
  
  # create a sim_network
  sim_network <- network(simulate_matrix, directed = TRUE,
                         matrix.type = "adjacency",ignore.eval = FALSE,
                         names.eval = "weight")
  
  # calculate the degree
  sim_test <- as.edgelist(sim_network, attrname = "weight", output = "matrix")
  library(tnet)
  #sim_test <- ifelse(sim_test < 0, 0, sim_test)
  sim_degree <- degree_w(net = sim_test, 
                         measure=c("degree","output"), alpha=1, type="in")
  colnames(sim_degree) <- c("node","degree","weighted_indegree")
  
  sim_degree <- cbind(model_id = node_attr$model_id,sim_degree)
  sim_degree <- as.data.frame(sim_degree)
  sim_mrk_share <- sim_degree[which(sim_degree$model_id %in% model_crossover$model_id),]
  sim_mrk_share$share <- sim_mrk_share$weighted_indegree / sum(sim_mrk_share$weighted_indegree)
  
  return(sim_mrk_share)
}

sim_market_share <- sim_ms(mv_sim_poi,node_attr, model_crossover)

sim_market_share_1 <- sim_ms(mv_sim_poi_1,node_attr, model_crossover)

sim_market_share_2 <- sim_ms(mv_sim_poi_2,node_attr, model_crossover)
