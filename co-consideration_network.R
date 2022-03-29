# Codes for co-consideration network estimation and prediction
# For the paper: A Weighted Statistical Network Modeling Approach to Product Competition Analysis
# Yaxin Cui, 2022

# Load in data and package ------------------------------------------------
library(statnet)
library(ggplot2)
library(dplyr)
library(scales)

# customer consideration and purchase data
consider_2013 <- read.csv("consider_data_2013.csv", header = T)
consider_2014 <- read.csv("consider_data_2014.csv", header = T)

# product data as node attributes
node_attr_2013 <- read.csv("product_data_2013.csv", header = T)
node_attr_2014 <- read.csv("product_data_2014.csv", header = T)


# Network construcstion ---------------------------------------------------
# Fucntion: Get_consider_net_data
get_consider_net_data <- function(considerdata,node_attr){
  
  # delete the respondent who have not buy a car in the car model list
  considerdata <- considerdata[-which(considerdata$model_id %in% node_attr$model_id == FALSE),]
  for (i in unique(considerdata$rspd_id)){
    subset <- considerdata[which(considerdata$rspd_id == i),]
    if(sum(subset$purchase) == 0){
      considerdata <- considerdata[-which(considerdata$rspd_id == i),]
    }
  }
  
  # Number of unique car models being purchased
  model_n <- nrow(subset(considerdata,!duplicated(considerdata$model_id)))
  
  model_seqid <- seq(model_n)
  uniquemodel_id <- subset(considerdata,!duplicated(considerdata$model_id))
  uniquemodel_id <- cbind(uniquemodel_id[order(uniquemodel_id$model_id),],as.data.frame(model_seqid))
  uniquemodel_id <- uniquemodel_id[,c("model_id","model_seqid")]
  
  # Rearrange considerdata
  considerdata <- merge(considerdata,uniquemodel_id)
  considerdata <- considerdata[order(considerdata$rspd_id,considerdata$purchase),]
  
  # construct a matrix
  consider_mat <- matrix(0, model_n, model_n)
  # initial values for counts
  size_i <- 1
  
  # calculate consider frequency
  for (i in 1:nrow(considerdata)){
    id_i <- considerdata$rspd_id[i]
    choice_i <- considerdata$purchase[i]
    
    if (choice_i != 1){
      size_i <- size_i + 1
    }else{
      if(size_i == 2){
        chosen_num <- considerdata$model_seqid[i]
        consid_num <- considerdata$model_seqid[i-1]
        consider_mat[consid_num, chosen_num] <- consider_mat[consid_num, chosen_num] + 1
        consider_mat[chosen_num, consid_num] <- consider_mat[consid_num, chosen_num]
      }else if(size_i == 3){
        chosen_num <- considerdata$model_seqid[i]
        consid_num <- considerdata$model_seqid[i-2]
        consid2_num <- considerdata$model_seqid[i-1]
        consider_mat[consid_num, chosen_num] <- consider_mat[consid_num, chosen_num] + 1
        consider_mat[chosen_num, consid_num] <- consider_mat[consid_num, chosen_num]
        consider_mat[consid2_num, chosen_num] <- consider_mat[consid2_num, chosen_num] + 1
        consider_mat[chosen_num, consid2_num] <- consider_mat[consid2_num, chosen_num]
        consider_mat[consid_num, consid2_num] <- consider_mat[consid_num, consid2_num] + 1
        consider_mat[consid2_num, consid_num] <- consider_mat[consid_num, consid2_num]
      }
      size_i <- 1
    }
  }
  
  colnames(consider_mat) <- uniquemodel_id$model_id
  rownames(consider_mat) <- uniquemodel_id$model_id
  return(consider_mat)
}

# Get the co-consideration network
consider_net_2013 <- get_consider_net_data(consider_2013,node_attr_2013)
consider_net_2014 <- get_consider_net_data(consider_2014,node_attr_2013)

# Construct the network and get the summary of network
consider_network_13 <- network(consider_net_2013,directed = FALSE,
                               matrix.type = "adjacency",ignore.eval = FALSE,
                               names.eval = "weight")

summary(consider_network_13)

consider_network_14 <- network(consider_net_2014,directed = FALSE,
                               matrix.type = "adjacency",ignore.eval = FALSE,
                               names.eval = "weight")


# Assign attributes to the constructed network
# Baseline effects of vehicle attributes
set.vertex.attribute(consider_network_13,list("Price","Fuelconsump","Power","Import"),
                     node_attr_2013[,c("price","fuelconsump","power","import")])
# homophily effects of vehicle attribute mathcing and difference
set.vertex.attribute(consider_network_13,"Segment",node_attr_2013$segment_num)
set.vertex.attribute(consider_network_13 ,"MakeOrigin",
                     as.integer(factor(node_attr_2013$makeorigin,
                                       levels = c("A","B","C",
                                                  "D","E"))))

# Baseline effects of vehicle attributes
set.vertex.attribute(consider_network_14,list("Price","Fuelconsump","Power","Import"),
                     node_attr_2014[,c("price","fuelconsump","power","import")])
# homophily effects of vehicle attribute mathcing and difference
set.vertex.attribute(consider_network_14,"Segment",node_attr_2014$segment_num)
set.vertex.attribute(consider_network_14 ,"MakeOrigin",as.integer(factor(node_attr_2013$makeorigin,
                                                                         levels = c("A","B","C",
                                                                                    "D","E"))))



# ERGM estimation ---------------------------------------------------------

# run the ERGM formulation
# poisson distribution: it takes more than 50 iterations
# the reference distribution can be changed in the model specification
date()
mv_poisson <- ergm(consider_network_13 ~ sum +
                       nodefactor("Import") + nodecov("Price",form = "sum") +
                       nodecov("Power",form = "sum") + nodecov("Fuelconsump",form = "sum") +  
                       nodematch("Segment",form = "sum") + 
                       nodematch("MakeOrigin",form = "sum")+
                       absdiff("Price") + absdiff("Power") + absdiff("Fuelconsump") +
                       nodefactor("MakeOrigin",form = "sum"),
                     response = "weight", reference = ~Poisson,
                     control = control.ergm(MCMC.interval=1024, MCMC.burnin=20480, 
                                            MCMC.samplesize=10240, MCMLE.maxit=100, 
                                            seed=123,parallel = 6))
summary(mv_poisson)
date()



# ERGM prediciton ---------------------------------------------------------

# prediction on the same year network
# using the simulate function in ERGM to predict the same year network structure
mv_2013_pois_sim <- simulate(consider_network_13 ~ sum + nodefactor("Import") +
                               nodecov("Price",form = "sum") +
                               nodecov("Power",form = "sum") + nodecov("Fuelconsump",form = "sum") +
                               nodematch("Segment",form = "sum") +
                               nodematch("MakeOrigin",form = "sum")+
                               absdiff("Price") + absdiff("Power") + absdiff("Fuelconsump") +
                               nodefactor("MakeOrigin",form = "sum"), nsim = 100, coef = mv_poisson$coef,
                             response = "weight",reference = ~Poisson, seed = 214)


# prediction on the new year network
# assume the new year network inherit the network structure in the previous year as the start point
# construct the network in 2014 using 2013 data
pre_consider_network_14 <- network(consider_net_2013, directed = FALSE,
                                   matrix.type = "adjacency",ignore.eval = FALSE,
                                   names.eval = "weight")

# set node attributes (based on 2014 product features)
# Baseline effects of vehicle attributes
set.vertex.attribute(pre_consider_network_14,list("Price","Fuelconsump","Power","Import"),
                     node_attr_2014[,c("price","fuelconsump","power","import")])
# homophily effects of vehicle attribute matching and difference
set.vertex.attribute(pre_consider_network_14,"Segment",node_attr_2014$segment_num)
set.vertex.attribute(pre_consider_network_14 ,"MakeOrigin",as.integer(factor(node_attr_2014$makeorigin)))

# prediction on 2014 network using the simulation function in ERGM
# the coefficients are from the estimation results
# poission distribution 
mv_2014_pois_sim <- simulate(pre_consider_network_14 ~ sum + nodefactor("Import") + 
                            nodecov("Price",form = "sum") +
                            nodecov("Power",form = "sum") + nodecov("Fuelconsump",form = "sum") +  
                            nodematch("Segment",form = "sum") + 
                            nodematch("MakeOrigin",form = "sum")+
                            absdiff("Price") + absdiff("Power") + absdiff("Fuelconsump") +
                            nodefactor("MakeOrigin",form = "sum"), nsim = 100, coef = coef(mv_poisson),
                          response = "weight",reference = ~Poisson, seed = 214)


# evaluation the prediction performance
evaluation_v <- function(nw_sim,num_sim,response,response_name){
  # list the simulated edge values
  simulate_matrix <- rep(0,296,296)
  for (i in (1:num_sim)){
    simulate_matrix <- simulate_matrix + as.matrix(nw_sim[[i]],
                                                   attrname = response_name)
  }
  simulate_matrix <- simulate_matrix / num_sim
  
  # list the edge values using a vector
  simulate_ave <- rep(0,43600)
  edgelist <- rep(0,43600)
  n <- 0
  for (i in (1:295)){
    for (j in ((i+1):296)){
      n <- n + 1
      simulate_ave[n] <- simulate_matrix[i,j]
      edgelist[n] <- response[i,j]
    }
  }
  
  # calculate pearson correlation
  cor <- cor(edgelist,simulate_ave,method = "pearson")
  # calculate mse
  mse <- mean((edgelist-simulate_ave)^2)
  
  rss <- sum((edgelist-simulate_ave) ^ 2)
  tss <- sum((edgelist - mean(edgelist)) ^ 2)
  rsq <- 1 - rss/tss
  
  sim_comparision <- cbind(edgelist,simulate_ave)
  gof <- cbind(cor,mse,rsq)
  result <- list(comp = sim_comparision, cor = gof)
  
  return(result)
}

# results for the prediciton of the same year and the new year
result_pois_2013 <-  evaluation_v(mv_2013_pois_sim,100,consider_net_2013,"weight")
result_pois_2014 <-  evaluation_v(mv_2014_pois_sim,100,consider_net_2014,"weight")

test_pois_2013 <- as.data.frame(result_pois_2013$comp)
test_pois_2014 <- as.data.frame(result_pois_2014$comp)

# the plot of the prediciton results
p2013 <- ggplot(test_pois_2013,aes(x = edgelist, y = simulate_ave))+ geom_point()
p2013 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
           panel.background = element_blank(),axis.line = element_line(colour = "black"),
           text = element_text(size=14)) + xlim(0,350) + ylim(0,350) + 
  xlab("Actual link strength") + ylab("Predicted link strength")

p2014 <- ggplot(test_pois_2014,aes(x = edgelist, y = simulate_ave))+ geom_point()
p2014 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
           panel.background = element_blank(),axis.line = element_line(colour = "black"),
           text = element_text(size=14)) + xlim(0,350) + ylim(0,350) + 
  xlab("Actual link strength") + ylab("Simulated link strength")


