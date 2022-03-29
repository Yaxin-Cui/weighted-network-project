# weighted-network-project

## General information
- This is the anomalized data and the key part of the codes for the weighted network modeling using [valued-Exponential Random Graph Models (ERGM)](http://statnet.org/Workshops/valued.html) methods. 
- The code and data are assiocated with paper [''A Weighted Statistical Network Modeling Approach to Product Competition Analysis''](https://www.hindawi.com/journals/complexity/2022/9417869/).
- The code is writing under R language and using statenet package for ERGM simulation. There are two seperate codes corresponding to the co-consideration network and choice network constructed by customer purchase and consideration history. 

## Co-consideration network

### Code: 
[co-consideration_network.R](https://github.com/Yaxin-Cui/weighted-network-project/blob/main/choice_network.R)

### Data:
data: [consider_data_2013.csv](https://github.com/Yaxin-Cui/weighted-network-project/blob/main/consider_data_2013.csv), 
[consider_data_2014.csv](https://github.com/Yaxin-Cui/weighted-network-project/blob/main/consider_data_2014.csv), 
[product_data_2013.csv](https://github.com/Yaxin-Cui/weighted-network-project/blob/main/product_data_2013.csv), 
[product_data_2014.csv](https://github.com/Yaxin-Cui/weighted-network-project/blob/main/product_data_2014.csv)

## Choice network

### Code:
[choice_network.R](https://github.com/Yaxin-Cui/weighted-network-project/blob/main/choice_network.R)

### Data:
[consider_data_crossover.csv](https://github.com/Yaxin-Cui/weighted-network-project/blob/main/consider_data_crossover.csv), 
[product_data_crossover.csv](https://github.com/Yaxin-Cui/weighted-network-project/blob/main/product_data_crossover.csv)

## Note:
All data have been anomalized and the real feature values have been hidden. Therefore, you may not get the same results in the paper, but the code should illustrate the basic steps of network construction, estimation and prediction.

## Contact:
Create by Yaxin Cui - feel free to contact me at yaxincui2023@u.northwestern.edu
