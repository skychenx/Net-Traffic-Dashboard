# Net-Traffic-Dashboard

A Rshiny dashboard for visualizing network traffic data across countries, ports, and ASNs. 

## Request Access To Data
For accessing the .csv files, which are collected throught Merit Network's Distributed Network Telescope, please contact Michael Kallitsis at mgkallit@merit.edu.

## Download the files
```
git clone https://github.com/skychenx/Net-Traffic-Dashboard.git
```

## Setup
1. Download [Rstudio](https://rstudio.com/products/rstudio/download/), the IDE used to run the .R files.
2. Download [Redis](https://redis.io/download), which is a distributed, in-memory key–value database used for this project.
3. Follow the Redis tutorial for installation. To start up the Redis server, run
'''
src/redis-server
'''
4. Check that the .csv files resides inside a folder called "data" inside this repository.
5. Source store_redis.R to run the file, which extracts the data from the .csv files and stores them in key-value pairs inside a local Redis database.
6. Run app.R.

## Contributions
Thanks to Dr. Michael Kallitsis, who provided the data required for this project and the guidance he provided throughout the project, Dr. Stilian Stoev for his wonderful mentoring and guidance for the technical details of this project, and Mark Weiman for his many advices and suggestions, including the usage of Redis as our database. 
