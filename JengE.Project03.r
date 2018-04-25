# Name: Eric Jeng
# Course: 44-149 Scientific Computing
# Assignment # Project #03
# Due Date: 04/20/2018
# Brief: Clustering a population from data given to us
# By submitting this, I pledge that the code in this file was written by the author idicated above,
# and that ll assistance was correctly attributed in comments.  Additionally, I
# agree to abide by the rules expressed in the CSIS Academic Honest Policy

census <- read.csv('us_census.csv')
n <- 12
iters <- 12

contiguous <- census[!census$state %in% c('AK', 'HI', 'PR'), ]
counties <- sample(1:nrow(contiguous), n)

plot(contiguous$longitude,contiguous$latitude,type = 'p',col = contiguous$state)

centers <- matrix(0, nrow = n, ncol = 2)
for (i in 1:n) {
  centers[, 1] = contiguous$latitude[counties]
  centers[, 2] = contiguous$longitude[counties]
}

dist_sq <- function(county, center) {
  deltax <- county[1, 'latitude'] - center[1]
  deltay <- county[1, 'longitude'] - center[2]
  deltax ^ 2 + deltay ^ 2
}

print(dist_sq(contiguous[1, ], centers[1, ]))

# belongs_to[i] means the ith county belongs to the cluster at belongs_to[i]
belongs_to <- rep(0, nrow(contiguous))

for (l in 1:iters) {
  #figure out closest cluster
  for (county in 1:nrow(contiguous)) {
    closest_center <- 1
    closest_distance <- dist_sq(contiguous[county,], centers[1,])
    
    for (cluster in 2:n) {
      d <- dist_sq(contiguous[county,], centers[cluster,])
      if (d < closest_distance) {
        closest_distance <- d
        closest_center <- cluster
      }
    }
    belongs_to[county] <- closest_center
  }
  
  plot(contiguous$longitude,contiguous$latitude,type = 'p',col = belongs_to)
  
  for (new_cluster in 1:n) {
    clust_of_interest <- contiguous[belongs_to == new_cluster,]
    total_pop <- (sum(clust_of_interest$population))
    new_latitude <-sum(clust_of_interest$latitude * clust_of_interest$population) / total_pop
    new_longitude <-sum(clust_of_interest$longitude * clust_of_interest$population) / total_pop
    centers[new_cluster, 1] <- new_latitude
    centers[new_cluster, 2] <- new_longitude
  }
  plot(contiguous$longitude,contiguous$latitude,type = 'p',col = belongs_to)
}