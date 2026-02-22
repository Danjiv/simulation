library(tidyr)
library(readxl)
library(stringr)
library(purrr)
setwd("~/edinburgh_or/simulation/group_projects")

drivers_filename <- "drivers.xlsx"
riders_filename <- "riders.xlsx"

sheets_drivers <- readxl::excel_sheets(drivers_filename)
sheets_riders <- readxl::excel_sheets(riders_filename)

drivers <- readxl::read_excel(drivers_filename, sheet = sheets_drivers)
riders <- readxl::read_excel(riders_filename, sheet = sheets_riders)

################################################################################################################################

#LOOKING AT THE DATA

# DRIVERS:
# The drivers status column takes 4 different values: available (20), busy (21), offline (2073), offline-scheduled(2605)
# From this it looks like the available & busy status' are for drivers still active when the test data window closed
# Offline, looks like drivers who were 'available' when their offline_time was reached; and
# offline-scheduled looks like drivers whose offline_time was reached while they were on a job, so finished
# that job before going offline.
# I think, then, that current location is individual driver location when either their offline_time has been reached
# or the test data window closed.

#RIDERS:
# The riders status column takes 4 different values: abandoned(279), dropoff-scheduled(17), dropped-off(34117), pickup-scheduled(8).
# Clearly dropoff-scheduled and pickup-scheduled will be trips that were ongoing when the test data window closed.
# Notably, we do not have the data for the patience time of each rider - we only know that a trip was abandoned.

#BOTH DRIVERS AND RIDERS:
# We don't have a unique ID for drivers on the riders dataset, so we can't see which driver was matched to each rider,
# or the number of journeys each driver completed during their shift - and we can't map which drivers were available/busy at any given point
# during their shift.

#############################################################################################################################

#ASSUMPTIONS

#DRIVERS

#1. The drivers make themselves available in the app at random times with an inter-arrival of exponential(3/hour)

# Not sure why the first driver arrival time is just after 13:00 if they've started logging from 7:00. 

drivers$previous_arrival_times <- c(0, drivers$arrival_time[1:length(drivers$arrival_time)-1])
drivers$interarrival_times <- drivers$arrival_time - drivers$previous_arrival_times
# removing the first interarrival_times val as we don't really want to capture the initial 5 hour wait
interarrival_times <- drivers$interarrival_times[2:length(drivers$interarrival_times)]
# have a look
p1 <- hist(interarrival_times, breaks = "FD")
# have no values here for a few bins in the tail, try reducing number of breaks
p2 <- hist(interarrival_times, breaks = c(seq(0, 1.8, 0.075)))
# looks a bit better
breakpoints <- p2$breaks
probs <- pexp(breakpoints, rate=3)
#want the probability of a value falling within each bin
probs1 <- probs[2:length(probs)]
probs2 <- probs[1:length(probs)-1]
probs3 <- probs1 - probs2
# check number of expected values - remember we got rid of the first value
expected_numbers <- (length(drivers$id)-1) * probs3
# number of expected values in each bin doesn't look terrible
#goodness of fit test should have 23 degrees of freedom, given there 24 values
# the test stat value looks ridiculous: T = 719.622, which is obviously massively significant
# So obviously we would reject BoxCar's assumption that the interarrival time is 3/hour
test1 <- sum((p2$counts-expected_numbers)**2 / expected_numbers) 
#but then if we look at the MLE for the sample data, we get back 4718/sum(interarrival_times) = 4.742254
# Looking at the diff between counts and expected numbers, the real damage is being done in the first bin
# so repeating the analysis removing the large, tail outliers would have no real impact on the test stat.
# repeating the test to use Kolmogorov-Smirnov:
test2 <- ks.test(interarrival_times, "pexp", rate=3, alternative = "two.sided")
# again the p-value is massively significant!



#2. Once a driver becomes available, the driver stays available for a random time which is uniformly
# distributed between 5 and 8 hours and then goes offline. The length of availability is known to the driver,
# but not BoxCar. If the driver is serving a rider at the time s/he was planning to get offline, then the driver waits until
# s/he drops the rider at the destination and then goes offline
driver_working_time <- drivers$offline_time - drivers$arrival_time
#sanity checks
min(driver_working_time)
max(driver_working_time)
# min here is 6 hours - clearly the availability time can't be uniformly distributed between 5 and 8 hours
# have a look
d1 <- hist(driver_working_time, breaks = "FD")
# that looks pretty solidly uniformly distributed between 6 and 8 hours


#3. The location where the driver becomes available is equally likely to be anywhere in Squareshire, i.e. follows
# a uniform distribution. The driver stays at that location until being assigned a passenger. Similarly, when a driver
# drops a rider after reaching the destination, s/he stays there (at the last destination) until being assigned
# to another rider
drivers_initial_coords <- stringr::str_split(drivers$initial_location, ",")
drivers_x_coords <- purrr::map_dbl(drivers_initial_coords, ~as.double(stringr::str_replace(.[1], "\\(", "")))
drivers_y_coords <- purrr::map_dbl(drivers_initial_coords, ~as.double(stringr::str_replace(.[2], "\\)", "")))
#I think the way to test this, is it bin the initial coords of all drivers in a 20x20 grid, and test against that
initial_grid_position <- table(purrr::map2_chr(drivers_x_coords, drivers_y_coords, ~stringr::str_c(floor(.x), ":", floor(.y))))
# need to test for the grids that have no vals and add a val of 0
grid_side <- 20
#enumerate a full list of gridnames
full_grid <- unlist(purrr::map(0:(grid_side-1), function(x){
  purrr::map(0:(grid_side-1), ~stringr::str_c(x, ":", .))}))

full_grid_observed <- ifelse(full_grid %in% names(initial_grid_position),
                             initial_grid_position[full_grid],
                             0)
full_grid_expected <- rep(length(drivers$id) * 1/(grid_side)**2, grid_side**2)
test4 <- sum(((full_grid_observed - full_grid_expected)**2 / full_grid_expected))
# Again, looks massively significant, so clearly initial position is not uniformly distributed over Squareshire

#RIDERS

#1. The riders arrive at random times to demand a taxi ride with an inter-arrival time of exponential(30/hour)

riders$previous_request_time <- c(0, riders$request_time[1:length(riders$request_time)-1])
riders$interarrival_times <- riders$request_time - riders$previous_request_time
# removing the first interarrival_times val as we don't really want to capture the initial 5 hour wait
r_interarrival_times <- riders$interarrival_times[2:length(riders$interarrival_times)]
# have a look
test3 <- ks.test(r_interarrival_times, "pexp", rate=30, alternative = "two.sided")
# again p value is massively significant,
# looking at the MLE for the sample data gives 34420/(sum(r_interarrival_times)) = 34.59611


#2. The origin (the point where the rider appears to demand the ride) and the destination of the trip are independent of
# each other and is equally likely to be anywhere in Squareshire
# Again, split rider pick_up location and dropoff location into 20x20 grids, and test if they're uniformly
# distributed over Squareshite.
#Additionally, test if they are independent
riders_pickup_initial_coords <- stringr::str_split(riders$pickup_location, ",")
riders_pickup_x_coords <- purrr::map_dbl(riders_pickup_initial_coords, ~as.double(stringr::str_replace(.[1], "\\(", "")))
riders_pickup_y_coords <- purrr::map_dbl(riders_pickup_initial_coords, ~as.double(stringr::str_replace(.[2], "\\)", "")))
#
riders_dropoff_initial_coords <- stringr::str_split(riders$dropoff_location, ",")
riders_dropoff_x_coords <- purrr::map_dbl(riders_dropoff_initial_coords, ~as.double(stringr::str_replace(.[1], "\\(", "")))
riders_dropoff_y_coords <- purrr::map_dbl(riders_dropoff_initial_coords, ~as.double(stringr::str_replace(.[2], "\\)", "")))
#
rider_pickup_position <- table(purrr::map2_chr(riders_pickup_x_coords, riders_pickup_y_coords, ~stringr::str_c(floor(.x), ":", floor(.y))))
rider_dropoff_position <- table(purrr::map2_chr(riders_dropoff_x_coords, riders_dropoff_y_coords, ~stringr::str_c(floor(.x), ":", floor(.y))))
# need to test for the grids that have no vals and add a val of 0
grid_side <- 20
#enumerate a full list of gridnames
full_grid <- unlist(purrr::map(0:(grid_side-1), function(x){
  purrr::map(0:(grid_side-1), ~stringr::str_c(x, ":", .))}))

rider_pickup_grid_observed <- ifelse(full_grid %in% names(rider_pickup_position),
                             rider_pickup_position[full_grid],
                             0)
rider_dropoff_grid_observed <- ifelse(full_grid %in% names(rider_dropoff_position),
                                     rider_dropoff_position[full_grid],
                                     0)
full_grid_expected <- rep(length(drivers$id) * 1/(grid_side)**2, grid_side**2)

#test for rider pickup locations being uniformly distributed over Squareshire
test5 <- sum(((rider_pickup_grid_observed - full_grid_expected)**2 / full_grid_expected))
#test for rider dropoff locations being uniformly distributed over Squareshire
test6 <- sum(((rider_dropoff_grid_observed - full_grid_expected)**2 / full_grid_expected))
#Clearly it's nonsense that rider pickup and dropoff locations are uniformly distributed over Squareshire.

# testing for independence
rider_pickup_position2 <- purrr::map2_chr(riders_pickup_x_coords, riders_pickup_y_coords, ~stringr::str_c(floor(.x), ":", floor(.y)))
rider_dropoff_position2 <- purrr::map2_chr(riders_dropoff_x_coords, riders_dropoff_y_coords, ~stringr::str_c(floor(.x), ":", floor(.y)))

rider_pickup_dropoff_positions <- purrr::map2_chr(rider_pickup_position2, rider_dropoff_position2,
                                                 ~stringr::str_c(.x, "-", .y))
# enumerate a full list of possible pickup-dropoff location gridnames
full_grid_both <- unlist(purrr::map(full_grid, function(x){
  purrr::map(full_grid, ~stringr::str_c(x, "-", .))
}))

full_grid_both_observed <- ifelse(full_grid_both %in% names(rider_pickup_dropoff_positions),
                                  rider_pickup_dropoff_positions[full_grid_both],
                                  0)
full_grid_both_expected <- map_dbl(full_grid_both, function(x){
  
})

#3. Each arriving customer has an exponential(5/hour) patience times and if they are not matched with a driver within
# this patience time, they cancel the request and leave the system.
# don't think we have any way of directly testing this

#RIDER-DRIVER Matching and trip

#1. The length of each trip depends on the Euclidean distance between points. It is assumed that the average speed is approximately
# 20mph and expected trip time is d/20 and the actual trip time is uniformly distributed between (0.8*d/20, 1.2*d/20)

