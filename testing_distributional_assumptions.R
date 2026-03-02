library(tidyr)
library(readxl)
library(stringr)
library(purrr)
#library(truncdist)
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
# add on the probability that a val is larger than the final breakpoint
probs3 <- c(probs3, pexp(breakpoints[length(breakpoints)], rate=3, lower.tail = FALSE))
# check number of expected values - remember we got rid of the first value
expected_numbers <- (length(drivers$id)-1) * probs3
# number of expected values in each bin doesn't look terrible
#goodness of fit test should have 24 degrees of freedom, given there 25 values
# the test stat value looks ridiculous: T = 740.931, which is obviously massively significant
# So obviously we would reject BoxCar's assumption that the interarrival time is 3/hour
test1 <- sum((c(p2$counts, 0)-expected_numbers)**2 / expected_numbers) 
#but then if we look at the MLE for the sample data, we get back 4718/sum(interarrival_times) = 4.742254
# Looking at the diff between counts and expected numbers, the real damage is being done in the first bin
# so repeating the analysis removing the large, tail outliers would have no real impact on the test stat.
# repeating the test to use Kolmogorov-Smirnov:
test2 <- ks.test(interarrival_times, "pexp", rate=3, alternative = "two.sided")
# again the p-value is massively significant!
#
# testing that the data follow a exponential(4.742253) distribution
probs_new <- pexp(breakpoints, rate=4.742253)
#want the probability of a value falling within each bin
probs1_new <- probs_new[2:length(probs_new)]
probs2_new <- probs_new[1:length(probs_new)-1]
probs3_new <- probs1_new - probs2_new
probs3_new <- c(probs3_new, pexp(breakpoints[length(breakpoints)], rate=4.742253, lower.tail = FALSE))
expected_numbers_new <- (length(drivers$id)-1) * probs3_new
test1_new <- sum((c(p2$counts, 0)-expected_numbers_new)**2 / expected_numbers_new)
#test stat should follow a chi-squared distribution with 23 degrees of freedom
pchisq(test1_new, 23, lower.tail=FALSE)
# test looks non significant - all good!


#2. Once a driver becomes available, the driver stays available for a random time which is uniformly
# distributed between 5 and 8 hours and then goes offline. The length of availability is known to the driver,
# but not BoxCar. If the driver is serving a rider at the time s/he was planning to get offline, then the driver waits until
# s/he drops the rider at the destination and then goes offline
driver_working_time <- drivers$offline_time - drivers$arrival_time
#sanity checks
min(driver_working_time)
max(driver_working_time)
# min here is 6 hours - clearly the availability time won't be uniformly distributed between 5 and 8 hours
#performing the test anyway:
dwt <- hist(driver_working_time, breaks = c(seq(5, 8, 0.1)))
dwt_expected <- length(drivers$id) * (1/(length(dwt$breaks)-1))
test_dwt <- sum((dwt$counts-dwt_expected)**2 / dwt_expected)
pchisq(test_dwt, 29, lower.tail=FALSE) # massively significant, probably won't report this!

# have a look
d1 <- hist(driver_working_time, breaks = "FD")
# that looks pretty solidly uniformly distributed between 6 and 8 hours
# testing...
breakpoints2 <- d1$breaks
probs_drivers <- punif(breakpoints2, min = 6, max = 8)
#want the probability of a value falling within each bin
probs1_drivers <- probs_drivers[2:length(probs_drivers)]
probs2_drivers <- probs_drivers[1:length(probs_drivers)-1]
probs3_drivers <- probs1_drivers - probs2_drivers
expected_numbers_drivers <- length(drivers$id) * probs3_drivers
# test should follow a chi-squared distribution with 18 degrees of freedom
test_drivers <- sum(((d1$counts - expected_numbers_drivers)**2) / expected_numbers_drivers)
pchisq(test_drivers, 18, lower.tail=FALSE)
# non-significant, assumption of uniform(6, 8) looks good




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
# looking at the x and y coords
dx1 <- hist(drivers_x_coords, breaks = "FD")
# that looks normally distributed
#
dy1 <- hist(drivers_y_coords, breaks = "FD")
# also looks normally distributed
#
# looking at the covariance
dxy_cov <- cov(drivers_x_coords, drivers_y_coords)
# testing correlation
dxy_cor <- cor.test(drivers_x_coords, drivers_y_coords,  method = "pearson")
# don't look at all correlated, can then assume the x-coords and y-coords are sampled from their own
# normal distributions - will test parameters below.

# Think the problem with trying to use a ks test here 
# is that the data is obviously truncated!
xcord_test <- drivers_x_coords
ycord_test <- drivers_y_coords
xcord_test_mean <- mean(xcord_test)
xcord_test_var <- var(xcord_test)
ycord_test_mean <- mean(ycord_test)
ycord_test_var <- var(ycord_test)
xpos_ypos_n <- length(drivers$id)
test_drivers_x_pos <- ks.test(drivers_x_coords, "pnorm", mean = xcord_test_mean, sd = sqrt(xcord_test_var), alternative = "two.sided")
test_drivers_x_pos_stat <- (sqrt(xpos_ypos_n) - 0.01 + (0.85/(sqrt(xpos_ypos_n))))*test_drivers_x_pos$statistic
test_drivers_y_pos <- ks.test(drivers_y_coords, "pnorm", mean = 11.5, sd = sqrt(18.8), alternative = "two.sided")




#RIDERS

#1. The riders arrive at random times to demand a taxi ride with an inter-arrival time of exponential(30/hour)

riders$previous_request_time <- c(0, riders$request_time[1:length(riders$request_time)-1])
riders$interarrival_times <- riders$request_time - riders$previous_request_time
# removing the first interarrival_times val as we don't really want to capture the initial 5 hour wait
r_interarrival_times <- riders$interarrival_times[2:length(riders$interarrival_times)]
# have a look
test3 <- ks.test(r_interarrival_times, "pexp", rate=30, alternative = "two.sided")
test3_n <- length(r_interarrival_times)
test3_stat <- (sqrt(test3_n) + 0.12 + (0.11/sqrt(test3_n)))*test3$statistic
# our critical value for a two-tailed test at the 95% significant level here is
# 1.480, so clearly we would reject the null hypothesis!
# again p value is massively significant,
# looking at the MLE for the sample data gives 34420/(sum(r_interarrival_times)) = 34.59611
#
# will test using the sample MLE
# 
estimated_lambda <- 34420/(sum(r_interarrival_times))
test_riders_exponential <- ks.test(r_interarrival_times, "pexp", rate=estimated_lambda, alternative = "two.sided")
# calculate the adjusted test stat
# here the critical value is 1.190
test_riders_exponential_stat <- (test_riders_exponential$statistic - (0.2/test3_n))*(sqrt(test3_n) + 0.26 + (0.5/sqrt(test3_n)))


#2. The origin (the point where the rider appears to demand the ride) and the destination of the trip are independent of
# each other and is equally likely to be anywhere in Squareshire
# Again, split rider pick_up location and dropoff location into 20x20 grids, and perform a serial test to check if they're uniformly
# distributed over Squareshire.
riders_pickup_initial_coords <- stringr::str_split(riders$pickup_location, ",")
riders_pickup_x_coords <- purrr::map_dbl(riders_pickup_initial_coords, ~as.double(stringr::str_replace(.[1], "\\(", "")))
riders_pickup_y_coords <- purrr::map_dbl(riders_pickup_initial_coords, ~as.double(stringr::str_replace(.[2], "\\)", "")))
#
#want to bin the coordinates into 5 grids for the serial test
riders_pickup_x_coords2 <- ifelse(riders_pickup_x_coords < 4, 1,
                                  ifelse(riders_pickup_x_coords < 8, 2,
                                         ifelse(riders_pickup_x_coords < 12, 3,
                                                ifelse(riders_pickup_x_coords < 16, 4,
                                                       5))))

riders_pickup_y_coords2 <- ifelse(riders_pickup_y_coords < 4, 1,
                                  ifelse(riders_pickup_y_coords < 8, 2,
                                         ifelse(riders_pickup_y_coords < 12, 3,
                                                ifelse(riders_pickup_y_coords < 16, 4,
                                                       5))))
#
#
riders_dropoff_initial_coords <- stringr::str_split(riders$dropoff_location, ",")
riders_dropoff_x_coords <- purrr::map_dbl(riders_dropoff_initial_coords, ~as.double(stringr::str_replace(.[1], "\\(", "")))
riders_dropoff_y_coords <- purrr::map_dbl(riders_dropoff_initial_coords, ~as.double(stringr::str_replace(.[2], "\\)", "")))
#
#want to bin the coordinates into 5 grids for the serial test
riders_dropoff_x_coords2 <- ifelse(riders_dropoff_x_coords < 4, 1,
                                  ifelse(riders_dropoff_x_coords < 8, 2,
                                         ifelse(riders_dropoff_x_coords < 12, 3,
                                                ifelse(riders_dropoff_x_coords < 16, 4,
                                                       5))))
riders_dropoff_y_coords2 <- ifelse(riders_dropoff_y_coords < 4, 1,
                                   ifelse(riders_dropoff_y_coords < 8, 2,
                                          ifelse(riders_dropoff_y_coords < 12, 3,
                                                 ifelse(riders_dropoff_y_coords < 16, 4,
                                                        5))))
#
rider_pickup_position <- purrr::map2_chr(riders_pickup_x_coords2, riders_pickup_y_coords2, ~stringr::str_c(.x, ":", .y))
rider_dropoff_position <- purrr::map2_chr(riders_dropoff_x_coords2, riders_dropoff_y_coords2, ~stringr::str_c(.x, ":", .y))
rider_journey_position <- table(purrr::map2_chr(rider_pickup_position, rider_dropoff_position, ~stringr::str_c(.x, "-", .y)))
# need to test for the grids that have no vals and add a val of 0
grid_side <- 5
#enumerate a full list of gridnames
half_grid <- unlist(purrr::map(1:grid_side, function(x){
  purrr::map(1:grid_side, ~stringr::str_c(x, ":", .))}))

full_grid <- unlist(purrr::map(half_grid, function(x){
  purrr::map(half_grid, ~stringr::str_c(x, "-", .))
}))


rider_journey_observed <- ifelse(full_grid %in% names(rider_journey_position),
                             rider_journey_position[full_grid],
                             0)
                                     
full_grid_expected <- length(riders$id) * (1/625)

test_riders_location <- sum((rider_journey_observed - full_grid_expected)**2 / full_grid_expected)
# test will follow a chi-squared distribution with 5^4 - 1 = 624 degrees of freedom
pchisq(test_riders_location, 624, lower.tail=FALSE)
# clearly massively significant, so we can reject the null hypothesis that riders'
# pick-up and drop-off locations are uniformly distributed over Squareshire and independent.

#
# looking at rider pickup x and y coords
# test covariance / correlation
#
rpxy_cov <- cov(riders_pickup_x_coords, riders_pickup_y_coords)
# interesting
rpxy_cor_test <- cor.test(riders_pickup_x_coords, riders_pickup_y_coords)
# alright that's massively significant
#
# now looking to see if pickup x coords and dropoff x coords are correlated
rpdx_cor_test <- cor.test(riders_pickup_x_coords, riders_dropoff_x_coords)
# not correlated, all good!

# now looking at riders pickup y coords and riders dropoff y coords
rpdy_cor_test <- cor.test(riders_pickup_y_coords, riders_dropoff_y_coords)
# again uncorrelated - all good!
#
#

#3. Each arriving customer has an exponential(5/hour) patience times and if they are not matched with a driver within
# this patience time, they cancel the request and leave the system.
# don't think we have any way of directly testing this

#RIDER-DRIVER Matching and trip

#1. The length of each trip depends on the Euclidean distance between points. It is assumed that the average speed is approximately
# 20mph and expected trip time is d/20 and the actual trip time is uniformly distributed between (0.8*d/20, 1.2*d/20)
riders_pickup_initial_coords <- stringr::str_split(riders$pickup_location, ",")
riders_pickup_x_coords <- purrr::map_dbl(riders_pickup_initial_coords, ~as.double(stringr::str_replace(.[1], "\\(", "")))
riders_pickup_y_coords <- purrr::map_dbl(riders_pickup_initial_coords, ~as.double(stringr::str_replace(.[2], "\\)", "")))
#
riders_dropoff_initial_coords <- stringr::str_split(riders$dropoff_location, ",")
riders_dropoff_x_coords <- purrr::map_dbl(riders_dropoff_initial_coords, ~as.double(stringr::str_replace(.[1], "\\(", "")))
riders_dropoff_y_coords <- purrr::map_dbl(riders_dropoff_initial_coords, ~as.double(stringr::str_replace(.[2], "\\)", "")))

distance <- sqrt((riders_pickup_x_coords - riders_dropoff_x_coords)**2 + (riders_pickup_y_coords - riders_dropoff_y_coords)**2)
trip_time <- distance/20
trip_time_lb <- trip_time*0.8
trip_time_ub <- trip_time*1.2

checking_distance_df <- data.frame(distance = distance,
                                   trip_time = trip_time,
                                   trip_time_lb = trip_time_lb,
                                   trip_time_ub = trip_time_ub,
                                   actual_trip_time = (as.numeric(riders$dropoff_datetime - riders$pickup_datetime)/3600)) %>% subset(!is.na(actual_trip_time)) %>%
                                   dplyr::mutate(scalar = actual_trip_time / trip_time)

checking_scalar <- hist(checking_distance_df$scalar, breaks = "FD")
# looks like actual trip time is uniformly distributed between (0.7, 1.3)
# Let's test!
expected_vals <- (1/(length(checking_scalar$breaks)-1)) * length(checking_distance_df$distance)
test20 <- sum(((checking_scalar$counts - expected_vals)**2)/expected_vals)
# which is massively non significant, so we can go with uniform(0.7, 1.3)!!!
