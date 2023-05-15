# Alessandro Gentilin
# 12/05/2023 - Vicenza - Italy
# Open-source-synthetic-photoplethysmographic-signal-generator-with-analog-output
# R code to generate a PPG signal ranging from 0 to 255
# After generating the signal, copy-paste the data into the data array in the Arduino code

rm(list=ls())
PPG <- numeric()
number_cycles <- 25 # specify here the number of PPG cycles that will compose your generated signal

# data points per cycle
# Specify here the minimum and maximum number of points each PPG cycle can be composed. 
# The PPG lenght will be chosen randomly within the specified range. 
# To simulate a higher sampling rate, just provide higher numbers - i.e. from 990 to 1010
points_min <- 90  #modify the number here
points_max <- 110  #modify the number here


# change below the values of amplitude, mean (mu), and variance (sigma) of each Gaussian function
# each PPG cycle will be generated with random values of amplitude, mean (mu), and variance (sigma) within the specified range.

# first gaussian function
amplitude1_min <- 0.5 #modify the number here
amplitude1_max <- 0.7 #modify the number here
mu1_min <- 20 #modify the number here
mu1_max <- 30 #modify the number here
sigma1_min <- 10 #modify the number here
sigma1_max <- 15 #modify the number here

# second gaussian function
amplitude2_min <- 0.2 #modify the number here
amplitude2_max <- 0.4 #modify the number here
mu2_min <- 40 #modify the number here
mu2_max <- 50 #modify the number here
sigma2_min <- 17 #modify the number here
sigma2_max <- 23 #modify the number here

# third gaussian function
amplitude3_min <- 0.25 #modify the number here
amplitude3_max <- 0.35 #modify the number here
mu3_min <- 60 #modify the number here
mu3_max <- 70 #modify the number here
sigma3_min <- 18 #modify the number here
sigma3_max <- 12 #modify the number here


# signal generation
for (i in 1:number_cycles) {
a <- c(amplitude1_min+runif(1)*(amplitude1_max-amplitude1_min), amplitude2_min+runif(1)*(amplitude2_max-amplitude2_min), amplitude3_min+runif(1)*(amplitude3_max-amplitude3_min))
mu <- c(mu1_min+runif(1)*(mu1_max-mu1_min), mu2_min+runif(1)*(mu2_max-mu2_min), mu3_min+runif(1)*(mu3_max-mu3_min))
sigma <- c(sigma1_min+runif(1)*(sigma1_max-sigma1_min), sigma2_min+runif(1)*(sigma2_max-sigma2_min), sigma3_min+runif(1)*(sigma3_max-sigma3_min))
x <- seq(from = 1, to = (points_min +runif(1)*(points_max-points_min)), by = 1)
y1 <- a[1] * exp(-(((x - mu[1])/sigma[1])^2)/2)
y2 <- a[2] * exp(-(((x - mu[2])/sigma[2])^2)/2)
y3 <- a[3] * exp(-(((x - mu[3])/sigma[3])^2)/2)
y <- y1 + y2 + y3
PPG <- c(PPG, y)}

size_PPG <- dim(PPG)



# generation of the low frequency trend related to the respiratory rate
# you can modify ranges related to the length and amplitude of each breath
# it will be generated a series of breathing cycles ranging withing the selected limits
rumore <- numeric()
for (z in 1:(i*2)) {
min_breathing_noise_length=0.005 #modify the number here
maxbreathing_noise_length=0.02 #modify the number here
min_breathing_noise_amplitude=0.1 #modify the number here
maxbreathing_noise_amplitude=0.2 #modify the number here

x <- seq(-pi, pi, by = (min_breathing_noise_length + runif(1, 0, maxbreathing_noise_length - min_breathing_noise_length)))
rumore <- c(rumore, sin(x)*(min_breathing_noise_amplitude + runif(1, 0, maxbreathing_noise_amplitude - min_breathing_noise_amplitude)))
}
# plot(rumore,type="l") # this will display the generated breathing trend
len=length(PPG)
rum<- rumore[1:len]
PPG_rumore_bassa_frequenza <- PPG + rum
# plot(PPG_rumore_bassa_frequenza,type="l") # this will display the generated signal integrated with the breathing trend


# Generation of random noise
PPG_rumore_bianco_piu_bassa_frequenza <- rnorm(length(PPG_rumore_bassa_frequenza), mean = PPG_rumore_bassa_frequenza, sd = 0.1) #change the sd value to change the degree of noise
plot(PPG_rumore_bianco_piu_bassa_frequenza,type="l")


# normalization between 0 and 255

# PPG + breathing trend + noise
PPG_rumore_bianco_piu_bassa_frequenza_norm <- PPG_rumore_bianco_piu_bassa_frequenza
norm_data <- 255 * (PPG_rumore_bianco_piu_bassa_frequenza_norm - min(PPG_rumore_bianco_piu_bassa_frequenza_norm)) / (max(PPG_rumore_bianco_piu_bassa_frequenza_norm) - min(PPG_rumore_bianco_piu_bassa_frequenza_norm))
# plot(norm_data,type="l")
t(norm_data)

# PPG + breathing trend
PPG_rumore_bassa_frequenza_norm <- PPG_rumore_bassa_frequenza
PPG_rumore_bassa_frequenza_norm_norm_data <- 255 * (PPG_rumore_bassa_frequenza_norm - min(PPG_rumore_bassa_frequenza_norm)) / (max(PPG_rumore_bassa_frequenza_norm) - min(PPG_rumore_bassa_frequenza_norm))
# plot(PPG_rumore_bassa_frequenza_norm_norm_data,type="l")
t(PPG_rumore_bassa_frequenza_norm_norm_data)

# original PPG
PPG_norm <- PPG
PPG_norm_data <- 255 * (PPG_norm - min(PPG_norm)) / (max(PPG_norm) - min(PPG_norm))
# plot(PPG_norm_data,type="l")
t(PPG_norm_data)

# breathing rate
rumore_norm <- rumore
rumore_norm_data <- 255 * (rumore_norm - min(rumore_norm)) / (max(rumore_norm) - min(rumore_norm))
# plot(PPG_norm_data,type="l")
t(rumore_norm_data)



# save files - just replace "my_dir" with the directory you want to save the files
# you can generate all the following signals in the analog form:
# 1)PPG with breathing trend and noise; 2)PPG with breathing trend; 3)original PPG; 4)breathing rate
# Just copy-paste the data you want to convert into the "data" array in the Arduino script
write.table(norm_data, file = "my_dir/PPG_signal_breathing_and_noise.txt", sep = ",", dec = ".", row.names = FALSE)
write.table(PPG_rumore_bassa_frequenza_norm_norm_data, file = "my_dir/PPG_signal_breathing.txt", sep = ",", dec = ".", row.names = FALSE)
write.table(PPG_norm_data, file = "my_dir/original_PPG.txt", sep = ",", dec = ".", row.names = FALSE)
write.table(rumore_norm_data, file = "my_dir/breathing_trend.txt", sep = ",", dec = ".", row.names = FALSE)
