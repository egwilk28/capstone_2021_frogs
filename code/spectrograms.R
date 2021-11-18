library(tuneR)
library(signal) # signal processing functions
library(tidyverse)
library(oce)
library(viridis)
library(plyr)
library(readr) #reads the data in

#path of file
file_audio_path <- './data_derived/noise_event.wav'

# #Read Files
train_audio = readWave(file_audio_path)

#create a sequence of a length of recording
t = seq( from = 0, to = length(train_audio)/train_audio@samp.rate, length.out = length(train_audio))

# define parameters for the spectrogram
p = 30 #number of seconds to calculate a spectrogram
#number of points to use for the fft
nfft = 1024
# window size in points
window = 1024
#overlap in points
overlap = 0
# overlap time in windows
hop = 0
#determine the sample rate
fs = train_audio@samp.rate

# Create spectrograms
SPs <- specgram(t,
             n = nfft,
             Fs = fs,
             window = window,
             overlap = overlap
    )

#the fast fourier transform isnt right... im not sure what the value should be but rn the y axis is in the thousands. we want it to show like 10 khz.