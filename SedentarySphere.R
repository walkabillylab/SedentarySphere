# Created by Javad Rahimipour Anaraki on 08/02/18
# Ph.D. Candidate
# Department of Computer Science
# Memorial University of Newfoundland
# jra066 [AT] mun [DOT] ca | www.cs.mun.ca/~jra066

# This program is an implementation of the method explained in:
# Sedentary Sphere: Wrist-Worn Accelerometer-Brand Independent Posture Classification
# ALEX V. ROWLANDS, THOMAS YATES, TIM S. OLDS, MELANIE DAVIES, KAMLESH KHUNTI, and CHARLOTTE L. EDWARDSON
# https://www.ncbi.nlm.nih.gov/pubmed/26559451
#=======================Libraries==========================
library(rstudioapi)

#=========================Variables========================
cwd <- dirname(getSourceEditorContext()$path)
data <- read.csv(paste(cwd,'/Sed_Sph_data.csv', sep = ""), header = FALSE)

#==========================Time============================
data[, "Time"] <- substr(data[, "V1"], 12, 19)

#==========================Angle Q=========================
data[, "Angle Q"] <- atan( abs(data[, "V4"]) / abs(data[, "V2"]) ) * 180 / pi

#==================Rotational Q and 360 Angle==============
for(i in 1:nrow(data)) {
  if( (data[i,"V2"] > 0) & (data[i,"V4"] > 0) ) {
    data[i, "Rotational Q"] <- 1
    data[i, "360 Angle"] <- 90 - data[i, "Angle Q"]
  }
  else if( (data[i,"V2"] < 0) & (data[i,"V4"] > 0) ) {
    data[i, "Rotational Q"] <- 2
    data[i, "360 Angle"] <- 180 - data[i, "Angle Q"]
  }
  else if( (data[i,"V2"] < 0)  & (data[i,"V4"] < 0) ) {
    data[i, "Rotational Q"] <- 3
    data[i, "360 Angle"] <- 180 + data[i, "Angle Q"]
  }
  else {
    data[i, "Rotational Q"] <- 4
    data[i, "360 Angle"] <- 360 - data[i, "Angle Q"]
  }
  
  #==========================Up/down=========================
  if(data[i,"V3"] > 1) {data[i, "Up/down"] <- asin(1) * 180 / pi}
  else if(data[i,"V3"] < -1) {data[i, "Up/down"] <- asin(-1) * 180 / pi}
  else {data[i, "Up/down"] <- asin(data[i,"V3"]) * 180 / pi}
}

#===========================Sum SD=========================
data[, "Sum SD"] <- data[, "V9"] + data[, "V10"] + data[, "V11"]

#==========================Movement========================
for(i in 2:nrow(data)) {
  data[i, "Movement"] <- sqrt(abs(data[i-1,"Up/down"] - data[i,"Up/down"] + data[i-1,"Sum SD"] - data[i,"Sum SD"]))
}

#==========================Activity========================
data[, "Activity"] <- data[, "V8"] * data[, "Movement"]
data[1, "Activity"] <- 0

#====================Sleep 1 Sleep 2 Sleep 3===============
for(i in 1:nrow(data)) {
  if (data[i, "Activity"] < 6)
    data[i, "Sleep 1"] <-  1
  else
    data[i, "Sleep 1"] <-  0
  
  j <- (i - 48)
  if (j < 0) {j = 1}
  data[i, "Sleep 2"] <-  sum(data[j:i, "Sleep 1"])
  
  if (data[i, "Sleep 2"] > 24)
    data[i, "Sleep 3"] <-  1
  else
    data[i, "Sleep 3"] <-  0
}

#=======================Activity State=====================
for(i in 1:nrow(data)) {
  if(data[i,"Sleep 3"] == 1) {
    data[i, "Activity State"] <- "sleep"
    data[i, "Posture"] <- "sit/lie"
  } else if(data[i,"V8"] > 566)
    data[i, "Activity State"] <- "vigorous"
  else if((data[i,"V8"] < 566) & (data[i,"V8"] > 202))
    data[i, "Activity State"] <- "moderate"
  else if((data[i,"V8"] < 202) & (data[i,"V8"] > 95))
    data[i, "Activity State"] <- "light"
  else if(data[i,"V8"] <= 95)
    data[i, "Activity State"] <- "sedentary"
}

#=========================Posture==========================
for(i in 1:nrow(data)) {
  if(data[i,"Sleep 3"] == 1)
    data[i, "Posture"] <- "sit/lie"
  else if(data[i,"Up/down"] < -15)
    data[i, "Posture"] <- "stand"
  else if(data[i,"Activity"] < 489)
    data[i, "Posture"] <- "sit/lie"
  else
    data[i, "Posture"] <- "stand"
}

#======================Saving output=======================
write.csv(data, paste(cwd,'/Sed_Sph_data_labeled.csv', sep = ""))