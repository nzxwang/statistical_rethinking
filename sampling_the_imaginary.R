PrP_g_V <- 0.95
PrP_g_M <- 0.01
PrV <- 0.001
#Probability of Vampire given Positive test
PrV_g_P <- (PrP_g_V * PrV) / (PrP_g_V * PrV + PrP_g_M * (1-PrV))
#This is because FalseP much bigger than TrueP
FalseP <- PrP_g_M * (1-PrV)
TrueP <- PrP_g_V * PrV
