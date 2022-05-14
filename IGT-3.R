X <- rnorm(100)
NoiseEpsilon <- rnorm(100)
Y <- double(100)
Beta <- c(0.2137,0.420,0.911,0.69)
for(i in 1:100){
  Y[i] <- Beta[1] + Beta[2] * X[i] + Beta[3] * (X[i] ** 2) + Beta[4] * (X[i] ** 3) + NoiseEpsilon[i]
}

