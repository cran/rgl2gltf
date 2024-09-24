## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(rgl)
setupKnitr(autoprint=TRUE)

## -----------------------------------------------------------------------------
library(rgl)
material3d()

## -----------------------------------------------------------------------------
xyz <- cbind(c(0, 1, 1, 0), c(0, 0, 1, 1), c(0, 0, 1, 1))
st <- xyz[, 1:2]
quads3d(xyz, 
        texcoords = st, 
        texture = system.file("textures/rgl2.png", package = "rgl"),
        color = "steelblue")

## -----------------------------------------------------------------------------

# Typical PRB parameters
roughness <- 1
metalness <- 1
baseColor <- c(1, 1, 1)

# Default rgl parameters
s <- 50
Cs <- c(1,1,1)

Cd <- baseColor
m <- metalness
alpha <- roughness^2

h <- c(0.999, 0.5)

N <- c(0,0,1)
theta <- 30 * pi/180 # angle between V and N
V <- c(sin(theta), 0, cos(theta))
VdotN <- sum(V*N)


phi <- seq(-80, 80, by = 5)*pi/180 # angle between L and N
PBR <- OGL1 <- matrix(NA, nrow=length(phi), ncol=3)
for (j in seq_along(phi)) {
  L <- c(sin(phi[j]), 0, cos(phi[j]))
  H <- (L + V)/sqrt(sum((L + V)^2))
  HdotN <- sum(H*N)
  HdotL <- sum(H*L)
  NdotL <- sum(N*L)
  HdotV <- sum(H*V)
  D <- (1/pi)*alpha^2*max(0, HdotN)/(HdotN^2 * (alpha^2 - 1) + 1)^2
  G <- 2*NdotL*max(HdotL, 0)*2*VdotN*max(HdotV, 0)/
    (NdotL + sqrt(alpha^2+ (1-alpha^2)*NdotL^2))/
    (VdotN + sqrt(alpha^2+ (1-alpha^2)*VdotN^2))
  f0 <- 0.04*(1-m) + m*Cd
  F <- f0 + (1 - f0)*(1 - HdotV)^5
  f_diffuse <- (1/pi)*(1-F)*(1-m)*Cd
  f_specular <- F*D*G/4/abs(VdotN)/abs(NdotL)
  PBR[j,] <- (f_diffuse + f_specular)*NdotL
  OGL1[j,] <- max(NdotL, 0)*Cd + max(HdotN, 0)^s*Cs
}
plot(phi, PBR[,1], type="l", col = "red", ylim=range(PBR), ylab="PBR")
lines(phi, PBR[,2], col = "green")
lines(phi, PBR[,3], col = "blue")
plot(phi, OGL1[,1], type="l", col = "red", ylim=range(OGL1))
lines(phi, OGL1[,2], col = "green")
lines(phi, OGL1[,3], col = "blue")

