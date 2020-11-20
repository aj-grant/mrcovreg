# mr_covreg
An R package which implements a method for estimation of a causal effect in a Mendelian randomization setting as described in

Grant, A.J. and Burgess, S. (2020). An efficient and robust approach to Mendelian randomization with measured pleiotropic effects in a high-dimensional setting. *Biostatistics*, [https://doi.org/10.1093/biostatistics/kxaa045](https://doi.org/10.1093/biostatistics/kxaa045)

## Installation
```
library(devtools)
install_github("aj-grant/mvcovreg")
```

## Docker
```
docker build -t mrcovreg:latest .
docker run -it mrcovreg R
```

## Applied example
The file Urate_CHD_script.R in the example folder replicates the applied example in the paper.

# Simulations
The simulations folder contains R scripts to replicate the simulations presented in the paper.
