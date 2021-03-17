# "Affirmative action, critical mass, and a predictive model of undergraduate student body demographics" Code Repo

This repository provides the implementation of the model constructed and assessed in "Affirmative action, critical mass, and a predictive model of undergraduate student body demographics" by Maes, Tucher, and Topaz (2021). 


## Dependencies

# R Packages
```install.packages("plyr")
install.packages("car")
install.packages("nleqslv")
install.packages("matpow")
install.packages("lhs")
install.packages("magrittr")
install.packages("tidyverse")
install.packages("EnvStats")
```

## Roadmap

Code files should be examined and sourced in the following order:

- existing data from UC-Berkeley and UCLA for each year/racial-ethnic group combination is imported in UCB-Data.R and UCLA-Data.R, respectively
- model parameter rates for the specified college are generated in Predicting-Rates.R
- MC-matrices.R initializes high school data (Markov chain model input) and constructs a Markov chain for each year/racial-ethnic group combination using predicted rates
- MC-Pred-Functions.R iterates over the generated Markov chain model to calculate predictions for the next 10 years of undergraduate demographics
- Projections v2.R implements the critical mass metric and uses the predicted demographic counts to project when colleges will meet critical mass

