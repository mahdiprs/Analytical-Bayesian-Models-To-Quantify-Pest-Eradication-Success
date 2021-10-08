# Analytical-Bayesian-Models-To-Quantify-Pest-Eradication-Success
An R implementation of models developed in "Analytical Bayesian models to quantify pest eradication success or species 
absence using zero-sighting records" by B. Barnes, M. Parsa, F. Giannini and D. Ramsey (to appear in Theoretical Population Biology)

In this paper we use Bayesian methods to formulate analytical inferred distributions and statistics to provide accessible and versatile formulations to support an assessment of population absence for management decisions, using data from a series of regular and targeted surveys with zero-sightings.  Analytical solutions formulated include the inferred mean and variance for population size or number of infested survey-units, the probability of absence, the probability of a series of negative surveys conditional on presence, and the probability a population is first detected in a given survey, although we also formulate other statistics and provide explicit thresholds designed to support management decisions. 

Keywords: Bayesian models; analytical statistics; stochastic growth; zero-sightings; pest eradication success; 
population absence; biosecurity; probability generating functions;

# How to run the models
Two implementations of models are included for estimating various statistics developed in the paper. The first implementation uses numerical approximation for calculating the derivatives, and the second one uses symbolic algebra. We also developed shiny apps to visually explore the dynamics of the system by changing its building parameters. To run the models see exampleNumerical.R and exampleSymbolic.R.      
