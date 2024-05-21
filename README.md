# The InterModel Vigorish (IMV) and IRT

## Getting started

### The IMV
Key details regarding the use of the IMV when using item response theory (IRT) models to analyze dichotomous item repsonses are discussed below. For a generic introduction to the IMV concept and for illustrations of how it can be used more generally to model binary outcomes, see [here](https://github.com/ben-domingue/imv).

### The `irtimv` package

The `irtimv` package gets used at various points and will need to be installed to replicate results.
```
devtools::install_github("ben-domingue/irtimv/irtimv-helper", ref="main")
```

## A basic example
We offer a few basic examples of how to compute the IMV. The `imv` function [here](https://github.com/ben-domingue/irtimv/blob/main/examples/imv_function.R) can be used to compute the IMV with either [simulated](https://github.com/ben-domingue/irtimv/blob/main/examples/simulated_example.R) or [empirical](https://github.com/ben-domingue/irtimv/blob/main/examples/empirical_example.R) data.

## Reproducing results from the paper

### The simulations

Simulation code is [here](https://github.com/ben-domingue/irtimv/tree/main/sim). Code for simulation studies is largely self-contained (outside of usage of `irtimv` package). Figures from the main text are created in the following files

- f1: misfit.R
- f2: misfit.R
- f3: altmetric.R

Figures from the SI:
- f1: pvalue.R
- f2: misfit2.R
- f3: misfit_bysumscore.R
- f4: theta.R
- f5: prior_discrimination.R
- f6: prior.R
- f7: Nsim4.R
- f8: Nsim3.R
- f9: fuzzy.R
- f10: altmetric.R 
- f12: multidimensional.R

### Empirical examples
Code supporting work with empirical data is [here](https://github.com/ben-domingue/irtimv/tree/main/irw). This paper uses data from the Item Response Warehouse ([IRW](https://datapages.github.io/irw/)). Figures from the main text are produced via:

- 02_complexity.R computes unidimensional results.
- 03_mirt.R comptues multidimensional results.
- 04_figure.R produces figure 3.


