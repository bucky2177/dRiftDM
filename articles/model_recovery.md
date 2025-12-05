# How To Perform a Model Recovery

In this vignette, we demonstrate how to perform a model recovery
analysis. Model recovery is an important component of model validation,
particularly when we are interested in quantitatively comparing models.
The goal is to test whether different models can be distinguished from
one another based on the data they generate, ensuring that the models
are identifiable within a given model space. If, for example, model A
can also provide a good fit to data generated under models B or C, this
suggests that model A may be overly flexible. Such flexibility can limit
the generalizability of the model and make it difficult to disentangle
the competing models on a quantitative level.

A simple way to perform a model recovery analysis is to generate
synthetic data under each model and then fit each model to each data
set. To assess model recovery, we can track which model provides the
best fit to each data set, using a fit statistic that penalizes model
complexity (Vandekerckhove, Matzke, and Wagenmakers 2015), and summarize
the results in a contingency table—also known as a confusion matrix (see
Wilson and Collins 2019).

Here, we provide demonstration code comparing the Ratcliff DDM with a
simplified version of the Diffusion Model for Conflict Tasks (DMC),
using a combination of and . As a fit statistic, we use the Bayesian
Information Criterion (BIC), although the same logic applies to other
fit indices that similarly balance goodness of fit and model complexity
(see, e.g., Vandekerckhove, Matzke, and Wagenmakers 2015).

First, we define each model and specify the parameter spaces used to
generate the synthetic data. Note that we need to slightly customize the
Ratcliff model (see ) so that it includes the same two conditions as the
DMC model (i.e., and ).

``` r
# 1.1) create the Ratcliff model and set new conditions
ratcliff_model <- ratcliff_dm()
conds(ratcliff_model) <- c("comp", "incomp")
#> resetting parameter specifications

# allow the drift rate to vary
instructions <- "muc ~ "

ratcliff_model <- modify_flex_prms(
  object = ratcliff_model,
  instr = instructions
)
print(ratcliff_model)
#> Class(es) ratcliff_dm, drift_dm
#> (model has not been estimated yet)
#> 
#> Parameter Values:
#>        muc   b non_dec
#> comp     3 0.6     0.3
#> incomp   3 0.6     0.3
#> 
#> Parameter Settings:
#>        muc b non_dec
#> comp     1 3       4
#> incomp   2 3       4
#> 
#> Deriving PDFS:
#>   solver: kfe
#>   values: sigma=1, t_max=3, dt=0.0075, dx=0.02, nt=400, nx=100
#> 
#> Cost Function: neg_log_like
#> 
#> Observed Data: NULL



# 1.2) now create DMC (without trial-by-trial variability in the starting point
# and non-decision time; matching with the Ratcliff model)
dmc_model <- dmc_dm(var_non_dec = FALSE, var_start = FALSE)


# 1.3) create the parameter space for generating synthetic data
lower_dmc <- c(muc = 2, b = 0.3, non_dec = 0.2, tau = 0.02, A = 0.02)
upper_dmc <- c(muc = 6, b = 0.8, non_dec = 0.4, tau = 0.15, A = 0.20)

lower_ratcliff <- c(muc = 2, b = 0.3, non_dec = 0.2)
upper_ratcliff <- c(muc = 6, b = 0.8, non_dec = 0.4)
```

Note that the Ratcliff model in this demo comprises four parameters,
whereas the DMC model comprises five parameters.

``` r
coef(ratcliff_model)
#>   muc.comp muc.incomp          b    non_dec 
#>        3.0        3.0        0.6        0.3
coef(dmc_model)
#>     muc       b non_dec     tau       A 
#>    4.00    0.60    0.30    0.04    0.10
```

In a second step, we then generate data under both models. For each
model, we simulated `200` trials per condition and 10 data sets.
Critically, the small number of 10 data sets was chosen to ensure that
this vignette runs relatively fast! In practice, this number has to be
considerably higher! (e.g., `>=100`). Also, the `200` trials were chosen
rather arbitrary. In practice, choose the number of trials in accordance
with the typical trial numbers in your field and/or data sets! Finally,
note that we initially simulated more data under the Ratcliff model and
then chose 10 data sets with `muc` in `incomp` conditions being slightly
smaller than in `comp` condition. This ensures that the data simulated
under the Ratcliff model produces a reasonable congruency effect.

``` r
set.seed(1)
# Data generated under the Ratcliff model
data_ratcliff <- simulate_data(
  ratcliff_model, #  the model
  n = 200, # choose this value reasonably
  k = 100, # more data to filter afterwards
  lower = lower_ratcliff, # the lower parameter space for simulation
  upper = upper_ratcliff # the upper parameter space for simulation
)
# choose data were the drift rate in incompatible conditions is
# reasonably larger than in compatible conditions
prms_ratcliff <- data_ratcliff$prms
diffs <- prms_ratcliff$muc.comp - prms_ratcliff$muc.incomp
filter <- which(diffs < 1.5 & diffs > 0.1)
ids <- prms_ratcliff$ID[filter[1:10]]
data_ratcliff <- data_ratcliff$synth_data # extract the data.frame of raw RTs
data_ratcliff <- data_ratcliff[data_ratcliff$ID %in% ids, ]


# Data generated under the DMC model
data_dmc <- simulate_data(
  dmc_model, #  the model
  n = 200, # choose this value reasonably
  k = 10, # in practice, simulate more data sets!
  lower = lower_dmc, # the lower parameter space for simulation
  upper = upper_dmc # the upper parameter space for simulation
)
data_dmc <- data_dmc$synth_data # extract the data.frame of raw RTs
```

To assess whether the simulated data are reasonably comparable across
conditions, we compute summary statistics of the mean RTs:

``` r
calc_stats(data_ratcliff, type = "basic_stats", level = "group")
#> Type of Statistic: basic_stats
#> 
#>   Source   Cond Mean_corr Mean_err SD_corr SD_err P_corr
#> 1    obs   comp     0.424    0.445   0.062  0.036  0.984
#> 2    obs incomp     0.445    0.437   0.081  0.067  0.973
#> 
#> (access the data.frame's columns/rows as usual)
calc_stats(data_dmc, type = "basic_stats", level = "group")
#> Type of Statistic: basic_stats
#> 
#>   Source   Cond Mean_corr Mean_err SD_corr SD_err P_corr
#> 1    obs   comp     0.400    0.494   0.061  0.100  0.992
#> 2    obs incomp     0.436    0.401   0.069  0.056  0.950
#> 
#> (access the data.frame's columns/rows as usual)
```

In the third step, we fit each model to each data set. For parameter
estimation, we use Differential Evolution with two virtual cores and the
default parameter ranges that dRiftDM provides for each model out of the
box.

``` r
# get the parameter ranges for optimization
# (adapt this if you have a custom model)
l_u_ratcliff <- get_lower_upper(ratcliff_model)
l_u_dmc <- get_lower_upper(dmc_model)


# Fit the Ratcliff model to its data
fits_r_r <- estimate_dm(
  drift_dm_obj = ratcliff_model,
  obs_data = data_ratcliff,
  optimizer = "DEoptim",
  lower = l_u_ratcliff$lower,
  upper = l_u_ratcliff$upper,
  n_cores = 1, # you might want to increase this
  verbose = 0,
  progress = 0,
  messaging = 0 # suppresses all status infos
)

# Fit the Ratcliff model to DMC's data
fits_r_d <- estimate_dm(
  drift_dm_obj = ratcliff_model,
  obs_data = data_dmc,
  optimizer = "DEoptim",
  lower = l_u_ratcliff$lower,
  upper = l_u_ratcliff$upper,
  n_cores = 1, # you might want to increase this
  verbose = 0,
  progress = 0,
  messaging = 0 # suppresses all status infos
)

# Fit the DMC model to its data
fits_d_d <- estimate_dm(
  drift_dm_obj = dmc_model,
  obs_data = data_dmc,
  optimizer = "DEoptim",
  lower = l_u_dmc$lower,
  upper = l_u_dmc$upper,
  n_cores = 1, # you might want to increase this
  verbose = 0,
  progress = 0,
  messaging = 0 # suppresses all status infos
)

# Fit the DMC model to the Ratcliff model's data
fits_d_r <- estimate_dm(
  drift_dm_obj = dmc_model,
  obs_data = data_ratcliff,
  optimizer = "DEoptim",
  lower = l_u_dmc$lower,
  upper = l_u_dmc$upper,
  n_cores = 1, # you might want to increase this
  verbose = 0,
  progress = 0,
  messaging = 0 # suppresses all status infos
)
```

In the fourth step, we determine, for each data set, which model
provides the better fit using the BIC fit statistic.

``` r
# Ratcliff model -> Ratcliff data
stats_r_r <- BIC(fits_r_r, type = "fit_stats")
# DMC model -> Ratcliff data
stats_d_r <- BIC(fits_d_r, type = "fit_stats")
# Ratcliff model -> DMC data
stats_r_d <- BIC(fits_r_d, type = "fit_stats")
# DMC model -> DMC data
stats_d_d <- BIC(fits_d_d, type = "fit_stats")

# ensure the order of fits matches
stopifnot(stats_r_r$ID == stats_d_r$ID)
stopifnot(stats_r_d$ID == stats_d_d$ID)

# now count how often for each data set a model turned out "best"
winner_ratcliff_data <- ifelse(stats_r_r$BIC < stats_d_r$BIC, 1, 2)
winner_ratcliff_data <- factor(winner_ratcliff_data, levels = c(1, 2))
winner_dmc_data <- ifelse(stats_r_d$BIC < stats_d_d$BIC, 1, 2)
winner_dmc_data <- factor(winner_dmc_data, levels = c(1, 2))
```

Finally, we create a contingency table that quantifies the
\`\`probability’’ that a given model fits a data set best, conditional
on which model generated the data (i.e.,
$p\left( \text{fit model} \mid \text{sim model} \right)$).

``` r
# convert to proportions
table_ratcliff <- prop.table(table(winner_ratcliff_data))
table_dmc <- prop.table(table(winner_dmc_data))

# assemble matrix
mat <- rbind(table_ratcliff, table_dmc)
rownames(mat) <- c("sim_r", "sim_d")
colnames(mat) <- c("fit_r", "fit_d")
```

``` r
mat
#>       fit_r fit_d
#> sim_r   1.0   0.0
#> sim_d   0.2   0.8
```

The resulting matrix shows that for data simulated under the Ratcliff
model, the Ratcliff model provided the best fit in all cases. For data
simulated under the DMC model, DMC provided the best fit in 80% of the
cases. Thus, in our small toy example, we demonstrate that the classical
Ratcliff DDM and the DMC model can be distinguished. This is important
because it shows that the non-stationary diffusion models implemented in
dRiftDM can be empirically discriminated from simpler, stationary
diffusion models.[¹](#fn1)

We can go even a step further and convert the confusion matrix into an
*inversion matrix* (see Wilson and Collins 2019). Whereas the confusion
matrix quantifies
$p\left( \text{fit model} \mid \text{sim model} \right)$, the inversion
matrix quantifies
$p\left( \text{sim model} \mid \text{fit model} \right)$—that is, the
probability that data best fit by a given model were actually generated
by another model. Assuming equal prior probabilities across models, the
inversion matrix can be obtained simply by re-normalizing the confusion
matrix across columns.

``` r
inversion_mat <- apply(mat, 2, function(col) col / sum(col))
inversion_mat
#>           fit_r fit_d
#> sim_r 0.8333333     0
#> sim_d 0.1666667     1
```

An important note at the end of this small model recovery example: Model
recovery depends strongly on the settings used to simulate and fit the
synthetic data. If, for example, the simulated parameters are restricted
to a narrow region in which the models under comparison behave
similarly, the resulting data may not vary enough to meaningfully
distinguish the models. Moreover, if the optimization routine is not
well-calibrated or if the simulated data do not reflect the
characteristics of the empirical data to which the models will later be
applied, the conclusions drawn from the recovery analysis may not
generalize to the actual model-fitting context. Thus, it is important to
stress that a model recovery should be designed to closely mirror the
conditions under which the models will ultimately be fitted to empirical
data.

Vandekerckhove, Joachim, Dora Matzke, and Eric-Jan Wagenmakers. 2015.
“Model Comparison and the Principle of Parsimony.” In *The Oxford
Handbook of Computational and Mathematical Psychology*, edited by Wang
Busemeyer J. R., 300–319. Oxford University Press.
<https://doi.org/10.1093/oxfordhb/9780199957996.013.14>.

Wilson, Robert C, and Anne GE Collins. 2019. “Ten Simple Rules for the
Computational Modeling of Behavioral Data.” *eLife* 8: e49547.
<https://doi.org/10.7554/eLife.49547>.

------------------------------------------------------------------------

1.  Note that we also ran the same simulation using a higher number of
    data sets. The results were very similar. The present conclusion of
    discriminability is thus not merely due to a small number of
    simulated data.
