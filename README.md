README
================
Robert Kubinec
June 22, 2017

R Package Idealstan (V0.1) README
---------------------------------

This package implements new IRT models (models for absences and for dynamic hiearchical models) along with several standard IRT models (2 PL, rating scale, graded response) designed for modeling rollcall voting data and any other kind of binary or ordinal data. It is based on the Stan package for Bayesian modeling, which includes both MCMC full Bayesian inference and a faster variational Bayesian approximation. The package also has plotting functions for model parameters, particularly the legislator (person) ideal points (ability parameters).

This vignette demonstrates basic usage of the package, which is currently in alpha. A beta release is scheduled for July 13, 2017.

This package takes an approach to modeling legislative roll call data (and other IRT data) that uses a hurdle model to separately account for the probability that a legislator (i.e., test-taker) will show up to vote. This absence-inflated model estimates additional bill (item) parameters to reflect the relative gain or loss a legislator has from showing up to vote. Further details of this model are explained in Kubinec (2017).

To begin with, we can simulate data from an ordinal ideal-point model in which there are three possible responses corresponding to a legislator voting: yes, abstain and no. An additional category is also simulated that indicates whether a legislator shows up to vote or is absent. While traditional ideal point models tend to drop absences, this package can model absences via a hurdle model in which the censoring of the vote/score data is estimated as a function of individual bill intercepts and discrimination parameters for the decision to be absent or present.

``` r
ord_ideal_sim <- id_sim_gen(num_legis=20)
knitr::kable(as_data_frame(head(ord_ideal_sim@vote_matrix)))
```

|    1|    2|    3|    4|    5|    6|    7|    8|    9|   10|   11|   12|   13|   14|   15|   16|   17|   18|   19|   20|   21|   22|   23|   24|   25|   26|   27|   28|   29|   30|   31|   32|   33|   34|   35|   36|   37|   38|   39|   40|   41|   42|   43|   44|   45|   46|   47|   48|   49|   50|
|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|
|    2|    1|    4|    4|    4|    4|    3|    4|    3|    4|    4|    4|    4|    1|    2|    4|    4|    3|    4|    4|    4|    1|    2|    4|    1|    3|    2|    4|    4|    4|    4|    4|    4|    1|    3|    4|    4|    2|    4|    1|    2|    3|    4|    2|    1|    4|    3|    3|    1|    1|
|    4|    4|    4|    4|    3|    3|    4|    4|    1|    3|    2|    4|    4|    3|    3|    3|    1|    2|    1|    1|    2|    3|    4|    1|    4|    1|    1|    4|    4|    1|    3|    4|    3|    1|    4|    3|    4|    4|    4|    4|    3|    3|    2|    4|    1|    1|    4|    4|    4|    2|
|    1|    2|    4|    4|    4|    2|    2|    1|    3|    4|    3|    1|    1|    4|    4|    4|    1|    3|    4|    3|    1|    4|    4|    1|    2|    4|    3|    4|    1|    3|    4|    4|    4|    4|    3|    3|    3|    2|    1|    3|    3|    1|    4|    4|    3|    4|    4|    4|    2|    4|
|    1|    1|    1|    4|    4|    4|    3|    4|    3|    4|    4|    3|    1|    4|    2|    4|    1|    2|    4|    4|    1|    4|    3|    3|    4|    3|    3|    4|    4|    2|    4|    4|    4|    4|    4|    3|    3|    4|    4|    3|    2|    1|    2|    2|    3|    1|    3|    4|    1|    4|
|    2|    2|    4|    4|    4|    3|    4|    3|    3|    4|    3|    1|    1|    1|    4|    3|    1|    3|    1|    4|    4|    4|    4|    4|    4|    3|    2|    4|    4|    3|    3|    4|    4|    1|    4|    4|    3|    1|    4|    3|    3|    4|    1|    3|    3|    1|    2|    1|    1|    3|
|    1|    1|    4|    4|    4|    4|    3|    4|    2|    4|    4|    3|    4|    4|    4|    2|    1|    3|    1|    4|    4|    4|    2|    2|    4|    4|    3|    4|    4|    1|    4|    1|    4|    4|    3|    4|    4|    2|    3|    4|    4|    3|    4|    3|    3|    1|    4|    4|    2|    1|

The vote/score matrix has legislators/persons in the rows and bills/items in the columns. In this simulated data, yes votes are recorded as `3`, no votes as `1`, abstentions as `2`, and absences as `4`.

The function `id_estimate` will take this processed data and run an IRT ideal point model. Currently both inflated and non-inflated versions of the standard binary IRT 2-PL model and the ordinal IRT model (i.e., a rating-scale model) are supported. The package also includes models for hierarchical parameters for legislators/persons and bills/items, along with dynamic IRT models, but these have as yet not been fully tested.

The package has options for identification that are similar to other IRT packages in which the row or column indices of legislators/persons or bills/items to constrain are specified to the `id_estimate` function. For example, we can use the true values of the simulated legislators to constrain two legislators with the highest ideal points and two legislators with the lowest ideal points:

``` r
true_legis <- ord_ideal_sim@simul_data$true_legis
high_leg <- sort(true_legis,decreasing = T,index.return=T)
low_leg <- sort(true_legis,index.return=T)

ord_ideal_est <- id_estimate(idealdata=ord_ideal_sim,
                             model_type=4,
                             fixtype='constrained',
                             restrict_type='constrain_twoway',
                             restrict_ind_high = high_leg$ix[1:2],
                             restrict_ind_low=low_leg$ix[1:2])
```

    ## trying deprecated constructor; please alert package maintainer

Kubinec, Robert. 2017. “Absence Makes the Ideal Points Sharper.” In *2017 Political Methodology Annual Conference*.
