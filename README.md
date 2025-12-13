## Bayesian Dynamic Completion Probabilities


## Overview


## Outcome Probabilities



Formally, consider a play with $T$ total frames and for each $t = 1, \ldots, T,$ let $\boldsymbol{r}_{t}$ and $\boldsymbol{d}_{t}$ be the positions of the **r**eceiver and **d**efender at frame $t.$
For any indices $t < t'$ let $\boldsymbol{r}_{t:t'} = \{\boldsymbol{r}_{t}, \boldsymbol{r}_{t+1}, \ldots, \boldsymbol{r}_{t'}\}$ be the receiver's trajectory from frame $t$ to frame $t'.$
We similarly denote the defender's trajectory between frames $t$ and $t'$ 

let $\boldsymbol{r}_{t}$ be the receiver's position in frame $t$. We wish to compute the posterior distribution of $\boldsymbol{r}_{(t+1):T} \vert \boldsymbol{r}_{1:t}$.


## A Bayesian Model

Following [Miller & Bornn (2017)](https://www.lukebornn.com/papers/miller_ssac_2017.pdf) and [Chu et al. (2019)](https://arxiv.org/pdf/1908.02423), we model receiver and defender trajectories using Bezier curves.
That is, we re-scale the time index to the interval $[0,1]$ and model

$$
\boldsymbol{r}_{t} =\boldsymbol{\phi}(t/T)^{\top}\boldsymbol{B} + \boldsymbol{\epsilon}_{t},
$$
where $\boldsymbol{B}$ is a to-be-estimated $(D+1) \times 2$ matrix; $\boldsymbol{\epsilon}_{t} \sim \mathcal{N}(0, \sigma^{2}I_{2})$ and $\boldsymbol{\phi}(\cdot) = (\phi_{0}(\cdot), \ldots, \phi_{D}(\cdot))^{\top}$ where for each $d = 0, \ldots, D,$ $\phi_{d}$ is the $d$-th Bernstein polynomial
$$
\phi_{d}(u) = \binom{D}{d}u^{d}(1 - u)^{D-d}.
$$


We take a Bayesian approach and specify independent $N(0,1)$ priors for the entries in $\boldsymbol{B}.$
Elementary calculation shows that conditionally on the path taken up to frame $t,$ the posterior distribution of the remaining trajectories is


## Example Output

Consider, for instance, this CeeDee Lamb reception on a 4th and 1 against the Eagles in Week.

Here is an animation showing Lamb's route and the path followed by the defender who was closest to him at the end of the play (). 


## Replication


### Data Preparation
Due to the input data size, the raw trajectory data is not tracked in this repository.
Instead, to reproduce the results, you should first clone his repostiory and download the Big Data Bowl data from Kaggle.
Unpack all the CSV files containing the tracking data and the CSV file containing the supplementary data into the directory `data/training`.
Then run the script `scripts/prepare_data.R`, which will create two data frames: (i) `raw_paths` containing the full trajectories of the targeted receiver and defender for all plays and (ii) `raw_final_frame` containing just those rows in `raw_paths` corresponding to the final frame.
That script saves these two data frames into the file `data/raw_data.RData`. 

### Fitting Completion Probabilities

We fit a simple random forests model to predict the probability that pass results in an interception, incompletion, or completion with a subset of the features used by NGS.
Specifically, we used the quarterback's speed at the time of the pass (`last_qb_s`), `pass_length`, and the total number of frames in the play (i.e., play duration).
From the tracking data in final frame of the play, we computed the distance between the receiver and the nearest sideline (`rec2side`), goalline (`rec2goal`), and defender (`def2rec`).
The script `scripts/fit_rf_model.R` trains the model and saves the fitted model output in the file `data/rf_model.RData`.
We use this fitted model to compute posterior completion, incompletion, and interception probabilities frame-by-frame. 
 
Note, you will need to have the [**ranger**](https://cran.r-project.org/web/packages/ranger/index.html) package installed to run this script.



