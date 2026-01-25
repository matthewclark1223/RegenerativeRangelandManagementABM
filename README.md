# RegenerativeRangelandManagementABM

This repository contains the code for the agent-based model and experiments used in the study ["Eliminating climate change mitigation trade-offs in African rangelands,"](https://www.researchsquare.com/article/rs-7750461/v1) which explores how pairing forecasts with conservation programming can reduce trade-offs among vegetation cover, livestock numbers, and wealth inequality, supporting effective adaptation to climate change at scale.

## Repository structure
- **RunModel.R**: Code for the agent-based model (ABM). Sources landscape files from `StylizedLandscape/` and precipitation from `PrecipTimeseries.R` to create the initial landscape and generate precipitation scenarios. Runtime for one full setting across the three climate scenarios is ~45 minutes. It also supports a toy quickstart via environment variables.
- **RunToy.R**: Code for a toy run of ``RunModel.R``.
- **GeneratePlots.R**: Code to reproduce visualizations for experiments 1-3 and supplementals from the manuscript. It expects ABM outputs in a Ts_Data-style folder (see below).
- **ExperimentConditions.rds**, **ConditionsExp1.rds**: Predefined experimental settings for running the ABM, used in manuscript experiments 1 to 3 and supplemental material (forecast settings, provision of supplemental fodder, social learning settings, etc.). The file ``ConditionsExp1.rds`` includes the conditions for experiment 1 only to run the toy example.
- **StylizedLandscape/**: Scripts and inputs for creating the stylized landscape.
- **AncillaryScripts/**: Ancillary scripts and inputs used for deriving parameters and settings for the ABM. To run `PredictedRainfall.R`, download [precipitation projection data](https://cds.climate.copernicus.eu/datasets/projections-cmip6?tab=download).

## Prerequisites
- R ≥ 4.1
- Download the file [povmap-grdi-v1.tif](https://syncandshare.lrz.de/getlink/fiPw7h8k9QaACopENWok9m/) and place it inside the `StylizedLandscape/` folder.
- Packages for ABM and visualizations:
```r
packages <- c("cowplot", "data.table", "doParallel", "doRNG", "dplyr", "foreach", "forcats", "fs", "ggh4x", "ggplot2", "ggthemes", "glue", "gstat", "ineq", "iterators", "lubridate", "parallel", "purrr", "raster", "readr", "rngtools", "sf", "sp", "spData", "spdep", "stringr", "tibble", "tidyr", "tidyverse", "vroom")

install.packages(setdiff(packages, rownames(installed.packages())))
```


## Toy example (quickstart)
The repo includes a small toy run that reproduces the core pipeline without the full ABM runtime. If you keep the plot pane open, you’ll see the stylized landscape update as the run progresses.

**What it does**
- Runs a minimal ABM configuration (one repetition of 100 timesteps across 3 precipitation scenarios, using only the conditions from experiment 1) and writes the outputs to `Ts_Data_toy/`.
- Generates ABM burn-in plot into `Manuscript_Vis_toy/`.

1. Clone this repository to your local machine:
```bash
  git clone https://github.com/matthewclark1223/RegenerativeRangelandManagementABM.git
  cd RegenerativeRangelandManagementABM
```

2. Run the toy model:
```bash
  Rscript RunToy.R
```

## Reproduce the full experiments

**Note:**
If you have previously run `RunToy.R` in the same R session, **restart your R session before proceeding**, as settings from the toy configuration may persist.


1. Clone this repository to your local machine:
  ```bash
    git clone https://github.com/matthewclark1223/RegenerativeRangelandManagementABM.git
    cd RegenerativeRangelandManagementABM
 ```

2. Generate data

*Option A (full run. This will take several days to run on a standard desktop):*
```bash
  Rscript RunModel.R
```

*Option B (use pregenerated data):*

Download [Ts_Data](https://syncandshare.lrz.de/getlink/fiPw7h8k9QaACopENWok9m/) and place the folder in the repo root next to the scripts.

3. Analyze and plot:
```bash
  # default expects Ts_Data and writes to Manuscript_Vis
  Rscript GeneratePlots.R
```

## How to customize the agent-based model

We encourage others to build on our model to answer new questions. Specifically, you might:

1. Modify the model in `RunModel.R`. Sections in the script correspond to the modules shown in figure 2 of the manuscript (e.g., T0 set-up/initialization of landscape, grazing, etc.).
2. Play with settings (e.g., animal cost, capital gain rate, etc.) in the `RunModel.R` section `Model parameters`.
3. Adjust plots in GeneratePlots.R (facets, normalization, themes).

# Citation

If you use this code in your work, please cite:

```bibtex
@article{clark2025eliminating,
  title        = {Eliminating climate change mitigation trade-offs in African rangelands},
  author       = {Clark, Matt and Fröhner, Cosima and Jørgensen, Andreas and Pienkowski, Thomas and Yekela, Sibabalo and Isacs, Aamirah and Crowe, Olivia and Andrews, Jeffrey and Smaldino, Paul and Gallizioli, Iacopo and Arena, Gina and Mills, Morena},
  year         = {2025},
  month        = {oct},
  note         = {PREPRINT (Version 1) available at Research Square},
  doi          = {10.21203/rs.3.rs-7750461/v1},
  url          = {https://doi.org/10.21203/rs.3.rs-7750461/v1}
}
