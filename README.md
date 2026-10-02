# EMBL-EBI Causality in biomedicine: going beyond associations

# Block on time-to-event analysis

Time-to-event analysis using longitudinal TMLE to assess effect of a possibly changing with time intervention on a time-to-event outcome.

## Requirements

- **R** version 4.5.0 or compatible (the exact version is recorded in 'renv.lock')
- **RStudio** (recommended)
- **Rtools** - **Windows users only.** Required to build packages from source, including the GitHub-hosted package '[LtAtStructuR]' (required to structure time-stamped data in the standard long format). Install the version matching your R release (e.g. Rtools45 for R 4.5.x): <https://cran.r-project.org/bin/windows/Rtools/>
- macOS users need the Xcode command line tools ('xcode-select --install')

## Setup and reproducibility

This project uses [`renv`](https://rstudio.github.io/renv/) to manage its package environment, so the exact set of packages used can be reproduced.

1.  **Clone the Git repository:** e.g.,

``` bash
git clone https://github.com/alinakumukova/time-to-event.git
cd time-to-event
```

2.  **Open the project** by double-clicking [time-to-event.Rproj] in RStudio. 'renv' will bootstrap automatically the first time.

3.  **Restore the package environment.** In the R console, run:

```         
renv::restore()
```

This will install all required packages (with dependencies) at the recorded versions. (This step might take a few minutes the first time)

4.  **Run the analysis.** 

## Key required packages

- renv
- tidyverse
- ltmle
- sl3
- SuperLearner
- survival
- knitr and bookdown (for compilation, not necessary for running code)

## Contact

Alina Kumukova - [alina.kumukova\@ed.ac.uk](mailto:alina.kumukova@ed.ac.uk){.email}
