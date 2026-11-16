# REDD-Abacus 2

[![R Version](https://img.shields.io/badge/R-%E2%89%A5_4.3.0-blue.svg)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-1.8+-orange.svg)](https://shiny.posit.co/)
[![bslib](https://img.shields.io/badge/Bootstrap-5.3_(bslib)-purple.svg)](https://rstudio.github.io/bslib/)

**REDD-Abacus 2** is an interactive, web-based decision-support system and Shiny application for estimating greenhouse gas (GHG) emissions from land use and land cover change (LULUCF), establishing reference emission baselines (Forest Reference Emission Levels / FREL / BAU), evaluating spatial planning and conservation policy interventions, and analyzing economic trade-offs (opportunity costs and abatement cost curves).

Originally developed as **REDD-Abacus SP** by the **World Agroforestry Centre (ICRAF)**, this updated version brings the full suite of REDD Abacus tools into a modern, reactive R Shiny interface powered by Bootstrap 5, high-performance spatial raster/vector processing (`stars` and `sf`), interactive visualizations (`plotly` and `leaflet`), and dedicated analytical widgets (`abacuslib`).

---

## Table of Contents

- [REDD-Abacus 2](#redd-abacus-2)
  - [Table of Contents](#table-of-contents)
  - [Key Features](#key-features)
  - [Methodological Modules](#methodological-modules)
    - [1. Land Cover Module](#1-land-cover-module)
    - [2. Emission Module](#2-emission-module)
    - [3. Projection Module (FREL \& Scenarios)](#3-projection-module-frel--scenarios)
    - [4. Economic Trade-Off Module (Opportunity Costs)](#4-economic-trade-off-module-opportunity-costs)
  - [System Requirements](#system-requirements)
  - [Installation \& How to Run](#installation--how-to-run)
    - [Method 1: Run Directly from GitHub (Quickest)](#method-1-run-directly-from-github-quickest)
    - [Method 2: Clone and Run Locally (Recommended)](#method-2-clone-and-run-locally-recommended)
    - [Method 3: Standalone Command-Line Execution](#method-3-standalone-command-line-execution)
  - [Dependencies](#dependencies)
  - [Project Structure](#project-structure)
  - [Citation](#citation)
    - [BibTeX](#bibtex)
    - [Origins \& Historical References](#origins--historical-references)
  - [Author \& Development](#author--development)
    - [License](#license)

---

## Key Features

- **Multi-Temporal Spatial Analysis**: Ingest and align multi-year land cover rasters (GeoTIFF) and vector planning units/zonation layers with automatic grid harmonization.
- **ALUCT Transition Dynamics**: Cross-tabulate historical land cover transitions, quantify persistence vs. conversion, and compute Markovian transition probability matrices.
- **IPCC-Compliant Carbon Accounting**: Estimate biomass carbon stock changes (IPCC Tier 2/3 approach) and continuous non-biomass GHG fluxes (peatland drainage, decomposition, soil emissions).
- **Forward-Looking Scenario Modeling**: Simulate future landscape trajectories under Business-As-Usual (BAU) and user-defined policy interventions (e.g., zero-deforestation moratoria, peat restoration, agroforestry expansion).
- **Interactive Multi-Scenario Benchmarking**: Compare cumulative emissions trajectories across multiple scenarios, toggle between Net, Gross, and Sequestration flux types, and evaluate offset percentages.
- **Bottom-Up Abatement Cost Curves**: Perform opportunity cost analysis ($/t CO₂e) to determine the cost-effectiveness of emission reduction options against user-defined carbon price thresholds.
- **Integrated Project Management**: Save, export, and load complete project workspaces (`.zip` format) to ensure reproducible analyses.

---

## Methodological Modules

### 1. Land Cover Module
- **Land Cover Maps**: Ingest categorical GeoTIFF rasters representing multi-temporal land cover distributions alongside spatial zonation strata (e.g., administrative districts, peat soil domains, forest concession boundaries). Spatial operations leverage `stars` and `sf` for memory-efficient raster warping and processing.
- **Land Cover Changes**: Automatically cross-tabulate transitions between any pair of observation periods $(t_1 \rightarrow t_2)$, compute Markovian transition probability matrices $P_{i,j} = A_{i,j} / A_i(t_1)$, track historical landscape area trajectories, and filter transitions by individual or combined planning zones.

### 2. Emission Module
- **Carbon Stock Change Emissions**: Apply the time-averaged carbon stock difference method to evaluate biomass loss and sequestration, converted into carbon dioxide equivalent using stoichiometric ratios:
  $$E_{C, i,j} = - (C_j - C_i) \times \frac{44}{12} \times A_{i,j} \quad (\text{t CO}_2\text{e})$$
- **GHG Flux Emissions**: Account for recurring annual non-biomass soil emissions $F_{i,j}$ (such as peatland oxidation and drainage decomposition) multiplied by observation period duration $T$:
  $$E_{\text{flux}, i,j} = F_{i,j} \times T \times A_{i,j} \quad (\text{t CO}_2\text{e})$$
- **Emissions Summary**: Integrate biomass carbon changes and soil fluxes into total landscape emissions, generate spatial emission intensity maps ($\text{t CO}_2\text{e}/\text{ha}$), and explore transition waterfall summaries.

### 3. Projection Module (FREL & Scenarios)
- **Intervention Scenarios**: Model future land distributions using iterative Markov chain matrix multiplication:
  $$L(t + 1) = L(t) \times P$$
  Establish the historical Business-As-Usual (BAU / Reference Level) baseline, and design alternative policy scenarios by modifying transition probabilities (with automatic row-sum normalization), specifying zone-targeted restrictions, or testing carbon stock adjustments.
- **Projection Summary**: Interactive comparison of cumulative emission trajectories ($E_{\text{cum}}$) across all defined scenarios. Inspect emission offsets ($\Delta E$) and offset percentages relative to baseline, with dynamic filtering by emission type:
  - **Net emission** (overall balance)
  - **Positive emission** (gross emissions from deforestation/degradation)
  - **Negative emission** (carbon sequestration/removals)

### 4. Economic Trade-Off Module (Opportunity Costs)
- **Net Present Value (NPV)**: Assess the lifecycle profitability of each land use system ($\$/\text{ha}$) using discounted cash flows under private or social accounting stances.
- **Abatement Cost Curve**: Construct empirical, bottom-up marginal abatement cost curves (Opportunity Cost Curves). Land transitions are sorted by cost per unit emission avoided ($\$/\text{t CO}_2\text{e}$) against cumulative emissions reduced, identifying "win-win" options (negative opportunity costs) and evaluating financial viability against carbon price cutoffs.

---

## System Requirements

- **R**: Version $\ge 4.3.0$ (tested on R 4.3.x and 4.4.x).
- **Operating System**: Windows, macOS, or Linux.
- **System Spatial Libraries** (primarily Linux/macOS when building from source):
  - `GDAL` ($\ge 3.0$)
  - `GEOS` ($\ge 3.8$)
  - `PROJ` ($\ge 7.0$)
- **Web Browser**: Modern browser (Google Chrome, Mozilla Firefox, Microsoft Edge, or Safari).

---

## Installation & How to Run

### Method 1: Run Directly from GitHub (Quickest)

You can launch the app directly from your R or RStudio console without manually downloading files:

```r
# Ensure shiny is installed
if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}

# Run app directly from GitHub
shiny::runGitHub("redd-abacus", "degi")
```

> **Note**: Required CRAN packages will be verified and loaded automatically on first launch. If any package fails to install automatically, install it manually as shown in the dependencies section below.

---

### Method 2: Clone and Run Locally (Recommended)

Cloning the repository is recommended when working with large local spatial datasets or custom projects.

1. **Clone the repository**:
   ```bash
   git clone https://github.com/degi/redd-abacus.git
   cd redd-abacus
   ```

2. **Install R dependencies**:
   Open R or RStudio in the project folder and run:
   ```r
   # Install CRAN dependencies
   install.packages(c(
     "shiny", "shinyWidgets", "shinyBS", "bslib", "bsicons", "htmltools",
     "reactable", "plotly", "leaflet", "stars", "sf", "mapview",
     "excelR", "RColorBrewer", "jsonlite", "reshape2", "leafem",
     "yaml", "paletteer", "pak"
   ))

   # Install the companion widget library (abacuslib)
   pak::pak("degi/abacuslib")
   ```

3. **Launch the app**:
   ```r
   shiny::runApp()
   ```

---

### Method 3: Standalone Command-Line Execution

You can run the application directly from the system terminal:

```bash
# Using run_abacus.R (sets host to 127.0.0.1 and port to 3838)
Rscript run_abacus.R

# Or using run_app.R
Rscript run_app.R
```

Once running, navigate to:
```text
http://127.0.0.1:3838
```

---

## Dependencies

The application relies on the following R packages:

| Package | Purpose | Source |
| :--- | :--- | :--- |
| `shiny`, `shinyWidgets`, `shinyBS` | Reactive web UI and advanced controls | CRAN |
| `bslib`, `bsicons`, `htmltools` | Modern Bootstrap 5 UI theming and dashboard layouts | CRAN |
| `stars`, `sf`, `mapview`, `leafem` | Spatial raster and vector processing, warping, and mapping | CRAN |
| `leaflet` | Interactive map visualization | CRAN |
| `plotly` | Interactive projection and emission trend charts | CRAN |
| `reactable`, `excelR` | Interactive, editable data tables and spreadsheets | CRAN |
| `jsonlite`, `yaml`, `reshape2` | Data serialization, configuration, and matrix reshaping | CRAN |
| `RColorBrewer`, `paletteer` | Palette and color scale management for maps and plots | CRAN |
| `abacuslib` | Specialized scenario editor and abatement cost curve widgets | [degi/abacuslib](https://github.com/degi/abacuslib) |

---

## Project Structure

```text
redd-abacus/
├── global.R               # Environment setup, library loader, and helper functions
├── ui.R                   # Dashboard layout, navigation panels, and CSS styling
├── server.R               # Core reactive logic, data processors, and calculation observers
├── run_abacus.R           # Command-line launcher script with port configuration
├── run_app.R              # Simple Shiny runApp helper script
├── README.md              # Project documentation and quick-start guide
├── R/
│   ├── abacus_func.R         # Core mathematical and matrix calculation routines
│   ├── abacus_projection.R   # Markov chain forward projection and scenario engines
│   ├── abacus_zones.R        # Spatial zonation aggregation and masking helpers
│   ├── abacus_info_docs.R    # In-app methodological documentation and formulas
│   └── *.tif, *.csv          # Sample spatial rasters and carbon stock datasets
├── docs/
│   └── manual/
│       ├── abacus_manual.md  # Comprehensive REDD Abacus SP user manual
│       └── images/           # Documentation screenshots and workflow diagrams
└── www/                   # Static web assets (CSS stylesheets, logos, icons)
```

---

## Citation

To cite **REDD Abacus 2** in publications, reports, or modeling studies, please use the following citation (APA Style):

> **Asmara, D. H.** REDD Abacus 2: A Tool for Modeling Land Use Opportunity Costs and Emissions Reduction Scenarios. Software available from https://github.com/degi/redd-abacus

### BibTeX

```bibtex
@manual{asmara_redd_abacus2,
  title  = {REDD Abacus 2: A Tool for Modeling Land Use Opportunity Costs and Emissions Reduction Scenarios},
  author = {Asmara, Degi Harja},
  note   = {Software available from \url{https://github.com/degi/redd-abacus}},
  url    = {https://github.com/degi/redd-abacus}
}
```

### Origins & Historical References

REDD Abacus 2 is the modern R/Shiny successor to **REDD Abacus SP**, originally developed at the **World Agroforestry Centre (ICRAF) Southeast Asia Regional Program** in collaboration with Sonya Dewi, Meine van Noordwijk, Andree Ekadinata, and Arief Rahmanulloh. Supported historically through the ALLREDDI (European Commission) and REALU (BMU / Norad) initiatives.

1. **Harja D, Dewi S, van Noordwijk M, Ekadinata A, Rahmanulloh A.** (2011). *REDD Abacus SP - User Manual and Software*. World Agroforestry Centre (ICRAF), Southeast Asia Regional Office, Bogor, Indonesia. 89p.
2. **van Noordwijk M, Dewi S, Suyanto, Minang P, White D, Robiglio V, Hoang MH, Ekadinata A, Mulia R, Harja D.** (2011). *Abatement cost curves relating past greenhouse gas emissions to the economic gains they allowed*. Project Report. ICRAF Southeast Asia.
3. **Ekadinata A, Rahmanulloh A, Pambudhi F, et al.** (2010). *Carbon Emissions from Land Use, Land Use Change and Forestry (LULUCF) in Berau District, East Kalimantan, Indonesia*. ICRAF.
4. **Hairiah K, Dewi S, Agus F, van Noordwijk M, Rahayu S, Velarde SJ.** (2010). *Measuring Carbon Stocks Across Land Use Systems: A Manual*. World Agroforestry Centre (ICRAF), Brawijaya University, and ICALRRD.
5. **IPCC.** (2006). *2006 IPCC Guidelines for National Greenhouse Gas Inventories: Volume 4 Agriculture, Forestry and Other Land Use (AFOLU)*. IGES, Japan.

---

## Author & Development

- **Degi Harja Asmara** — Software Architect, Modeler & Lead Developer  
  Email: [degiharja@gmail.com](mailto:degiharja@gmail.com)  
  GitHub: [@degi](https://github.com/degi)

### License

Open Source & Public Domain for non-commercial research, spatial planning, and education. Proper attribution is appreciated in all derived works.
