# rENM.core

**Core infrastructure for the rENM Framework**

## Overview
`rENM.core` provides the foundational utilities that standardize how rENM projects are structured, accessed, and queried. It defines the minimal, stable layer on which all higher-level modules (analysis, reporting, AI) depend.

This package is intentionally lightweight and focused on **project organization, data discovery, and reproducibility** rather than modeling or analysis.

## Role in the rENM Framework
Within the modular rENM ecosystem, `rENM.core`:
- Defines the canonical **project directory structure**
- Provides consistent access to **species metadata and variables**
- Standardizes **path resolution** across all downstream packages
- Ensures **reproducible, CRAN-compliant workflows** (no hard-coded paths)

All other rENM packages assume and depend on this layer.

## Key Functions
- `rENM_project_dir()` — Resolve the active rENM project root
- `get_species_info()` — Retrieve species-level metadata
- `show_species()` — Inspect available species in a project
- `show_variables()` — Inspect available environmental predictors

## Design Principles
- **Minimal and stable**: core utilities change rarely
- **Strict separation of concerns**: no modeling, no analysis logic
- **Reproducibility-first**: standardized paths and structure
- **Framework alignment**: enforces consistency across modules

## Installation
```r
# install from source
devtools::install_local("rENM.core")
```

## Example
```r
library(rENM.core)

# locate project
proj <- rENM_project_dir()

# list species
show_species()

# inspect variables
show_variables()
```

## Relationship to Other Packages
- `rENM.analysis` — modeling and trend computation
- `rENM.reports` — maps, summaries, and reports
- `rENM.ai` — AI-driven synthesis and interpretation

`rENM.core` underpins all of the above.

## License
See `LICENSE` for details.

---

**rENM Framework**  
A modular system for reconstructing and analyzing long-term ecological niche dynamics in climate space.
