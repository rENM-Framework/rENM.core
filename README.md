# rENM.core

![rENM](https://img.shields.io/badge/rENM-framework-blue)
![module](https://img.shields.io/badge/module-core-informational)

**Core infrastructure for the rENM Framework**

## Overview

`rENM.core` provides the foundational utilities used throughout an rENM
analysis. It is intentionally lightweight, focused on **project organization,
metadata discovery, and reproducibility** rather than modeling or analysis.
All other rENM packages depend on this one.

## Key functions

| Function | Description |
|---|---|
| `rENM_project_dir()` | Resolve the active rENM project root |
| `get_species_info()` | Retrieve standardized metadata for a species by alpha code |
| `show_species()` | Inspect the species table for a project |
| `show_variables()` | Inspect the environmental variables table for a project |

## Installation

```r
# From GitHub
devtools::install_github("rENM-Framework/rENM.core")

# From a local source directory
devtools::install_local("rENM.core")
```

## Getting started

All rENM functions that access project files need to know where your project
directory is. The recommended approach for scripts is to pass it explicitly:

```r
library(rENM.core)

proj <- "/path/to/your/rENM/project"

# Inspect species and variables defined for this project
show_species(project_dir = proj)
show_variables(project_dir = proj)

# Retrieve metadata for a single species by its four-letter alpha code
casp <- get_species_info("CASP", project_dir = proj)
```

For interactive work, you can set the project directory once per session so
you don't have to pass it everywhere:

```r
options(rENM.project_dir = "/path/to/your/rENM/project")
# or
Sys.setenv(RENM_PROJECT_DIR = "/path/to/your/rENM/project")

# Functions then resolve the directory automatically
show_species()
get_species_info("CASP")
```

See `?rENM_project_dir` for the full resolution precedence and persistent
configuration options.

## Project directory structure

`rENM.core` expects the following layout within the project root:

```
<project_dir>/
└── data/
    ├── _species.csv     # species metadata table
    └── _variables.csv   # environmental variables metadata table
```

## Role in the rENM framework

`rENM.core` sits at the base of the pipeline. All downstream packages
(`rENM.data`, `rENM.model`, `rENM.analysis`, `rENM.ai`, `rENM.reports`)
use it for project-directory resolution and metadata access. Changes to
the conventions established here propagate across the entire framework.

## License

See `LICENSE` for details.

---

**rENM Framework** — A modular system for reconstructing and analyzing
long-term ecological niche dynamics.
