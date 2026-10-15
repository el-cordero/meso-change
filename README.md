# ML-Mesohabitats

Satellite-based surface-water change detection and chained mesohabitat
classification at the Kansas–Big Blue River confluence.

## Analysis

The current workflows and run instructions are in
[`_scripts/03_data_analysis/README.md`](_scripts/03_data_analysis/README.md).
They cover class-balanced training, binary surface-water comparisons,
mesohabitat classification, confusion matrices, and the R2 reporting audit.
Earlier analysis scripts are preserved in
[`_scripts/03_data_analysis/original/`](_scripts/03_data_analysis/original/).

## Manuscript and figures

The `_docs` submodule contains the manuscript, reviewer comment matrix,
figure scripts, and figure outputs. Clone both repositories with:

```sh
git clone --recurse-submodules https://github.com/el-cordero/ML-Mesohabitats.git
```

Raw data, fitted models, generated analysis tables, and local submission
packages are excluded from this repository. Running the workflows requires
the corresponding local `_data` inputs described in the scripts.
