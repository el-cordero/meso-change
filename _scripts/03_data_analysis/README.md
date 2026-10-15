# Classification workflows

## R2 reporting audit

Recalculate the corrected eight-class macro F1 and class-specific precision,
recall, and F1 directly from the saved seven-fold confusion matrices with:

```sh
Rscript _scripts/03_data_analysis/15_revision2_metrics_audit.R
```

This reporting-only script does not fit models or predict rasters. It writes
the corrected date-level summaries, paired tests, pooled class metrics, and
arithmetic audits to `_data/Tables/revision2/`, preserving the earlier output
files. Classes with reference support and no correct predictions contribute
zero F1 instead of being omitted. The shared metric helper uses the same
correction for future analysis runs.

## Binary land and surface-water revision

The reviewer-facing binary analysis is the current manuscript workflow. Run it
from the project root with:

```sh
Rscript _scripts/03_data_analysis/run_binary_surface_water.R
```

This workflow reclassifies the reference polygons as land or surface water,
uses an 80:20 stratified split, randomly undersamples the two training classes
to the smaller available class within every fitted model, and leaves validation
data at their observed prevalence. It fits time-stacked baseline and
date-specific random forests for Landsat-9, Sentinel-2, PlanetScope, and the
seven-date PlanetScope hydraulic-comparison series. Existing K-means outputs
are deterministically collapsed to the same two classes so the original
cluster solution is preserved while every downstream product is binary.

The analysis then calculates baseline-relative surface-water change and
compares the seven PlanetScope classifications with the corresponding
hydraulic-model inundation masks. Outputs are isolated in:

- `_data/GIS/Raster/Analysis/Binary/`
- `_data/Models/binary_surface_water/`
- `_data/Cache/binary_surface_water/`
- `_data/Tables/binary_surface_water/`

To recalculate only the cross-sensor Spearman correlations from the saved
22-date surface-water series and the three-gauge mean daily streamflow data,
run:

```sh
Rscript _scripts/03_data_analysis/14_multisensor_surface_water_spearman.R
```

Earlier scripts and results remain available for provenance. Nothing in
`_scripts/03_data_analysis/original/` is modified by the binary workflow.

## Earlier balanced multiclass workflow

This folder contains the authoritative, rerunnable analysis used for the
class-imbalance revision. Earlier script variants are retained unchanged in
`original/` for provenance.

Run the complete analysis from the project root with:

```sh
Rscript _scripts/03_data_analysis/run_all.R
```

The scripts run in this order:

1. `00_config.R` — paths, class definitions, seeds, and model settings.
2. `01_helpers.R` — shared balancing, metric, and raster helpers.
3. `02_landcover_random_forest.R` — balanced Landsat-9, Sentinel-2, and
   PlanetScope land-cover random forests, including baseline tuning and
   date-specific models.
4. `03_mesohabitat_classification.R` — balanced leave-one-date-out and final
   PlanetScope mesohabitat models.
5. `06_finalize_multiclass_metrics.R` — calculates accuracy, kappa, macro
   recall (reported as multiclass balanced accuracy), and macro F1 directly
   from the saved confusion tables.
6. `07_pool_mesohabitat_confusion.R` — pools the seven held-out-date
   mesohabitat confusion matrices by workflow for reviewer-facing audit.
7. `04_postprocess_results.R` — time series, water validation, association
   tests, and manuscript-ready summaries.
8. `05_validate_outputs.R` — confirms equal fitted class support and checks
   that the required balanced outputs were created.

The balancing rule is applied after the validation split. For every fitted
model, each training class is randomly undersampled to the number of pixels in
the smallest available training class. Validation and held-out-date data retain
their observed class distributions.

The mesohabitat workflow checkpoints every leave-one-date-out fold and resumes
from completed checkpoints after an interrupted run. Set `MESO_FORCE_REFIT=1`
to refit all folds and final models from scratch.

New outputs are kept separate from the legacy results:

- `_data/GIS/Raster/Analysis/RandomForest/balanced/`
- `_data/GIS/Raster/Outputs/meso_predictions_balanced/`
- `_data/Models/balanced_analysis/`
- `_data/Tables/balanced_analysis/`

The legacy polygon value `cropland` is normalized to the manuscript term
`agricultural land`. The apparent data-entry typo `watere` is normalized to
`water`.
