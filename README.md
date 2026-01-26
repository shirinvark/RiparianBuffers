# RiparianBuffers

This module computes raster-based riparian influence from hydrological features.
It does **not** make landbase, harvesting, or legal decisions.

The output is intended to be consumed by downstream modules
(e.g. landbase accounting, AAC calculations, or constraint evaluation).

---

## What this module does

- Takes hydrological stream geometry prepared upstream
- Applies either:
  - a uniform riparian buffer, or
  - province-specific buffer rules (policy-driven)
- Produces a **fractional riparian influence raster**

---

## What this module does NOT do

- Does not exclude land
- Does not classify forest vs non-forest
- Does not apply regulatory or management constraints

All interpretation of riparian influence is deferred to downstream modules.

---

## Inputs

- `PlanningRaster` (SpatRaster)  
  Coarse-resolution planning grid supplied by an upstream module.

- `Hydrology$streams` (SpatVector)  
  Stream network geometry.

- `Provinces` (SpatVector, optional)  
  Used only when province-based riparian policy is applied.

---

## Outputs

- `Riparian$riparianFraction`  
  Raster expressing proportional riparian influence.

---

## Status

This module is under active development.
Design choices are being refined in coordination with upstream and downstream modules.

flowchart TD

A[Upstream data preparation\n(outside this module)]

A --> B1[PlanningRaster\nSpatRaster\n~250 m\nSource: EasternCanadaDataPrep]
A --> B2[Hydrology_streams\nSpatVector\nSource: HydroRIVERS]
A --> B3[Provinces\nSpatVector\nprovince_code\nSource: provincial boundaries]

B1 --> C
B2 --> C
B3 --> C

C[RiparianBuffers module]

C --> D1[Validate inputs\n(class and CRS)]
D1 --> D2[Resolve riparian policy\n(user or default)]
D2 --> D3[Build high-resolution hydrology template\n(hydroRaster_m)]
D3 --> D4[Rasterize buffer distances]
D4 --> D5[Distance-to-stream raster]
D5 --> D6[Binary riparian mask\n(distance <= buffer)]
D6 --> D7[Aggregate to planning resolution]

D7 --> E[Outputs]

E --> O1[RiparianFraction\nSpatRaster\nvalues 0 to 1]
E --> O2[RiparianMeta\npolicy and resolution]
