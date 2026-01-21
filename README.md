# EasternCanadaHydrology

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
