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


## RiparianBuffers – Conceptual Workflow

```mermaid
flowchart TD

A[Upstream Data Preparation<br/>(outside this module)]

A --> B1[PlanningRaster<br/>SpatRaster<br/>~250 m<br/>Source: EasternCanadaDataPrep]
A --> B2[Hydrology_streams<br/>SpatVector<br/>Source: HydroRIVERS / National Hydrography]
A --> B3[Provinces<br/>SpatVector<br/>province_code<br/>Source: Provincial boundaries]

B1 --> C
B2 --> C
B3 --> C

C[RiparianBuffers Module]

C --> D1[1. Validate inputs<br/>(class + CRS)]
D1 --> D2[2. Resolve riparian policy<br/>User-supplied OR default buffer]
D2 --> D3[3. Build high-resolution hydrology template<br/>(hydroRaster_m)]
D3 --> D4[4. Rasterize province-based buffer distances]
D4 --> D5[5. Compute distance-to-stream raster]
D5 --> D6[6. Binary riparian mask<br/>(distance ≤ buffer)]
D6 --> D7[7. Aggregate to planning resolution]

D7 --> E[Outputs]

E --> O1[RiparianFraction<br/>SpatRaster<br/>Values 0–1]
E --> O2[RiparianMeta<br/>Policy + resolution]

