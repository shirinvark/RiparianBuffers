# RiparianBuffers
*A SpaDES module for generating fractional riparian influence*

---

## Overview

**RiparianBuffers** computes a **fractional riparian influence raster (0–1)** from hydrological stream geometry and buffer-distance rules.

The module is **policy-aware but decision-free**:  
it generates a spatial signal only and deliberately avoids landbase or management assumptions.

---

## Why this module exists

Riparian constraints are often implemented as:
- binary exclusions, or
- hard-coded rules embedded in landbase logic.

That approach hides assumptions and limits reuse.

**RiparianBuffers separates concerns** by translating hydrology and buffer rules into a **continuous spatial signal** that downstream modules can interpret as needed.

---

## What this module does

✔ Computes **fractional riparian influence (0–1)** at planning resolution  
✔ Supports **uniform** or **province-based** buffer distances  
✔ Preserves **sub-cell riparian structure** using higher-resolution processing  
✔ Produces outputs reusable across harvesting, habitat, and AAC workflows  

---

## What this module does *not* do

✘ Define landbase or effective harvestable area  
✘ Apply harvest or regulatory exclusions  
✘ Classify forest / non-forest land  
✘ Interpret policy or management intent  

All of these are **explicitly downstream responsibilities**.

---

## Inputs (supplied upstream)

| Object | Class | Description |
|------|------|-------------|
| `PlanningRaster` | `SpatRaster` | Target planning grid |
| `Hydrology_streams` | `SpatVector` | Stream / river network |
| `Provinces` | `SpatVector` | Jurisdiction boundaries (`province_code`) |

---

## Buffer policy handling

Exactly **one buffering strategy** is used per simulation.

### Uniform buffer
A single buffer distance applied everywhere  
(useful for baseline testing or sensitivity analysis).

### Province-based policy
A table mapping jurisdictions to buffer distances:

```r
data.frame(
  province_code = c("ON", "QC", "NB"),
  buffer_m      = c(300, 300, 300)
)
If no policy is supplied, a conservative default is applied.

Note
Buffer distances are applied geometrically only and carry no regulatory interpretation.

Core computation (high level)
Validate spatial inputs (class and CRS)

Resolve buffering strategy

Compute distance-to-stream at high resolution

Convert distance ≤ buffer to a riparian mask

Aggregate to planning resolution as fractional cover (0–1)

No thresholds.
No exclusions.
No interpretation.

Outputs
The module produces a single structured output:

sim$Riparian
Element	Description
riparianFraction	Fractional riparian influence (0–1)
raster_m	Resolution used for computation
policy	Buffer policy applied
Interpretation
0 → no riparian influence

1 → fully riparian

0–1 → partial influence

Downstream modules decide how this signal is used.

Design principle
RiparianBuffers generates signal, not decisions.

This design improves transparency, reuse, and auditability across modelling workflows.

