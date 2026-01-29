# RiparianBuffers
A SpaDES module for generating fractional riparian influence

Overview

RiparianBuffers computes a fractional riparian influence raster (values between 0 and 1) from hydrological features and buffer-distance rules.

The module is policy-aware but decision-free:
it generates a continuous spatial signal only and deliberately avoids landbase, harvesting, or management assumptions.

Why this module exists

Riparian effects are often implemented as:

binary exclusions, or

hard-coded rules embedded in landbase logic.

These approaches hide assumptions and limit reuse.

RiparianBuffers separates concerns by translating hydrological geometry and buffer rules into a transparent, reusable spatial signal that downstream modules can interpret as needed.

What this module does

✔ Computes fractional riparian influence (0–1) at planning resolution
✔ Uses streams and lakes as hydrological sources
✔ Supports uniform or province-based buffer distances
✔ Preserves sub-cell riparian structure via higher-resolution processing
✔ Produces outputs reusable across harvesting, habitat, and AAC workflows

What this module does not do

✘ Define landbase or effective harvestable area
✘ Apply harvest, regulatory, or conservation exclusions
✘ Classify forest / non-forest land
✘ Interpret policy or management intent

All of these are explicitly downstream responsibilities.

Inputs (supplied upstream)

All spatial preparation (download, cropping, reprojection, filtering) is expected to occur upstream
(e.g. in EasternCanadaDataPrep).

Object	Class	Description
PlanningRaster	SpatRaster	Target planning grid
Hydrology_streams	SpatVector	Stream / river network
Hydrology_lakes	SpatVector	Lakes and large water bodies
Provinces	SpatVector	Jurisdiction boundaries (province_code)
Buffer policy handling

Exactly one buffering strategy is used per simulation.

Uniform buffer

A single buffer distance applied everywhere
(useful for baseline testing or sensitivity analysis).

Province-based policy

A table mapping jurisdictions to buffer distances:

data.frame(
  province_code = c("ON", "QC", "NB"),
  buffer_m      = c(300, 300, 300)
)


If no policy is supplied, a conservative default is applied.

Note:
Buffer distances are applied geometrically only and carry no regulatory interpretation.

Core computation (high level)

Validate spatial inputs (class and CRS consistency)

Resolve buffering strategy (uniform or policy-based)

Rasterize hydrology (streams + lakes) at higher resolution

Compute distance-to-hydrology

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

0–1 → partial riparian influence

Downstream modules decide how this signal is used.

Design principle

RiparianBuffers generates signal, not decisions.

This design improves transparency, reuse, and auditability across landscape modelling workflows.