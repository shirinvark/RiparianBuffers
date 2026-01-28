s

<sub><em>Fractional riparian influence from hydrology and buffer policy</em></sub>

🎯 Purpose

Generate a fractional riparian influence raster (0–1) from hydrological streams and buffer rules.

Nothing else.

✅ What this module does

Produces a continuous riparian signal at planning resolution

Supports uniform or province-based buffer distances

Preserves sub-cell riparian structure via high-resolution processing

Outputs are reusable across harvesting, habitat, and AAC workflows

🚫 What this module explicitly does NOT do

Landbase definition

Harvest or regulatory exclusions

Forest / non-forest classification

Management or policy interpretation

These decisions are intentionally downstream.

📥 Required inputs (supplied upstream)
Object	Class	Role
PlanningRaster	SpatRaster	Target planning grid
Hydrology_streams	SpatVector	Stream network
Provinces	SpatVector	Jurisdictions (province_code)
⚖️ Buffer policy handling

Exactly one buffering strategy is used per run:

Option A — Uniform buffer

Single distance applied everywhere (baseline / testing).

Option B — Province-based policy (typical use)
data.frame(
  province_code = c("ON", "QC", "NB"),
  buffer_m      = c(300, 300, 300)
)


If no policy is supplied, a conservative default is used.

Buffer distances are applied geometrically only.
No regulatory meaning is inferred.

⚙️ Core computation (high-level)

Validate spatial inputs

Resolve buffer strategy

Compute distance-to-stream (high resolution)

Convert distance ≤ buffer → riparian mask

Aggregate to planning grid → fraction (0–1)

No thresholds.
No exclusions.
No interpretation.

📤 Output
sim$Riparian

Element	Description
riparianFraction	Fractional riparian influence (0–1)
raster_m	Resolution used for computation
policy	Buffer policy applied

Value meaning

0 → no riparian influence

1 → fully riparian

0–1 → partial influence

🔁 Design principle

This module generates signal, not decisions.

Keeping riparian influence continuous and policy-agnostic ensures:

transparent assumptions

easier review

flexible downstream use

🧭 Data flow
flowchart LR
Hydrology_streams --> RiparianBuffers
Provinces --> RiparianBuffers
PlanningRaster --> RiparianBuffers
RiparianBuffers --> RiparianFraction
