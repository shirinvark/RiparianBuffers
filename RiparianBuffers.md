---
title: "RiparianBuffers Manual"
subtitle: "v.0.0.0.9000"
date: "Last updated: 2026-01-26"
output:
  bookdown::html_document2:
    toc: true
    toc_float: true
    theme: sandstone
    number_sections: false
    df_print: paged
    keep_md: yes
editor_options:
  chunk_output_type: console
link-citations: true
always_allow_html: true
---
## Overview

This module translates hydrological stream geometry into a continuous spatial
signal representing riparian influence.

The module is intentionally **policy-aware but decision-neutral**.

---

## Design philosophy

Riparian influence is treated as a physical signal rather than a binary constraint.

This design allows:
- comparison across jurisdictions
- sensitivity analysis
- deferred interpretation in downstream modules

---

## Province-based riparian policy

Jurisdictional differences in riparian regulations are expressed
as a simple policy table with the following fields:

- `province_code`
- `buffer_m`

This approach avoids embedding policy logic directly into hydrological processing
and enables transparent modification or scenario testing.

---

## Outputs

The primary output is a raster expressing proportional riparian influence,
which can be interpreted differently depending on downstream use cases.

---

## RiparianBuffers – workflow (simplified)

PlanningRaster (SpatRaster)
        |
        v
Hydrology_streams (SpatVector)
        |
        v
[Validate inputs & CRS]
        |
        v
[Resolve riparian policy]
  (default OR province-based)
        |
        v
[Create high-resolution hydrology template]
  (resolution = hydroRaster_m)
        |
        v
[Rasterize province buffer widths]
        |
        v
[Distance-to-stream raster]
        |
        v
[Binary riparian mask (distance ≤ buffer)]
        |
        v
[Aggregate to planning resolution]
        |
        v
RiparianFraction (fractional raster)
