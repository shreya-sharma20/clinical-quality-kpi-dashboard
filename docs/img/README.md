# Screenshots

Drop PNGs here and reference them from the top-level `README.md`:

- `operational.png` — Operational Leader view (volume, LOS, occupancy)
- `clinical.png` — Clinical Leader view (readmissions, O/E LOS, cohorts)
- `methodology.png` — Methodology tab
- `drilldown.png` — a service-line bar click filtering the detail table

Capture at ~1600px wide with the sidebar open. To generate programmatically:

```r
# install.packages("webshot2")
webshot2::webshot("http://127.0.0.1:8080", "docs/img/operational.png",
                  vwidth = 1600, vheight = 1100, delay = 3)
```
