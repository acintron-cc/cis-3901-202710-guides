# CIS-3901 course materials (Fall 2026)

Slides, reports and guides for CIS-3901 Data Visualization and Presentation, Catawba College, Fall 2026 (term code 202710), Modules 10–15.

Published with GitHub Pages: <https://acintron-cc.github.io/cis-3901-202710-guides/>

This repository is separate from the project template `acintron-cc/cis-3901-202710` on purpose: anything in the template is copied into every student's repository.

## Layout

```
_quarto.yml          site settings, navbar (add one line per module)
index.qmd            home page: table of modules
module-10/
  index.qmd          the module's page: links to everything below
  setup-part2.qmd    a guide (rendered to HTML)
  files/             PDFs, images, self-contained HTML: published as they are
data/                datasets shared by several modules (e.g. penguins.csv)
_templates/          slides.qmd and report.qmd to copy; never rendered
_freeze/             saved R results (created by Quarto; commit it)
docs/                the rendered site that GitHub Pages serves (commit it)
```

## Adding a module

1. Create `module-NN/index.qmd` (copy `module-10/index.qmd`).
2. Add slides or reports: copy a file from `_templates/`, or put a PDF in `module-NN/files/`. Link each one from `module-NN/index.qmd`.
3. Add the module to the **Modules** menu in `_quarto.yml` and to the table in `index.qmd`.
4. If you add pages outside `module-NN/`, list them under `render:` in `_quarto.yml` (only listed files are rendered, so README files stay off the site).
5. `quarto render`, then commit and push everything, including `docs/` and `_freeze/`.

Pages settings: Deploy from a branch, `main`, `/docs`.

## Notes

- Pages with R code need the packages they use (render in the Posit Cloud project). `freeze: auto` re-runs a page only when its source changes, so older modules never need re-running.
- The repository is public: everything pushed is visible at once. Keep answer keys out until their release date.
- Course conventions: `%>%` only (never `|>`), US spelling, `fig-alt` on every figure, colorblind-safe palettes.
