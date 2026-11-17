**Basics of R for Data Science**

This is a GitHub repository that supports the course *Basics of R for Data Science*. This is an introductory course on R that will prepare you for most future methodological courses in the **<a href="https://dottorato.psy.unipd.it/en">PhD program in Psychological Sciences</a>** *(University of Padova)*, where R is extensively used. We will navigate the development environment, work with basic data structures like vectors and dataframes, learn about essential concepts of programming like loops and functions, and see quick ways to plot data (data visualization is covered in the dedicated ggplot2 course). Advanced topics like statistical modelling and data simulation will be covered in future courses and are **not** presented here (though occasional simple examples may appear).

The primary resource for the course is the online book available on GitHub: <a href="https://psicostat.github.io/Introduction2R_EN/" target="_blank">***Introduction2R** (English edition)*</a>.

Find the course website at **<a href="https://enricotoffalini.github.io/Basics-R/" target="_blank">https://enricotoffalini.github.io/Basics-R/</a>**


**How to update the website**

The course website is a Quarto website (`_quarto.yml`). Edit the `.Qmd` sources, then run from the repository root:

```
quarto render
```

The site is written to `docs/` (generated output: do not edit by hand) and computations are stored in `_freeze/`, so only modified files are re-executed. Commit `docs/` and `_freeze/` together with the sources; pushing to `main` publishes `docs/` on GitHub Pages (`.github/workflows/static.yml`; nothing is rendered on GitHub). Datasets and slide PDFs linked for download are listed under `resources:` in `_quarto.yml`: add new downloadable files there. Slide PDFs are made with `Slides/_rendering to pdf.R` after rendering.
