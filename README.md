# CA1 Data Exploration
- Author: Carlos Eduardo Menezes
- Student Number: 2023252
- Lecturer: Muhammad Iqbal

---

## What this project is about

It is often assumed that countries "get cleaner as they get richer": as an economy grows, its electricity should move away from coal, oil and gas. This project tests that assumption using 23 years of country-level energy data.

**Research question:** *How has the electricity mix of the world's countries changed between 2000 and 2022, and which countries break the expected link between wealth, energy use and clean electricity?*

Domain: **Energy**. Language: **R** (tidyverse + ggplot2).

## What I found, in short

- **Richer countries use more energy, very predictably.** On a log-log scale the slope is 1.19 (R² = 0.89): a country that is 1% richer uses about 1.2% more energy.
- **Richer countries do not reliably have cleaner electricity.** The rank correlation between GDP per person and low-carbon share is about zero (ρ = −0.05). The relationship is U-shaped: some of the poorest countries (Ethiopia, DR Congo, Nepal) and some of the richest (Norway, Iceland, Switzerland) both run on more than 90% low-carbon power. The poorer ones get there through legacy hydro and low demand; the richer ones through hydro, nuclear and wind.
- **Only "new" renewables follow wealth.** Wind + solar share does rise with income (ρ = 0.38).
- **Progress can go backwards.** Denmark (+65 points, wind) and Sierra Leone (+84, hydro) decarbonised the most, while Congo, Tanzania and Ghana lost 55–78 points of low-carbon share as gas and oil met fast-growing demand.
- **A data-quality flag:** gas-based Turkmenistan and Uzbekistan report a higher grid carbon intensity than coal-heavy Poland, which doesn't fit lifecycle emission factors for gas. I kept these rows but flagged them.
- **PCA compresses 12 features into 5 components (≈80% of the variance).** PC1 measures development and energy intensity; PC2 measures how carbon-intensive the grid is (oil/diesel grids at one end, hydro and wind at the other).

## Dataset

[Our World in Data – Energy dataset](https://github.com/owid/energy-data) (`owid-energy-data.csv`), which brings together Ember, the Energy Institute Statistical Review, the U.S. EIA and Maddison Project GDP. The codebook (`owid-energy-codebook.csv`) describes every column and its source.

|               | Raw                     | After cleaning       |
|---------------|-------------------------|----------------------|
| Rows          | 23,377                  | 3,783                |
| Columns       | 130                     | 34 (25 + 9 encoded)  |
| Entities      | 314 (incl. aggregates)  | 165 countries        |
| Years         | 1900–2025               | 2000–2022            |
| Missing cells | 65.7%                   | 0                    |

One row = one country in one year.

## Repository structure

```
CA1/
├── CA1.R                          # The full analysis: preparation, EDA, encoding, PCA
├── CA1.Rproj                      # RStudio project (sets the working directory)
├── owid-energy-data.csv           # Raw dataset
├── owid-energy-codebook.csv       # Column definitions and sources
├── data/
│   ├── owid_energy_clean_encoded.csv   # Clean dataset with encodings (script output)
│   └── pca_scores.csv                  # Principal component scores (script output)
├── figures/                       # 16 charts used on the poster (script output)
├── CA1_Poster.pptx                # Poster, built on the module template
└── README.md
```

## How to run it

1. Open `CA1.Rproj` in RStudio so that the paths resolve.
2. Install the packages once:
   ```r
   install.packages(c("tidyverse", "naniar", "countrycode", "corrplot",
                      "factoextra", "GGally", "fastDummies"))
   ```
3. Source `CA1.R` (Ctrl + Shift + S). It takes under a minute, prints its findings to the console, and writes all charts to `figures/` and the clean data to `data/`.

## How the analysis works

The script is split into sections that match the four marking criteria.

### 1. Data preparation

Most of the effort went here, because the raw file is messy in ways that are easy to miss:

1. **Aggregates mixed in with countries.** Rows such as "World", "Africa (EI)" or "High-income countries" sit in the same table as real countries. They have no ISO code, so they were removed; leaving them in would double-count everything.
2. **Coverage changes over time.** Before 2000, electricity-mix data exists for only about 55 countries, and GDP stops after 2022. That sets the 2000–2022 window (see `figures/01_coverage_by_year.png`).
3. **GDP missing for whole countries.** 55 small territories have no GDP at all. They were dropped rather than imputed: with nothing to base an estimate on, imputation would mean inventing data. Because this depends on country size, it is *missing not at random* and is listed as a limitation.
4. **Missing values that are really zeros.** When a source share (e.g. wind) is missing, the shares that *are* present already add up to 100% in every case (243 rows). So a missing value means "this country has no plant of that type", and the true value is 0. The script tests this before filling anything in. Mean imputation would have pushed the totals above 100%.
5. **Leading gaps.** Serbia and Montenegro report separately only after their 2006 split. Filling those early years would be extrapolation, not interpolation, so those 7 rows were dropped.
6. **Outliers.** Turkey's IQR rule flags 259 energy-per-person values on the raw scale and **none** on a log10 scale. The "outliers" (Iceland, Qatar, Norway…) are genuine energy systems produced by a heavily skewed variable, so they were kept, and log10 transforms were used for the skewed variables.

### 2. Exploratory data analysis

Five supporting questions, each answered with a chart:

| | Question | Figure |
|---|---|---|
| Q1 | How does energy use per person vary across regions? | `04_q1_energy_by_region.png` |
| Q2 | Do richer countries use more energy, and who breaks the trend? | `05_q2_wealth_vs_energy.png` |
| Q3 | How has the electricity mix shifted since 2000? | `08_q3_mix_trend.png` |
| Q4 | Do richer countries have cleaner electricity? | `09_q4_wealth_vs_low_carbon.png`, `10_q4b_route_by_income.png` |
| Q5 | Which countries decarbonised, and which went backwards? | `11_q5_biggest_movers.png` |

Covariation is summarised with a Spearman correlation heatmap and a pairs plot. Spearman was used because it is robust to outliers and to the many zero shares.

### 3. Encoding and PCA

- **One-hot encoding** for `region` (7 World Bank regions → 7 binary columns). Region has no natural order, so label encoding (1–7) would invent one.
- **Ordinal label encoding** for `energy_use_tier` (quartiles: Low = 1 … High = 4). Here the order is real, so integers keep information instead of inventing it.
- **PCA** on 12 standardised features: the 8 source shares, log10 GDP, energy and electricity per person, and carbon intensity. Standardising stops carbon intensity (0–1,300) from dominating through its units alone.
  - The Kaiser rule keeps 5 components (79.8% of variance); the 80% rule keeps 6 (87.4%).
  - The last eigenvalue is ~0 because the shares always sum to 100%. PCA detected this redundancy on its own.
  - Signs of components are arbitrary, so the script fixes their direction (`orient_component()`) to keep "high PC1 = richer" true on every run.
  - A sensitivity check showed that adding the region dummies to the PCA lowers the variance captured by PC1 + PC2 from 49% to 37%, so they were kept out and used only to compare regions.

## A note on code style

- **Data contracts.** After every preparation step, a `stopifnot()` check asserts what should now be true (no aggregates left, shares sum to 100, each row has exactly one region, at least 3,000 rows remain). They work like unit tests for the data: if an assumption breaks, the script stops at that line instead of quietly producing misleading charts.
- **Readable over clever.** Variable names are descriptive, every meaningful block is commented, and where one construct was chosen over an equivalent one (e.g. `max.col()` instead of looping with `apply()`), a comment explains why.
- **Reproducible.** A fixed seed, relative paths through the R project, saved outputs, and `sessionInfo()` at the end.

## Limitations and next steps

- Country-years are not independent: the same country appears 23 times.
- Regional averages treat every country equally; they are not weighted by how much electricity each produces.
- Small territories without GDP are excluded, so the results describe the 165 countries that have it.
- A few carbon-intensity values look implausible and are flagged rather than corrected.

Natural next steps: a generation-weighted analysis, clustering countries on their PC scores, and log-ratio PCA, which is designed for data that sums to 100%.

## References

- Aitchison, J. (1982) 'The statistical analysis of compositional data', *Journal of the Royal Statistical Society: Series B*, 44(2), pp. 139–177.
- Arel-Bundock, V., Enevoldsen, N. and Yetman, C. (2018) 'countrycode: an R package to convert country names and country codes', *Journal of Open Source Software*, 3(28), p. 848.
- Bolt, J. and van Zanden, J.L. (2023) *Maddison Project Database, version 2023*. Groningen Growth and Development Centre.
- Ember (2026) *Yearly Electricity Data*. Available at: https://ember-energy.org/data/yearly-electricity-data/
- Energy Institute (2025) *Statistical Review of World Energy 2025*. London: Energy Institute.
- Jolliffe, I.T. and Cadima, J. (2016) 'Principal component analysis: a review and recent developments', *Philosophical Transactions of the Royal Society A*, 374(2065), 20150202.
- Kaiser, H.F. (1960) 'The application of electronic computers to factor analysis', *Educational and Psychological Measurement*, 20(1), pp. 141–151.
- Our World in Data (2025) *Energy dataset*. Available at: https://github.com/owid/energy-data
- Schlömer, S. et al. (2014) 'Annex III: Technology-specific cost and performance parameters', in *IPCC Climate Change 2014: Mitigation of Climate Change*. Cambridge University Press, pp. 1329–1356.
- Tukey, J.W. (1977) *Exploratory Data Analysis*. Reading, MA: Addison-Wesley.
- Wickham, H. et al. (2019) 'Welcome to the tidyverse', *Journal of Open Source Software*, 4(43), p. 1686.
