# CA1 video script (target 3:45, hard limit 4:00)

Record with the poster full-screen (zoom into each column as you go), then switch to RStudio for the code part. Aim for about 140 words a minute.

---

**[0:00-0:30] Intro: poster header + column 1 top**
"Hi, I'm [name]. My project asks one question: do countries get cleaner electricity as they get richer? I used the Our World in Data energy dataset, which combines Ember, the Energy Institute and Maddison GDP data. I looked at 165 countries from 2000 to 2022."

**[0:30-1:10] Data preparation: column 1**
"The raw file has 23,000 rows, 130 columns and 66% missing cells, so preparation was the main challenge. First, regions like 'World' are stored as rows next to countries, so I removed every row without an ISO code. Second, electricity data only becomes global in 2000 and GDP stops in 2022, which set my time window. The key finding was about missing source shares: in every one of those rows, the known shares already added up to 100%. So a missing value meant the country has no plant of that type, and I filled it with zero. I tested that assumption in code before imputing. Finally, the outliers like Iceland and Qatar disappear on a log scale, so they are real extremes, not errors, and I kept them."

**[1:10-2:00] EDA: column 2 + column 4 top**
"Energy use rises with wealth almost perfectly: the log-log slope is 1.19 with an R-squared of 0.89. The unusual pattern is that wealth does not predict clean electricity at all; the rank correlation is about zero. Both the poorest countries, like Ethiopia and DR Congo, and the richest, like Norway and Iceland, run on over 90% low-carbon power. The poor ones get there through old hydro dams and low demand; the rich ones through hydro, nuclear and wind. Over time, Denmark and Sierra Leone made big gains, but Congo, Ghana and Tanzania went backwards as gas met growing demand. I also flagged a data-quality issue: Turkmenistan's gas grid reports a higher carbon intensity than coal-based Poland, which doesn't fit the physics."

**[2:00-2:50] Encoding + PCA: column 3**
"For encoding, region has no order, so I one-hot encoded it into seven binary columns. Energy-use tier does have an order, low to high, so ordinal label encoding fits it. For PCA I standardised 12 features so that units don't drive the result. The Kaiser rule keeps 5 components, covering 80% of the variance. PC1 is development: GDP, energy and electricity use. PC2 is grid carbon intensity: oil-based grids in the Sahel at one end, Denmark and Iceland at the other. The last eigenvalue is zero because the shares always add to 100%, and PCA found that redundancy by itself. Adding the region dummies made the components worse, so I left them out."

**[2:50-3:35] Code overview: switch to RStudio, scroll CA1.R**
"The script follows the poster: section 1 prepares the data, section 2 is the EDA, section 3 is encoding and PCA. After each cleaning step there's a stopifnot check that acts like a unit test: if an assumption about the data is wrong, the script stops there instead of producing misleading charts. Here's the structural-zero test, the one-hot check that each row belongs to exactly one region, and the helper that fixes the sign of each principal component so the interpretation stays stable. Every figure is saved to the figures folder, and the clean data is exported for reproducibility."

**[3:35-3:50] Conclusion: column 4**
"In short: richer countries use more energy, but not necessarily cleaner energy, and progress can reverse. Next steps would be generation-weighted analysis and clustering on the PCA scores. Thanks for watching."
