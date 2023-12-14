# edi-survey-code

This repository stores the anonymized data and data analyses for the survey sent out by *PsychoPy* to gauge equity, diversity, and inclusion among its own user base. But the raw data is not stored here.

Assuming you have access to the raw `.csv` file,[^1] the `.qmd` files must be run in a particular order in order to render the appropriate output. Run the `tidyingAndFormatting.qmd` file first. Then run both `summariesAndVisualisations.qmd` and `codebook.qmd`, but not necessarily in that order. 

If you do not have access to the raw `.csv` file, then you just need either to run `Render Website` (under the `Build` tab in *RStudio* or run `quarto render` in the *Terminal*.

These files are designed to render `.html` output, nothing else.

Note that in the code cells labelled *getAndAttachPackages* near the top of each `qmd` file, there is a small block of convenience code to install packages that you need, but you have not yet installed. This block of code must be un-commented before it will function.

The link to the report itself is as follows:

[https://psychopy.github.io/psychopy-edi-survey/](https://psychopy.github.io/psychopy-edi-survey/)

[^1]: The raw `.csv` file is neither on this repository nor publicly available.