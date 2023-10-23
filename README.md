# edi-survey-code

This repository stores the anonymised data and data analyses for the survey sent out by *PsychoPy* to gauge equity, diversity, and inclusion among its own user base. But the raw data is not stored here.

Assuming you somehow have access to the raw data, the `.qmd` files must be run in a particular order in order to render the appropriate output. Run the `tidyingAndFormatting.qmd` file first. Then run both `summariesAndVisualisations.qmd` and `codebook.qmd`, but not necessarily in that order. These files are designed to render `.html` output, nothing else.