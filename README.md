# orpp-public

Public repository for charts, data, reports, and web tools from the NCDPI Office of Research and Promising Practices (ORPP). Most items are static HTML pages that open in a browser; one folder contains a Shiny app, and several folders contain the R and Python scripts used to produce the outputs.

## Contents

### Reports and interactive outputs

| Folder | Description |
|---|---|
| [attendance](attendance/) | 2025 AttendNC "bright spots" report, interactive map, and summary tables, including a view filtered to a 50% decrease |
| [attendnc-counts](attendnc-counts/) | AttendNC reports by district (67), region (8), RESA (3), and typology (10) |
| [enrollments](enrollments/) | Private K–12 enrollment share estimates for 2011 and 2021 (US and NC), with the R scripts, Quarto source, and national enrollment data used to build them |
| [working-conditions](working-conditions/) | Teacher working conditions maps and summary tables, including mean and percent breakdowns by subgroup |

### Applications

| Folder | Description |
|---|---|
| [bright-spots-app](bright-spots-app/) | Shiny app for exploring 2025 attendance bright spots. Contains the app code (`ui.R`, `server.R`, `global.R`), prepared map data, and a deployment record for shinyapps.io |

### Research and policy resources

| Folder | Description |
|---|---|
| [ncdpi-research-agenda](ncdpi-research-agenda/) | NCDPI research and policy priorities directory: searchable agenda, office directory, agenda development background, structured source data, and a crosswalk for editing in Excel |
| [research-compendium](research-compendium/) | Draft NC Education Research Compendium: question syntheses, study records, researcher directory, coverage map, and filtered CSV exports. Companion to the research agenda; marked as a working draft pending content and accessibility review |

## Using the outputs

Open any `.html` file in a modern browser. Pages that include a `_files` folder need that folder alongside them. The research agenda and compendium run without an internet connection, though external links require one. The Shiny app requires R with the packages listed in its code, or the shinyapps.io deployment.

To regenerate the research compendium, run from the repository root:

```
python3 research-compendium/tools/build.py
python3 research-compendium/tools/verify.py
```

## Status

- The research agenda and compendium are working drafts. Neither has been published as an official NCDPI or NCLRN site, and both need owner content review and agency web approval before that.
- Office leaders listed in the research agenda should be rechecked before public deployment.
- Office-to-topic relationships in the agenda are suggestions, not approved assignments.

## Data sources

Enrollment comparisons use NCES Digest Table 203.40 for public school denominators and Private School Survey (PSS) microdata for private school counts, as documented in the enrollment scripts. Source notes for the attendance and working conditions outputs are not yet summarized here; check each folder's files and the research agenda's background page.
