Maintaining the NC Education Research Compendium

Editorial owner proposed by the project sponsor: NCLRN.
Initial content status: all summaries and classifications are drafts.

1. Review the evidence before editing the page

Use data/studies.csv and data/agenda-evidence-crosswalk.csv to review the initial
collection in a spreadsheet. These CSV files are generated review copies.
Changes to them do not automatically alter the website.

The authoritative content lives in data/catalog.json. A web maintainer can apply
approved revisions there and rebuild all pages and exports with one command.
Do not edit generated HTML pages or evidence-data.js directly; rebuilding would
replace those edits.

2. What to collect for an addition

- A complete citation and a stable, public source URL.
- Whether this is a distinct study or a newer version of an indexed study.
- Publication year, or an explicit note if the project summary is undated.
- The NC population, setting, data period, and study design.
- A brief findings summary, practical considerations, limitations, and gaps.
- The review basis: full article, abstract, report summary, or project page.
- A source locator, such as a section, table, or page number.
- Relevant existing agenda IDs and the shared topic tags.
- Researcher IDs and public professional profile/contact sources.
- Source-check date and editorial reviewer/date.

Use the next unused NC-### study ID. Retain IDs when updating a record.
Researchers use stable readable IDs. Do not rename them when an affiliation changes.
Questions retain the agenda's RA-### IDs and exact wording.

3. Distinguish findings from projects

Set hasFindings=false and status="Project record" when no findings have been
reviewed. A grant award, research question, or planned deliverable is not a
finding. A past project end date is not proof of completion or publication.
For preliminary findings, say so and identify whether the accessible source
contains a full technical analysis. Do not infer causal effects from perceptions,
participant feedback, or simple before-and-after comparisons.

Undated NCLRN pages currently cite the known 2022 portfolio announcement and
record year=null. Do not substitute the source-check year as the publication year.

4. Update the question synthesis deliberately

Map studies only where their actual findings address part of the agenda question.
Related vocabulary alone is not enough: ESSER is not Title I, ATR is not National
Board certification, and digital monitoring is not AI-supported instruction.
Add or revise the corresponding entry in syntheses when a new question gains
its first record. State the scope of the answer and what remains uncertain.

Evidence maturity and consistency are separate editorial judgments. Follow the
criteria on about.html. Multiple papers from one project, annual evaluations,
or analyses of the same student data are not automatically independent replications.
The site makes no formal ESSA or agency evidence-strength determination.

5. Review and rebuild

For each record, check that the public source supports the summary and the design
label. Avoid reporting precise numbers from a summary without their limitations.
Confirm source-reported affiliations and current professional contact details.
Record reviewer and review date in editorial notes in the catalog. The current
template displays draft status throughout; changing that site-wide status should
be a deliberate NCLRN editorial decision after review, not an automated update.

Update meta.updated and the relevant source checked dates in catalog.json.
Update the search-window dates and scope statement if the window changes.
Run from the repository root:
  python3 research-compendium/tools/build.py
  python3 research-compendium/tools/verify.py

The build reads the current sibling agenda data, validates mappings, and creates
all static detail pages, the browser data file, CSV exports, and a build summary.
Review the preview on a desktop and a narrow screen; check search, filters,
keyboard focus, source links, downloads, and the question/researcher connections.

6. Publish through the repository's normal process

The initial build is local. Once reviewed and authorized for publication, include
the entire companion folder and preserve its sibling agenda stylesheet and logo.
No external form, analytics, email transmission, or background update runs here.
The contact link opens NCLRN's existing public contact page.

Suggested ongoing cadence (not scheduled):
- Check undated preliminary project pages for final reports first.
- Review new submissions and version updates as NCLRN receives them.
- Recheck all public links and researcher affiliations at least annually.
- Expand coverage using maintenance/backlog.csv and record the search outcome.
- Keep a dated editorial record when a synthesis or classification changes.
