NC Education Research Compendium
Companion to the NCDPI Research & Policy Priorities directory

Status: working collection; latest addition October 27, 2026.

Open index.html or serve the parent orpp-public folder with a local web server.
The site uses static HTML, CSS, and JavaScript and can run on GitHub Pages.
There is no build-time package install, account system, remote API, or database.

Current collection:
- 33 study/project records: 31 with findings and 2 project-only records.
- 34 researcher profiles, using public, source-reported affiliations.
- Six priorities and all 17 topics from the current agenda.
- A coverage map for all 62 supporting questions and areas of interest.
- Search, combined filters, shareable filtered links, and filtered CSV exports.
- Individual priority, topic, question, study, and researcher pages.

The initial collection is intentionally described as a structured starting search,
not a complete literature review. It includes some abstract-only and preliminary
project sources, explicitly labeled on each record. Researcher availability and
current affiliations have not been confirmed with the people listed.

Main pages:
  index.html          Evidence directory and priority overview
  researchers.html    Searchable researcher directory
  coverage.html       Complete agenda-to-evidence map
  about.html          Scope, classifications, interpretation, and stewardship

Source and maintenance files:
  data/catalog.json               Canonical editable content
  data/studies.csv                Generated study review export
  data/researchers.csv            Generated researcher review export
  data/agenda-evidence-crosswalk.csv  Generated agenda map
  maintenance/README.txt          Editing and review workflow
  maintenance/research-log.json   Search scope, exclusions, and limitations
  maintenance/backlog.csv         Priority follow-up searches
  tools/build.py                 Static-page and export generator
  tools/verify.py                Content and internal-link verification

Regenerate from the repository root:
  python3 research-compendium/tools/build.py
  python3 research-compendium/tools/verify.py

The source agenda is ../ncdpi-research-agenda/agenda_data.json. The companion
imports the existing agenda stylesheet and logo so that the two sites stay
visually related. Keep the sibling agenda folder available when moving the site.

Public site: https://sbkellogg.github.io/orpp-public/research-compendium/
New local additions require the repository’s normal publication process.

Before treating this as an official NCDPI or NCLRN publication, complete the
owners' content and accessibility review and applicable agency web process.
