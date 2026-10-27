#!/usr/bin/env python3
"""Build the static evidence directory and review exports. Python standard library only."""
import csv
from datetime import date
import html
import io
import json
import re
from pathlib import Path
from urllib.parse import quote

ROOT = Path(__file__).resolve().parents[1]
AGENDA = ROOT.parent / 'ncdpi-research-agenda' / 'agenda_data.json'
catalog = json.loads((ROOT / 'data/catalog.json').read_text())
agenda = json.loads(AGENDA.read_text())
studies = catalog['studies']
programs = sorted(catalog.get('programs', []), key=lambda p: p['name'])
program_map = {p['id']: p for p in programs}
people = catalog['researchers']
questions = [q for q in agenda['items'] if q['kind'] in ('Supporting Question', 'Area of Interest')]
question_map = {q['id']: q for q in questions}
people_map = {p['id']: p for p in people}
study_map = {s['id']: s for s in studies}
priorities = agenda['priorities']
topics = agenda['topics']

def e(value):
    return html.escape(str(value or ''), quote=True)

def slug(value):
    return re.sub(r'[^a-z0-9]+', '-', value.lower()).strip('-')

def unique(values):
    return list(dict.fromkeys(values))

def display_date(value):
    parsed = date.fromisoformat(value)
    return parsed.strftime('%B') + f' {parsed.day}, {parsed.year}'

updated_label = display_date(catalog['meta']['updated'])

def related(qid):
    return [s for s in studies if qid in s['questions']]

def findings(records):
    return [s for s in records if s['hasFindings']]

for q in questions:
    records = related(q['id'])
    q['studyIds'] = [s['id'] for s in records]
    q['evidenceCount'] = len(findings(records))
    q['projectCount'] = len(records) - q['evidenceCount']
    q['synthesis'] = catalog['syntheses'].get(q['id'])
    if records:
        assert q['synthesis'], f'Missing synthesis: {q["id"]}'
        q['maturity'] = q['synthesis']['maturity']
        q['consistency'] = q['synthesis']['consistency']
    else:
        q['maturity'] = 'Not yet indexed'
        q['consistency'] = 'Not assessed'
    q['url'] = f'questions/{q["id"]}.html'
for s in studies:
    s['priorities'] = sorted({question_map[q]['priority'] for q in s['questions']})
    s['url'] = f'studies/{s["id"]}.html'
for p in people:
    records = [s for s in studies if p['id'] in s['researchers']]
    p['studyIds'] = [s['id'] for s in records]
    p['topics'] = unique(t for s in records for t in s['topics'])
    p['priorities'] = sorted({n for s in records for n in s['priorities']})
    p['pageUrl'] = f'researchers/{p["id"]}.html'

def validate():
    for group in (studies, people, questions, programs):
        assert len({x['id'] for x in group}) == len(group), 'Duplicate IDs'
    for s in studies:
        links = s.get('programs', [])
        assert len({p['id'] for p in links}) == len(links), s['id']
        assert all(p['id'] in program_map and p['relationship'] in ('direct', 'related') and p['note'] for p in links), s['id']
        assert s['questions'] and all(q in question_map for q in s['questions']), s['id']
        assert set(s['topics']) <= set(topics), s['id']
        assert all(p in people_map for p in s['researchers']), s['id']
        assert s['sources'] and all(x['url'].startswith('https://') for x in s['sources']), s['id']
        assert date.fromisoformat(s['checked']) <= date.fromisoformat(catalog['meta']['updated']), s['id']
        assert s['year'] is None or int(catalog['meta']['windowStart'][:4]) <= s['year'] <= int(catalog['meta']['windowEnd'][:4]), s['id']
        assert s['year'] is not None or s.get('dateNote'), s['id']
    assert all(p['studyIds'] for p in people), 'Researcher without linked work'
    for p in programs:
        assert any(p['id'] == link['id'] for s in studies for link in s.get('programs', [])), p['id']
        assert not p.get('parentId') or p['parentId'] in program_map, p['id']
        assert p['authorityUrl'].startswith('https://'), p['id']
    for qid in catalog['syntheses']:
        assert qid in question_map and related(qid), f'Orphan synthesis: {qid}'
    assert len(topics) == 17 and len(priorities) == 6

validate()

def badge(text, kind='neutral'):
    return f'<span class="badge {kind}">{e(text)}</span>'

def maturity_badge(value):
    return badge(value, {'Substantial':'substantial', 'Emerging':'emerging', 'Project only':'project', 'Not yet indexed':'neutral'}[value])

def priority_badges(numbers):
    return ''.join(f'<span class="pill priority-pill" data-priority="{n}">Priority {n}</span>' for n in numbers)

def topic_links(names, prefix=''):
    return ''.join(f'<a class="tag" href="{prefix}topics/{slug(t)}.html">{e(t)}</a>' for t in names)

def date_label(s):
    return str(s['year']) if s['year'] else 'Undated summary'

def program_links(s, prefix=''):
    return ''.join(f'<a class="program-tag" href="{prefix}programs/{p["id"]}.html">{e(program_map[p["id"]]["name"])} <span>· {"Direct" if p["relationship"] == "direct" else "Related"} evidence</span></a>' for p in s.get('programs', []))

def program_export(s):
    return '; '.join(f'{program_map[p["id"]]["name"]} ({p["relationship"]}): {p["note"]}' for p in s.get('programs', []))

def question_card(q, prefix=''):
    syn = q['synthesis']
    summary = syn['summary'] if syn else 'No eligible study has been indexed for this question in the initial collection. This is a coverage gap in the directory, not a finding that no research exists.'
    return f'''<article class="evidence-card question-card" id="question-{q['id']}" data-record="{q['id']}">
<div class="card-main"><div class="card-kicker">{priority_badges([q['priority']])}{maturity_badge(q['maturity'])}<span>{q['id']}</span></div>
<h3><a href="{prefix}{q['url']}">{e(syn['headline'] if syn else q['title'])}</a></h3><p class="question-label">{e(q['title'])}</p><p class="desc">{e(summary)}</p></div>
<div class="card-bottom"><span>{q['evidenceCount']} evidence record{'s' if q['evidenceCount'] != 1 else ''}{f' · {q["projectCount"]} project record' if q['projectCount'] else ''}</span><a href="{prefix}{q['url']}">Explore the question <span aria-hidden="true">→</span></a></div></article>'''

def study_card(s, prefix=''):
    status_kind = 'project' if not s['hasFindings'] else ('preliminary' if s['status'] == 'Preliminary findings' else 'neutral')
    return f'''<article class="evidence-card study-card" id="study-{s['id']}" data-record="{s['id']}"><div class="card-main">
<div class="card-kicker">{badge(s['status'], status_kind)}<span>{e(date_label(s))} · {s['id']}</span></div><h3><a href="{prefix}{s['url']}">{e(s['title'])}</a></h3>
<p class="authors">{e(s['authors'])}</p><p class="desc">{e(s['findings'])}</p><div class="tagline">{topic_links(s['topics'][:3], prefix)}</div><div class="program-tags">{program_links(s, prefix)}</div></div>
<div class="card-bottom"><span>{e(s['design'])}</span><a href="{prefix}{s['url']}">Read the evidence <span aria-hidden="true">→</span></a></div></article>'''

def person_card(p, prefix=''):
    initials = ''.join(x[0] for x in p['name'].split() if x not in ('C.', 'A.', 'F.', 'S.', 'W.', 'G.', 'E.', 'L.', 'D.'))[:2]
    return f'''<article class="evidence-card person-card" id="person-{p['id']}" data-record="{p['id']}"><div class="card-main"><div class="person-top"><span class="initials" aria-hidden="true">{e(initials)}</span><div><h3><a href="{prefix}{p['pageUrl']}">{e(p['name'])}</a></h3><p class="institution">{e(p['institution'])}</p></div></div><p class="desc">{e(p['focus'])}</p><div class="tagline">{topic_links(p['topics'][:3],prefix)}</div></div><div class="card-bottom"><span>{len(p['studyIds'])} linked record{'s' if len(p['studyIds']) != 1 else ''}</span><a href="{prefix}{p['pageUrl']}">View researcher <span aria-hidden="true">→</span></a></div></article>'''

def shell(title, body, prefix='', current='', description='', script=False, hero=''):
    nav = [('index.html#directory','Evidence Directory','evidence'),('researchers.html','Researchers','researchers'),('about.html','About the Evidence','about')]
    links = ''
    for url, label, key in nav:
        attr = ' aria-current="page"' if current == key else ''
        links += f'<a href="{prefix}{url}"{attr}>{label}</a>'
    return f'''<!doctype html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1"><meta name="theme-color" content="#092342"><meta name="description" content="{e(description or 'Explore North Carolina education evidence, research questions, and researcher connections. A companion to NCDPI research priorities.')}"><title>{e(title)} | NC Education Research Compendium</title><link rel="stylesheet" href="{prefix}styles.css"></head>
<body><a class="skip" href="#main">Skip to main content</a><header><div class="top"><a class="brand" href="{prefix}index.html" aria-label="NC Education Research Compendium home"><img class="brand-logo" src="{prefix}../ncdpi-research-agenda/orpp-logo-full-white.png" alt="Office of Research and Promising Practices, North Carolina Department of Public Instruction" width="2128" height="376"></a><nav class="topnav" aria-label="Main navigation">{links}<a href="{prefix}../ncdpi-research-agenda/">Research Priorities <span aria-hidden="true">↗</span></a></nav></div></header>
<div class="collection-bar"><div><span class="draft-dot" aria-hidden="true"></span><strong>Working collection</strong><span>Draft summaries for content review</span><a href="{prefix}about.html#scope">Collection updated {updated_label}</a></div></div>
{hero}<main id="main" class="content">{body}</main>
<footer class="footer"><div class="footer-in"><div><strong>NC Education Research Compendium</strong><p>A companion to NCDPI’s Research &amp; Policy Priorities.<br>Prepared for stewardship by the NC Learning Research Network.</p></div><nav aria-label="Footer"><a href="{prefix}about.html">How to use the evidence</a><a href="{prefix}coverage.html">Collection coverage</a><a href="{prefix}data/studies.csv" download>Download study records</a><a href="https://nclrn.unc.edu/contact-us/">Connect with NCLRN <span aria-hidden="true">↗</span></a></nav></div><p class="footer-note">Draft editorial summaries and classifications do not imply agency endorsement, a systematic review, or an ESSA evidence rating.</p></footer>
<script src="{prefix}data/evidence-data.js"></script>{f'<script src="{prefix}app.js"></script>' if script else ''}</body></html>'''

def write(path, content):
    target = ROOT / path
    target.parent.mkdir(parents=True, exist_ok=True)
    target.write_text(content)

def breadcrumbs(parts, prefix='../'):
    items = ['<nav class="breadcrumbs" aria-label="Breadcrumb"><a href="'+prefix+'index.html">Evidence home</a>']
    for label, url in parts:
        items.append('<span aria-hidden="true">/</span>')
        items.append(f'<a href="{e(url)}">{e(label)}</a>' if url else f'<span>{e(label)}</span>')
    return ''.join(items)+'</nav>'

def detail_section(title, text, cls=''):
    return f'<section class="detail-section {cls}"><h2>{e(title)}</h2><p>{e(text)}</p></section>'

def cards_grid(cards):
    return '<div class="items">'+''.join(cards)+'</div>'

def source_list(s):
    return '<ul class="sources">'+''.join(f'<li><a href="{e(x["url"])}">{e(x["title"])} <span aria-hidden="true">↗</span></a><span>{e(x["basis"])} · {e(x["locator"])}</span></li>' for x in s['sources'])+'</ul>'

def filter_html(mode='evidence'):
    is_people = mode == 'researchers'
    statuses = sorted({s['status'] for s in studies})
    extra = f'''<div class="field"><label for="institution">Institution</label><select id="institution"><option value="">All institutions</option>{''.join(f'<option>{e(i)}</option>' for i in sorted({p['institution'] for p in people}))}</select></div>''' if is_people else f'''<div class="field"><label id="statusLabel" for="status">Publication / project status</label><select id="status"><option value="">All statuses</option>{''.join(f'<option>{e(s)}</option>' for s in statuses)}</select></div>'''
    view = '' if is_people else '''<fieldset class="view-switch"><legend class="sr-only">Browse the directory by</legend><label><input type="radio" name="view" value="studies" checked><span>Studies &amp; Projects</span></label><label><input type="radio" name="view" value="questions"><span>Questions &amp; Findings</span></label></fieldset>'''
    sort_field = '' if is_people else '''<div class="field"><label for="sort">Sort results</label><select id="sort"><option value="priority">Priority, then title</option><option value="title">Title A–Z</option><option value="newest">Newest reviewed output first</option></select></div>'''
    lower = '' if is_people else '''<div class="secondary-filters" id="questionFilters" hidden><div class="field" id="consistencyField" hidden><label for="consistency">How findings compare</label><select id="consistency" disabled><option value="">All patterns</option><option>Aligned within indexed evidence</option><option>Mixed / context-dependent</option><option>Limited comparison</option><option>Not assessed</option></select></div><label class="checkbox" id="gapsLabel" hidden><input type="checkbox" id="gaps" disabled> Include questions awaiting evidence review</label></div>'''
    program_filters = f'''<div class="secondary-filters program-filters"><div class="field"><label for="program">Program or initiative</label><select id="program" aria-describedby="programHelp"><option value="">All programs &amp; initiatives</option>{''.join(f'<option value="{p["id"]}">{e(p["name"])}</option>' for p in programs)}</select></div><div class="field" id="connectionField" hidden><label for="connection">Evidence connection</label><select id="connection" disabled><option value="">Direct &amp; related evidence</option><option value="direct">Direct evidence only</option><option value="related">Related evidence only</option></select></div>{sort_field}</div><p class="filter-help" id="programHelp">Programs shown have records in this collection. <a href="about.html#programs">About program connections</a></p>'''
    return f'''{view}<div class="filter-card"><div class="filter-grid"><div class="field"><label for="search">{'Search researchers, institutions, or expertise' if is_people else 'Search questions, findings, researchers, or keywords'}</label><input type="search" id="search" placeholder="{'Try: literacy, coaching, UNC Charlotte…' if is_people else 'Try: attendance, early college, teacher retention…'}" autocomplete="off"></div><div class="field"><label for="priority">Research priority</label><select id="priority"><option value="">All six priorities</option>{''.join(f'<option value="{i}">{i}. {e(p)}</option>' for i,p in enumerate(priorities,1))}</select></div>{extra}</div>{program_filters}{lower}<div class="topic-head"><strong>Explore by topic <span id="topicCount"></span></strong><button type="button" class="text-btn" id="clear">Clear all filters</button></div><div class="chips" role="group" aria-label="Topic filters">{''.join(f'<button type="button" class="chip" data-topic="{e(t)}" aria-pressed="false">{e(t)}</button>' for t in topics)}</div></div>'''

def toolbar(noun):
    return f'''<div class="toolbar"><p class="num" id="resultMessage" role="status" aria-live="polite"><strong id="resultCount">0</strong> <span id="resultNoun">{noun}</span><span id="activeSummary"></span></p><div class="actions"><button type="button" class="btn" id="copyLink">Copy filtered link</button><button type="button" class="btn primary" id="export">Export filtered CSV</button></div></div><div id="empty" class="empty" hidden><h3>No matches for these filters</h3><p>Try a broader keyword, fewer filters, or another topic.</p><button type="button" class="btn" id="emptyClear">Clear all filters</button></div>'''

hero = f'''<section class="hero"><div class="hero-inner"><div><div class="eyebrow">NC Education Research Compendium</div><h1>Connecting NC Research<br>to Policy and Practice</h1><p>Explore what we know about North Carolina education, see where questions remain, and connect with the people behind the research.</p><div class="hero-actions"><a class="btn hero-btn" href="#directory">Explore the evidence <span aria-hidden="true">↓</span></a><a class="hero-link" href="researchers.html">Find a researcher <span aria-hidden="true">→</span></a></div></div></div><div class="collection-stats"><div><span><strong>{len(studies)}</strong> studies &amp; projects</span><span><strong>{len(people)}</strong> researcher profiles</span><span><strong>6</strong> shared research priorities</span></div></div></section>'''
priority_intros = [
    'Follow evidence from early learning through advanced coursework and postsecondary transitions.',
    'Explore what research says about staffing, working conditions, coaching, and educator stability.',
    'Examine program findings alongside implementation, variation in outcomes, and cost questions.',
    'Connect evidence on engagement, school climate, health, family relationships, and discipline.',
    'Explore the validity, comparability, and practical use of information for educational decisions.',
    'Consider how funding, access, and organizational policies shape opportunities and outcomes.'
]
priority_cards=[]
for i,p in enumerate(priorities,1):
    count=len([s for s in studies if i in s['priorities'] and s['hasFindings']])
    priority_cards.append(f'<article class="priority-card" data-priority="{i}"><div class="priority-number">Priority {i} <span>{count} evidence record{"s" if count != 1 else ""}</span></div><h3>{e(p)}</h3><p>{priority_intros[i-1]}</p><a href="priorities/{i}.html">Explore this priority <span aria-hidden="true">→</span></a></article>')
home=f'''<section class="priority-overview" aria-labelledby="priorities-heading"><div class="section-title"><div><p class="section-eyebrow">Shared questions. Connected evidence.</p><h2 id="priorities-heading">Research Priority Areas</h2></div><a class="quiet-link" href="coverage.html">View collection coverage <span aria-hidden="true">→</span></a></div><p class="section-intro">The same six priorities as NCDPI’s research agenda, viewed through the findings already available. Explore a priority for question summaries, original sources, and researcher connections.</p><div class="priority-grid">{''.join(priority_cards)}</div></section>
<section id="directory" aria-labelledby="directory-heading" data-directory="evidence"><div class="section-title"><div><p class="section-eyebrow">From a question to the research</p><h2 id="directory-heading">Evidence Directory</h2></div><a class="quiet-link" href="about.html#classifications">What the labels mean <span aria-hidden="true">↗</span></a></div><p class="section-intro">Browse individual studies and projects, or switch to Questions &amp; Findings for a plain-language synthesis. The collection includes {len(findings(studies))} records with findings and {len(studies)-len(findings(studies))} project records awaiting results review.</p>
{filter_html()}{toolbar('matching studies &amp; projects')}<div class="filter-context" id="filterContext" hidden></div><div class="items" id="study-results">{''.join(study_card(s) for s in studies)}</div><div class="items" id="question-results" hidden>{''.join(question_card(q) for q in questions)}</div><p class="filter-help">Selected topics match any selected topic; other filters are combined. <a href="coverage.html">Questions without indexed evidence</a> identify where this collection needs further searching.</p><noscript><p class="note">Search and filters require JavaScript. Browse the priority pages above, <a href="coverage.html">the complete coverage map</a>, or <a href="data/studies.csv">download the study records</a>.</p></noscript></section>'''
write('index.html',shell('Evidence Directory',home,current='evidence',script=True,hero=hero))

people_body=f'''<div class="page-heading"><p class="section-eyebrow">Connect research with practice</p><h1>Researcher Directory</h1><p>Find people connected to the work in this collection. Browse expertise, institutions, priorities, and published or preliminary findings.</p></div><p class="note">Profiles are assembled from public research and institutional sources. Affiliations are source-reported; inclusion does not imply NCLRN membership, endorsement, or availability for new work. Public profiles and contact links help you confirm current details.</p><section data-directory="researchers" aria-label="Find a researcher">{filter_html('researchers')}{toolbar('matching researchers')}<div class="items people-grid" id="people-results">{''.join(person_card(p) for p in sorted(people,key=lambda p:p['name']))}</div><p class="filter-help">Expertise tags reflect the work currently indexed here. This is a growing directory, not a complete inventory of NC researchers.</p></section>'''
write('researchers.html',shell('Researcher Directory',people_body,current='researchers',script=True))

for q in questions:
    syn=q['synthesis']; records=related(q['id'])
    main=f'''{breadcrumbs([(f'Priority {q["priority"]}',f'../priorities/{q["priority"]}.html'),(q['title'],None)])}<div class="page-heading"><div class="card-kicker">{priority_badges([q['priority']])}{maturity_badge(q['maturity'])}<span>{q['id']} · {e(q['kind'])}</span></div><h1>{e(q['title'])}</h1><p class="agenda-question">{e(q['description'])}</p></div>'''
    if syn:
        rationale='Several distinct analyses from more than one team document attendance patterns, consequences, and selected barriers. This breadth does not settle all causes or establish a universal intervention.' if syn['maturity']=='Substantial' else ('Only a project description has been reviewed; no outcome evidence is counted.' if syn['maturity']=='Project only' else 'The indexed evidence addresses part of this question; study coverage, replication, or accessible methods remain limited. This is an editorial classification of this collection.')
        main+=f'''<div class="detail-layout"><div><section class="synthesis-box"><div class="section-eyebrow">Draft evidence synthesis</div><h2>{e(syn['headline'])}</h2><p>{e(syn['summary'])}</p><p class="source-ref">Based on {', '.join(f'<a href="../{s["url"]}">{s["id"]}</a>' for s in findings(records)) or 'the project description listed below; no outcome findings reviewed'}.</p></section>{detail_section('Considerations for practice',syn['practice'])}{detail_section('What we still need to learn',syn['gap'])}</div><aside class="evidence-aside"><h2>Reading this evidence</h2><dl><dt>Coverage</dt><dd>{maturity_badge(q['maturity'])}</dd><dt>How findings compare</dt><dd>{e(q['consistency'])}</dd><dt>Records reviewed</dt><dd>{q['evidenceCount']} with findings · {q['projectCount']} project-only</dd></dl><p>{e(rationale)}</p><a href="../about.html#classifications">Read the classification criteria →</a><p class="small">Draft editorial synthesis · {display_date(syn["checked"])}</p></aside></div>'''
    else:
        main+='<section class="empty gap-panel"><h2>Evidence review is still to come</h2><p>No study has been indexed for this agenda entry yet. This does not mean that the question is unanswered or that no relevant NC research exists.</p><a class="btn" href="../about.html#contribute">Help expand the collection →</a></section>'
    if records:
        main+='<section class="related-section"><h2>Studies and projects connected to this question</h2>'+cards_grid(study_card(s,'../') for s in records)+'</section>'
        researchers=unique(r for s in records for r in s['researchers'])
        if researchers:
            main+='<section class="related-section"><h2>Connect with researchers</h2>'+cards_grid(person_card(people_map[r],'../') for r in researchers)+'</section>'
    main+=f'''<p class="source-note">Agenda mapping: <a href="../../ncdpi-research-agenda/?priority={q['priority']}#{q['id']}">{q['id']} in NCDPI’s Research &amp; Policy Priorities ↗</a>. The mapping indicates relevance; it does not mean this agenda question has been fully answered.</p>'''
    write(q['url'],shell(q['title'],main,prefix='../',description=q['description']))

for s in studies:
    main=f'''{breadcrumbs([('Studies & projects','../index.html?view=studies#directory'),(s['id'],None)])}<div class="page-heading"><div class="card-kicker">{badge(s['status'],'preliminary' if s['status']=='Preliminary findings' else 'neutral')}{priority_badges(s['priorities'])}<span>{s['id']} · {e(date_label(s))}</span></div><h1>{e(s['title'])}</h1><p class="authors">{e(s['authors'])}</p><div class="tagline">{topic_links(s['topics'],'../')}</div></div>
<div class="detail-layout"><div>{detail_section('What the study found' if s['hasFindings'] else 'What the project source reports',s['findings'],'finding-box')}{detail_section('Considerations for practice' if s['hasFindings'] else 'A useful next step',s['implications'])}{detail_section('Limitations and interpretation',s['limitations'])}{detail_section('Questions that remain',s['gap'])}</div><aside class="evidence-aside"><h2>Study at a glance</h2><dl><dt>Design</dt><dd>{e(s['design'])}</dd><dt>NC setting &amp; population</dt><dd>{e(s['setting'])}</dd><dt>Source reviewed</dt><dd>{e(s['sources'][0]['basis'])}</dd><dt>Source check</dt><dd>{display_date(s["checked"])}</dd></dl><a class="btn primary source-button" href="{e(s['sources'][0]['url'])}">Open research source ↗</a></aside></div>'''
    if s.get('dateNote'):
        main+=f'<p class="note"><strong>Date and status:</strong> {e(s["dateNote"])} <a href="{e(s["cohortSource"])}">2022 portfolio announcement ↗</a></p>'
    if s.get('versionNote'):
        main+=f'<p class="note"><strong>Version reviewed:</strong> {e(s["versionNote"])}</p>'
    if s.get('programs'):
        main+='<section class="related-section"><h2>Program and initiative connections</h2><p class="small">Direct evidence examines the named program or model. Related evidence informs a component or application; it is not an evaluation of the whole program.</p><ul class="program-connections">'+''.join(f'<li><a href="../programs/{link["id"]}.html">{e(program_map[link["id"]]["name"])}</a> {badge("Direct evidence" if link["relationship"] == "direct" else "Related evidence")}<p>{e(link["note"])}</p></li>' for link in s['programs'])+'</ul></section>'
    main+='<section class="related-section"><h2>Agenda questions this work informs</h2>'+cards_grid(question_card(question_map[q],'../') for q in s['questions'])+'</section>'
    if s['researchers']:
        main+='<section class="related-section"><h2>Researchers connected to this work</h2>'+cards_grid(person_card(people_map[r],'../') for r in s['researchers'])+'</section>'
    main+='<section class="related-section"><h2>Sources &amp; provenance</h2>'+source_list(s)+'<p class="small">Plain-language findings, practice considerations, limitations, and mappings are draft editorial interpretations. Consult the original work for complete methods and results.</p></section>'
    write(s['url'],shell(s['title'],main,prefix='../',description=s['findings']))

for p in people:
    main=f'''{breadcrumbs([('Researchers','../researchers.html'),(p['name'],None)])}<div class="page-heading"><p class="section-eyebrow">Researcher connection</p><h1>{e(p['name'])}</h1><p class="institution">{e(p['institution'])}</p><p>{e(p['focus'])}</p><div class="hero-actions"><a class="btn primary" href="{e(p['url'])}">{e(p['linkLabel'])} ↗</a>{f'<a class="btn" href="mailto:{e(p["email"])}">Email researcher</a>' if p.get('email') else ''}</div></div><p class="note">{e(p['affiliationNote'])} Inclusion reflects a connection to indexed work, not an offer of services or a confirmed NCLRN membership. Source checked {display_date(p["checked"])}.</p><section><h2>Work in this collection</h2>{cards_grid(study_card(study_map[s],'../') for s in p['studyIds'])}</section>'''
    write(p['pageUrl'],shell(p['name'],main,prefix='../',current='researchers',description=p['focus']))

for i,p in enumerate(priorities,1):
    qs=[q for q in questions if q['priority']==i]
    rs=[s for s in studies if i in s['priorities']]
    central=next(q for q in agenda['items'] if q['kind']=='Primary Question' and q['priority']==i)
    ts=unique(t for s in rs for t in s['topics'])
    main=f'''{breadcrumbs([(f'Priority {i}',None)])}<div class="page-heading priority-heading" data-priority="{i}"><p class="priority-number">Priority {i}</p><h1>{e(p)}</h1><p>{priority_intros[i-1]}</p></div><section class="agenda-frame"><h2>The agenda’s central question</h2><p>{e(central['description'])}</p></section><p class="collection-summary"><strong>{len(findings(rs))}</strong> records with findings · <strong>{len(rs)-len(findings(rs))}</strong> project records · <strong>{len([q for q in qs if q['studyIds']])} of {len(qs)}</strong> agenda entries with indexed work</p><div class="tagline topic-links">{topic_links(ts,'../')}</div><section><h2>What the indexed evidence can tell us</h2><p class="section-intro">Each summary addresses part of an existing agenda question. Read its scope, supporting studies, and remaining gaps before applying findings.</p>{cards_grid(question_card(q,'../') for q in qs if q['studyIds'])}</section><section class="related-section"><h2>Where this collection needs further review</h2><ul class="gap-list">{''.join(f'<li><a href="../{q["url"]}">{e(q["title"])}</a><span>{q["id"]} · Not yet indexed</span></li>' for q in qs if not q['studyIds']) or '<li>All standing agenda entries in this priority have at least one linked record; coverage may still be partial.</li>'}</ul><p class="small">These are gaps in directory coverage, not verified gaps in the research literature.</p></section>'''
    write(f'priorities/{i}.html',shell(p,main,prefix='../'))

for t in topics:
    rs=[s for s in studies if t in s['topics']]
    qids=unique(q for s in rs for q in s['questions'])
    qs=[q for q in questions if q['id'] in qids]
    main=f'''{breadcrumbs([('Topics', '../index.html#directory'),(t,None)])}<div class="page-heading"><p class="section-eyebrow">Evidence by topic</p><h1>{e(t)}</h1><p>Explore question-level syntheses and studies tagged with this topic. Question summaries may draw on additional, related topics.</p></div><p class="collection-summary"><strong>{len(findings(rs))}</strong> records with findings · <strong>{len(rs)-len(findings(rs))}</strong> project records</p>'''
    if rs:
        main+='<section><h2>What we know—and what remains open</h2>'+cards_grid(question_card(q,'../') for q in qs)+'</section><section class="related-section"><h2>Studies and projects on this topic</h2>'+cards_grid(study_card(s,'../') for s in rs)+'</section>'
    else:
        main+='<section class="empty"><h2>Topic review is still to come</h2><p>No eligible NC study has been indexed under this topic in the initial collection. Relevant research may exist and remains a search priority.</p><a class="btn" href="../about.html#contribute">Help expand the collection →</a></section>'
    if t=='AI & technology':
        main+='<p class="note">This shared agenda topic includes digital technology. The current records do not establish the effects of AI-supported instruction.</p>'
    write(f'topics/{slug(t)}.html',shell(t,main,prefix='../'))

for program in programs:
    pid = program['id']
    direct = [s for s in studies if any(p['id'] == pid and p['relationship'] == 'direct' for p in s.get('programs', []))]
    indirect = [s for s in studies if any(p['id'] == pid and p['relationship'] == 'related' for p in s.get('programs', []))]
    main = f'''{breadcrumbs([('Programs & initiatives','../index.html#directory'),(program['name'],None)])}<div class="page-heading"><p class="section-eyebrow">Evidence by program or initiative</p><h1>{e(program['name'])}</h1><p>{e(program['authorityNote'])}</p></div><p><strong>Basis:</strong> {e(program['authorityType'])}. <a href="{e(program['authorityUrl'])}">Program authority and background ↗</a></p><p class="small">Background source checked {display_date(program['checked'])}. Program connections are draft editorial mappings, not evidence-strength ratings.</p>'''
    if program.get('parentId'):
        parent = program_map[program['parentId']]
        main += f'<p>Connected program: <a href="{parent["id"]}.html">{e(parent["name"])}</a>. Findings about one component do not establish effects of the whole program.</p>'
    main += f'<p><a class="btn" href="../index.html?program={pid}#directory">Browse matching records</a></p><section class="related-section"><h2>Direct evidence</h2><p>These records examine the program, model, its implementation, or its participants. Direct does not mean causal or positive.</p>'
    main += cards_grid(study_card(s,'../') for s in direct) if direct else '<p>No direct study is indexed yet. This is a collection gap, not evidence that no research exists.</p>'
    main += '</section><section class="related-section"><h2>Related evidence</h2><p>These records inform a component, historical model, or application. Read the connection note on each study before applying its findings.</p>'
    main += cards_grid(study_card(s,'../') for s in indirect) if indirect else '<p>No additional related records are mapped yet.</p>'
    main += '</section>'
    write(f'programs/{pid}.html',shell(program['name'],main,prefix='../'))

coverage='''<div class="page-heading"><p class="section-eyebrow">A transparent view of the collection</p><h1>Collection Coverage</h1><p>See which standing agenda questions and areas of interest have linked work. An empty entry means further searching is needed here; it does not mean that no NC research exists.</p></div><p class="note">The map includes the agenda’s 36 supporting questions and 26 areas of interest. Its six central questions frame the priority pages. Illustrative legislative initiatives, cross-cutting lenses, and data enablers are not counted as separate evidence questions.</p>'''
for i,p in enumerate(priorities,1):
    qs=[q for q in questions if q['priority']==i]
    coverage+=f'<section class="coverage-section"><h2>Priority {i}: {e(p)}</h2><div class="table-wrap"><table><caption class="sr-only">Coverage of priority {i}</caption><thead><tr><th scope="col">Agenda entry</th><th scope="col">Evidence coverage</th><th scope="col">Findings / projects</th></tr></thead><tbody>'+''.join(f'<tr><th scope="row"><a href="{q["url"]}">{e(q["title"])}</a><small>{q["id"]} · {e(q["kind"])}</small></th><td>{maturity_badge(q["maturity"])}</td><td>{q["evidenceCount"]} / {q["projectCount"]}</td></tr>' for q in qs)+'</tbody></table></div></section>'
coverage+='<p><a class="btn" href="data/agenda-evidence-crosswalk.csv" download>Download the agenda-to-evidence crosswalk</a></p>'
write('coverage.html',shell('Collection Coverage',coverage))

about='''<div class="page-heading"><p class="section-eyebrow">Context before conclusions</p><h1>About the Evidence</h1><p>A growing guide to NC education findings and the people behind them, designed to help practitioners and policymakers ask informed questions and connect with research partners.</p></div>
<div class="about-layout"><nav class="contents" aria-label="On this page"><strong>On this page</strong><a href="#scope">Scope & search</a><a href="#classifications">Evidence labels</a><a href="#programs">Programs &amp; initiatives</a><a href="#interpretation">Using findings</a><a href="#sources">Sources & versions</a><a href="#contribute">NCLRN stewardship</a></nav><div class="prose">
<section id="scope"><h2>Scope and initial search</h2><p>The initial search covers work released from October 26, 2016 through October 26, 2026 that uses NC data or directly examines NC education. It includes connected early learning and postsecondary work, journal articles, working papers, evaluations, agency reporting, technical policy studies, and preliminary project findings.</p><p>This is a structured starting collection, not a systematic review or a complete inventory. Searches began with NCLRN and the NC Collaboratory, then extended to author and institutional publication pages, SERVE, the Friday Institute, Duke, IES, CALDER, EdWorkingPapers, ERIC, journal publishers, and public agency sources. Search terms combined North Carolina with the existing agenda topics and program names.</p><p>The site retains the agenda’s six priorities and 17 topics. Study-to-question mappings are editorial judgments about relevance. Strategic-plan expansion is reserved for later curation rather than adding unapproved priority categories now.</p><p>For undated project pages, the directory records the known project cohort and leaves the publication year unknown. A past project end date does not prove that final results have been released. Project-only records do not count as findings.</p></section>
<section id="classifications"><h2>What the labels mean</h2><p>Coverage and consistency are separate judgments about the records currently indexed for a question. They are draft editorial classifications, not formal study-quality scores, ESSA tiers, or an agency determination that a question is answered.</p><h3>Evidence coverage</h3><dl class="definition-list"><dt>Substantial</dt><dd>Several distinct NC analyses from more than one research team, including published empirical findings, address meaningful parts of the question. This describes breadth in the collection; causal certainty and completeness may remain limited.</dd><dt>Emerging</dt><dd>Relevant findings are available, but the scope, replication, methods available for review, or maturity of the work limits a broader conclusion. A published study can still contribute to an emerging evidence base.</dd><dt>Project only</dt><dd>A relevant project has been located, but no outcome findings have been reviewed. Its design and contact information are useful; it supplies no effectiveness conclusion.</dd><dt>Not yet indexed</dt><dd>No study is mapped here yet. Further searching and review are needed; absence from this directory is not evidence that research does not exist.</dd></dl><h3>How findings compare</h3><dl class="definition-list"><dt>Aligned within indexed evidence</dt><dd>Reviewed findings point in a broadly similar direction for the outcomes discussed. This does not establish unanimity in the literature or a common effect size.</dd><dt>Mixed / context-dependent</dt><dd>Patterns differ across outcomes, populations, program models, or settings. This can reflect meaningful variation rather than contradictory studies.</dd><dt>Limited comparison</dt><dd>Too few directly comparable findings have been reviewed to assess agreement.</dd><dt>Not assessed</dt><dd>The question spans different constructs or the available records do not support a useful consistency judgment.</dd></dl><p>A record’s publication or project status is separate from these labels. “Working paper” names the version reviewed; it does not certify that no later journal version exists. “Preliminary findings” means that a source explicitly reports early results, often without a complete accessible methods report.</p></section>
<section id="programs"><h2>Programs and initiatives</h2><p>The program filter includes named NC programs, school models, statewide initiatives, and federal programs implemented in NC when relevant work is indexed. It shows only programs with at least one mapped record. Legislative or State Board authority is recorded separately; it is not a prerequisite for inclusion.</p><p><strong>Direct evidence</strong> examines the named program or model, its implementation, or its participants. <strong>Related evidence</strong> informs a component or application without evaluating the whole program. Neither label describes study quality, causal certainty, or whether findings are favorable. Connection notes explain each mapping.</p><p>Choose a program to reveal the evidence-connection filter. In Questions &amp; Findings and Researchers views, a match requires at least one linked study with that program connection. Question summaries and researcher profiles retain their full scope; they may discuss additional programs or work.</p><p>Components and parent programs can have separate names, such as LETRS and Read to Achieve. A component study is not automatically counted as a direct evaluation of its parent. Broad topic overlap alone is not enough to map a program.</p></section>
<section id="interpretation"><h2>Using findings in a decision</h2><ol><li><strong>Match the decision to the question.</strong> A related study rarely answers every part of a broad priority.</li><li><strong>Check the people, place, and period.</strong> A recent publication may analyze older data or one district.</li><li><strong>Read the design and limitations.</strong> Descriptions, associations, and effect estimates answer different questions. A null estimate is not proof that an intervention can never work.</li><li><strong>Separate findings from application.</strong> Practice considerations and proposed next questions are editorial interpretations, not tested prescriptions.</li><li><strong>Connect with the researchers.</strong> Ask about current evidence, local fit, implementation, costs, and feasibility before adopting or scaling an approach.</li></ol><p>Preliminary numbers are identified as preliminary. Multi-state results are not presented as NC-only findings. Different programs, outcomes, and designs are not ranked as if they were interchangeable.</p></section>
<section id="sources"><h2>Sources, provenance, and versions</h2><p>Each record identifies the source reviewed, the review basis (such as full text, abstract, project summary, or agency summary), the section supporting the findings, the source-check date, and relevant version notes. Full reports were not available or fully reviewed for every entry; that limitation is visible on the record.</p><p>Related working papers, briefs, and journal versions are treated as one study family. Annual program evaluations are not automatically counted as independent replications. The current source data include stable study, researcher, and agenda IDs so the collection can grow without breaking existing links.</p><p>The original agenda remains the authority for priority names and question wording. Researcher profiles use public professional information and source-reported affiliations. Inclusion does not imply NCLRN membership, endorsement, willingness to collaborate, or current availability.</p><ul><li><a href="https://nclrn.unc.edu/">North Carolina Learning Research Network ↗</a></li><li><a href="https://collaboratory.unc.edu/highlighted-projects/office-of-learning-research-olr/">NC Collaboratory K–12 research portfolio ↗</a></li><li><a href="../ncdpi-research-agenda/">NCDPI Research &amp; Policy Priorities ↗</a></li></ul></section>
<section id="contribute"><h2>Stewardship by NCLRN</h2><p>This draft is prepared for NCLRN to curate and maintain. Suggested additions should include a citation or public source, the NC setting or data used, relevant agenda questions, a findings summary, limitations, and the most current version available.</p><p>Before a record moves beyond draft status, an NCLRN reviewer should verify its citation, NC relevance, methods, summary, and mappings, and confirm public researcher contact information. Revise question syntheses when new findings change the balance of evidence, recording the date and reviewer. Recheck undated preliminary summaries for final reports.</p><p><a class="btn primary" href="https://nclrn.unc.edu/contact-us/">Find NCLRN contact information ↗</a></p><p>No submission is sent through this site. Use NCLRN’s public contact route to discuss a study, correction, or connection.</p><h3>Downloads for review and reuse</h3><ul><li><a href="data/studies.csv" download>Study records (CSV)</a></li><li><a href="data/researchers.csv" download>Researcher profiles (CSV)</a></li><li><a href="data/agenda-evidence-crosswalk.csv" download>Agenda-to-evidence crosswalk (CSV)</a></li><li><a href="data/catalog.json" download>Editable source catalog (JSON)</a></li><li><a href="maintenance/README.txt">Maintainer instructions</a></li><li><a href="maintenance/research-log.json">Initial search log and limitations</a></li></ul></section></div></div>'''
write('about.html',shell('About the Evidence',about,current='about'))

payload=dict(meta=catalog['meta'],priorities=priorities,topics=topics,programs=programs,questions=questions,studies=studies,researchers=people)
write('data/evidence-data.js','window.EVIDENCE = '+json.dumps(payload,ensure_ascii=False,separators=(',',':'))+';\n')

def write_csv(name, fields, rows):
    buf=io.StringIO(newline='')
    writer=csv.writer(buf);writer.writerow(fields)
    for row in rows:
        writer.writerow([('; '.join(map(str,v)) if isinstance(v,list) else v) for v in row])
    (ROOT/'data'/name).write_text('\ufeff'+buf.getvalue(),encoding='utf-8')

write_csv('studies.csv',['Study ID','Title','Year','Status','Authors','Researcher IDs','Agenda IDs','Priorities','Topics','Design','NC setting','Findings','Practice considerations','Limitations','Remaining question','Source URL','Review basis','Source locator','Source checked','Version note','Date note','Editorial status','Program connections'],
 [[s['id'],s['title'],s['year'],s['status'],s['authors'],s['researchers'],s['questions'],s['priorities'],s['topics'],s['design'],s['setting'],s['findings'],s['implications'],s['limitations'],s['gap'],s['sources'][0]['url'],s['sources'][0]['basis'],s['sources'][0]['locator'],s['checked'],s.get('versionNote',''),s.get('dateNote',''),s['reviewStatus'],program_export(s)] for s in studies])
write_csv('researchers.csv',['Researcher ID','Name','Source-reported institution','Focus','Public source','Public email','Related study IDs','Topics','Source checked','Affiliation note'],[[p['id'],p['name'],p['institution'],p['focus'],p['url'],p.get('email',''),p['studyIds'],p['topics'],p['checked'],p['affiliationNote']] for p in people])
write_csv('agenda-evidence-crosswalk.csv',['Agenda ID','Priority','Priority name','Entry type','Title','Exact agenda question','Coverage','Consistency','Evidence records','Project records','Study IDs','Draft synthesis'],[[q['id'],q['priority'],q['priority_name'],q['kind'],q['title'],q['description'],q['maturity'],q['consistency'],q['evidenceCount'],q['projectCount'],q['studyIds'],q['synthesis']['summary'] if q['synthesis'] else ''] for q in questions])

summary=dict(studies=len(studies),withFindings=len(findings(studies)),projects=len(studies)-len(findings(studies)),researchers=len(people),agendaEntries=len(questions),entriesWithRecords=sum(bool(q['studyIds']) for q in questions),topics=len(topics),programs=len(programs),priorities=len(priorities),generatedPages=len(list(ROOT.rglob('*.html'))))
write('data/build-summary.json',json.dumps(summary,indent=2)+'\n')
print(json.dumps(summary,indent=2))
