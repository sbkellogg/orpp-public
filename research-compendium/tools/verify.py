#!/usr/bin/env python3
"""Check generated pages, internal routes, fragments, and catalog invariants."""
import csv
import json
from collections import Counter
from html.parser import HTMLParser
from pathlib import Path
from urllib.parse import unquote, urlsplit

ROOT = Path(__file__).resolve().parents[1]

class Page(HTMLParser):
    def __init__(self, path):
        super().__init__(convert_charrefs=True)
        self.path=path; self.links=[]; self.ids=[]; self.h1=0; self.lang=None
    def handle_starttag(self, tag, attrs):
        a=dict(attrs)
        if a.get('id'): self.ids.append(a['id'])
        if tag=='h1': self.h1+=1
        if tag=='html': self.lang=a.get('lang')
        if tag in ('a','link','script','img'):
            value=a.get('href') or a.get('src')
            if value: self.links.append(value)
        if tag=='img': assert a.get('alt'), f'Missing image alt: {self.path}'

pages={}
for path in ROOT.rglob('*.html'):
    if path.name.startswith('_'): continue
    parsed=Page(path);parsed.feed(path.read_text());pages[path.resolve()]=parsed
    assert parsed.h1==1, f'Expected one h1 in {path}'
    assert parsed.lang=='en', f'Missing language in {path}'
    assert len(set(parsed.ids))==len(parsed.ids), f'Duplicate element IDs in {path}'

errors=[]; checked=0
for path,page in pages.items():
    for link in page.links:
        parts=urlsplit(link)
        if parts.scheme or parts.netloc: continue
        target=(path.parent/unquote(parts.path)).resolve() if parts.path else path
        if target.is_dir(): target=target/'index.html'
        checked+=1
        if not target.exists():
            errors.append(f'{path.relative_to(ROOT)} -> {link}')
        elif parts.fragment and target in pages and unquote(parts.fragment) not in pages[target].ids:
            errors.append(f'{path.relative_to(ROOT)} -> missing fragment {link}')
assert not errors, '\n'.join(errors)

catalog=json.loads((ROOT/'data/catalog.json').read_text())
summary=json.loads((ROOT/'data/build-summary.json').read_text())
for name, count in [('studies.csv',len(catalog['studies'])),('researchers.csv',len(catalog['researchers'])),('agenda-evidence-crosswalk.csv',62)]:
    with (ROOT/'data'/name).open(encoding='utf-8-sig',newline='') as f:
        rows=list(csv.DictReader(f))
    assert len(rows)==count, f'Incorrect CSV row count: {name}'
assert summary['withFindings']==sum(s['hasFindings'] for s in catalog['studies'])
assert summary['projects']==sum(not s['hasFindings'] for s in catalog['studies'])
assert all((ROOT/f'studies/{s["id"]}.html').exists() for s in catalog['studies'])
print(json.dumps({'htmlPagesChecked':len(pages),'internalReferencesChecked':checked,'missingLinks':0,'csvRowsVerified':True,'statuses':dict(Counter(s['status'] for s in catalog['studies']))},indent=2))
