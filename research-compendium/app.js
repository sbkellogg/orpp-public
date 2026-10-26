'use strict';

(() => {
  const root = document.querySelector('[data-directory]');
  if (!root || !window.EVIDENCE) return;
  const data = window.EVIDENCE;
  const byId = id => document.getElementById(id);
  const isPeople = root.dataset.directory === 'researchers';
  const studies = new Map(data.studies.map(s => [s.id, s]));
  const researchers = new Map(data.researchers.map(p => [p.id, p]));
  const questions = new Map(data.questions.map(q => [q.id, q]));
  const normalize = value => String(value || '').normalize('NFKD').replace(/[\u0300-\u036f]/g, '').toLocaleLowerCase();
  const studyText = s => [s.id, s.title, s.authors, s.findings, s.implications, s.limitations, s.design, s.setting, ...s.topics, ...s.researchers.map(id => researchers.get(id)?.name || '')].join(' ');
  const searchIndexes = {
    questions: new Map(data.questions.map(q => [q.id, normalize([q.id, q.title, q.description, q.synthesis?.summary, ...q.topics, ...q.studyIds.map(id => studyText(studies.get(id)))].join(' '))])),
    studies: new Map(data.studies.map(s => [s.id, normalize(studyText(s))])),
    researchers: new Map(data.researchers.map(p => [p.id, normalize([p.name, p.institution, p.focus, ...p.topics, ...p.studyIds.map(id => studyText(studies.get(id)))].join(' '))]))
  };
  let selectedTopics = new Set();
  let visible = [];

  function view() {
    return isPeople ? 'researchers' : document.querySelector('input[name="view"]:checked').value;
  }

  function setStatusOptions(requested = '') {
    if (isPeople) return;
    const questionView = view() === 'questions';
    const values = questionView ? ['Substantial', 'Emerging', 'Project only', 'Not yet indexed'] : [...new Set(data.studies.map(s => s.status))].sort();
    byId('status').replaceChildren(new Option(questionView ? 'All coverage' : 'All statuses', ''), ...values.map(v => new Option(v, v)));
    byId('status').value = values.includes(requested) ? requested : '';
    byId('statusLabel').textContent = questionView ? 'Evidence coverage' : 'Publication / project status';
    byId('consistencyField').hidden = !questionView;
    byId('consistency').disabled = !questionView;
    byId('gapsLabel').hidden = !questionView;
    byId('gaps').disabled = !questionView;
  }

  function readUrl() {
    const p = new URLSearchParams(location.search);
    byId('search').value = p.get('q') || '';
    const priority = p.get('priority') || '';
    byId('priority').value = /^[1-6]$/.test(priority) ? priority : '';
    selectedTopics = new Set((p.get('topics') || '').split('|').filter(t => data.topics.includes(t)));
    if (isPeople) {
      const institution = p.get('institution') || '';
      byId('institution').value = [...byId('institution').options].some(o => o.value === institution) ? institution : '';
    } else {
      const selected = p.get('view') === 'questions' ? 'questions' : 'studies';
      document.querySelector(`input[name="view"][value="${selected}"]`).checked = true;
      setStatusOptions(p.get('status'));
      byId('consistency').value = p.get('consistency') || '';
      byId('gaps').checked = p.get('gaps') === '1' || byId('status').value === 'Not yet indexed';
      byId('sort').value = ['priority', 'title', 'newest'].includes(p.get('sort')) ? p.get('sort') : 'priority';
    }
  }

  function writeUrl() {
    const p = new URLSearchParams();
    const current = view();
    if (!isPeople && current === 'questions') p.set('view', 'questions');
    if (byId('search').value.trim()) p.set('q', byId('search').value.trim());
    if (byId('priority').value) p.set('priority', byId('priority').value);
    if (selectedTopics.size) p.set('topics', [...selectedTopics].sort().join('|'));
    if (isPeople) {
      if (byId('institution').value) p.set('institution', byId('institution').value);
    } else {
      if (byId('status').value) p.set('status', byId('status').value);
      if (current === 'questions' && byId('consistency').value) p.set('consistency', byId('consistency').value);
      if (current === 'questions' && byId('gaps').checked) p.set('gaps', '1');
      if (byId('sort').value !== 'priority') p.set('sort', byId('sort').value);
    }
    if (['http:', 'https:'].includes(location.protocol)) history.replaceState(null, '', location.pathname + (p.size ? '?' + p.toString() : '') + location.hash);
  }

  function recordTopics(record, current) {
    return current === 'questions' ? [...new Set([...record.topics, ...record.studyIds.flatMap(id => studies.get(id).topics)])] : record.topics;
  }

  function render(updateUrl = true) {
    const current = view();
    const terms = normalize(byId('search').value.trim()).split(/\s+/).filter(Boolean);
    const priority = Number(byId('priority').value) || null;
    const values = current === 'questions' ? data.questions : current === 'studies' ? data.studies : data.researchers;
    const status = isPeople ? '' : byId('status').value;
    const consistency = !isPeople && current === 'questions' ? byId('consistency').value : '';
    visible = values.filter(record => {
      if (!terms.every(t => searchIndexes[current].get(record.id).includes(t))) return false;
      const priorities = current === 'questions' ? [record.priority] : record.priorities;
      if (priority && !priorities.includes(priority)) return false;
      if (selectedTopics.size && !recordTopics(record, current).some(t => selectedTopics.has(t))) return false;
      if (isPeople) return !byId('institution').value || record.institution === byId('institution').value;
      if (current === 'questions') {
        if (!byId('gaps').checked && !record.studyIds.length) return false;
        if (status && record.maturity !== status) return false;
        if (consistency && record.consistency !== consistency) return false;
      } else if (status && record.status !== status) return false;
      return true;
    });
    const name = r => r.name || r.title;
    const year = r => r.year || (r.studyIds ? Math.max(0, ...r.studyIds.map(id => studies.get(id).year || 0)) : 0);
    visible.sort((a, b) => {
      if (isPeople || byId('sort').value === 'title') return name(a).localeCompare(name(b));
      if (byId('sort').value === 'newest') return year(b) - year(a) || name(a).localeCompare(name(b));
      const pa = a.priority || a.priorities[0], pb = b.priority || b.priorities[0];
      return pa - pb || name(a).localeCompare(name(b));
    });
    const visibleIds = new Set(visible.map(r => r.id));
    const container = byId(current === 'questions' ? 'question-results' : current === 'studies' ? 'study-results' : 'people-results');
    [...container.children].forEach(card => { card.hidden = !visibleIds.has(card.dataset.record); });
    visible.forEach(record => container.appendChild(byId(`${current === 'questions' ? 'question' : current === 'studies' ? 'study' : 'person'}-${record.id}`)));
    if (!isPeople) {
      byId('question-results').hidden = current !== 'questions';
      byId('study-results').hidden = current !== 'studies';
    }
    byId('empty').hidden = visible.length > 0;
    byId('resultCount').textContent = visible.length;
    byId('resultNoun').textContent = current === 'questions' ? 'matching questions / areas of interest' : current === 'studies' ? 'matching studies & projects' : 'matching researchers';
    byId('topicCount').textContent = selectedTopics.size ? `(${selectedTopics.size} selected)` : '';
    document.querySelectorAll('[data-topic]').forEach(button => {
      const active = selectedTopics.has(button.dataset.topic);
      button.classList.toggle('active', active);
      button.setAttribute('aria-pressed', String(active));
    });
    const active = [];
    if (priority) active.push(`Priority ${priority}`);
    if (selectedTopics.size) active.push([...selectedTopics].join(' or '));
    if (status) active.push(status);
    if (consistency) active.push(consistency);
    if (isPeople && byId('institution').value) active.push(byId('institution').value);
    if (terms.length) active.push(`Search: ${byId('search').value.trim()}`);
    byId('activeSummary').textContent = active.length ? active.join(' · ') : '';
    if (!isPeople) {
      const context = byId('filterContext');
      context.hidden = !priority;
      context.textContent = priority ? `Priority ${priority}: ${data.priorities[priority - 1]}. Mappings identify relevance; broad questions may be only partly addressed.` : '';
    }
    if (updateUrl) writeUrl();
  }

  function clear() {
    byId('search').value = '';
    byId('priority').value = '';
    selectedTopics.clear();
    if (isPeople) byId('institution').value = '';
    else {
      byId('status').value = '';
      byId('consistency').value = '';
      byId('gaps').checked = false;
      byId('sort').value = 'priority';
    }
    render();
  }

  byId('search').addEventListener('input', () => render());
  byId('priority').addEventListener('change', () => render());
  document.querySelectorAll('[data-topic]').forEach(button => button.addEventListener('click', () => {
    selectedTopics.has(button.dataset.topic) ? selectedTopics.delete(button.dataset.topic) : selectedTopics.add(button.dataset.topic);
    render();
  }));
  byId('clear').addEventListener('click', clear);
  byId('emptyClear').addEventListener('click', clear);
  if (isPeople) byId('institution').addEventListener('change', () => render());
  else {
    byId('status').addEventListener('change', () => {
      if (view() === 'questions' && byId('status').value === 'Not yet indexed') byId('gaps').checked = true;
      render();
    });
    ['consistency', 'gaps', 'sort'].forEach(id => byId(id).addEventListener('change', () => render()));
    document.querySelectorAll('input[name="view"]').forEach(radio => radio.addEventListener('change', () => {
      setStatusOptions(byId('status').value);
      render();
    }));
  }

  const csvCell = value => {
    let text = Array.isArray(value) ? value.join('; ') : String(value ?? '');
    if (/^[=+\-@\t\r]/.test(text)) text = "'" + text;
    return '"' + text.replaceAll('"', '""') + '"';
  };
  byId('export').addEventListener('click', () => {
    const current = view();
    let headers, rows;
    if (current === 'questions') {
      headers = ['Agenda ID', 'Priority', 'Title', 'Exact agenda question', 'Evidence coverage', 'Consistency', 'Evidence record count', 'Project record count', 'Study IDs', 'Draft synthesis', 'Remaining question', 'Record URL'];
      rows = visible.map(q => [q.id, q.priority_name, q.title, q.description, q.maturity, q.consistency, q.evidenceCount, q.projectCount, q.studyIds, q.synthesis?.summary, q.synthesis?.gap, new URL(q.url, location.href).href]);
    } else if (current === 'studies') {
      headers = ['Study ID', 'Title', 'Year', 'Status', 'Authors', 'Design', 'NC setting', 'Findings', 'Practice considerations', 'Limitations', 'Remaining question', 'Agenda IDs', 'Source URL', 'Review basis', 'Version note', 'Date note', 'Source checked'];
      rows = visible.map(s => [s.id, s.title, s.year, s.status, s.authors, s.design, s.setting, s.findings, s.implications, s.limitations, s.gap, s.questions, s.sources[0].url, s.sources[0].basis, s.versionNote, s.dateNote, s.checked]);
    } else {
      headers = ['Researcher ID', 'Name', 'Source-reported institution', 'Focus', 'Public source', 'Public email', 'Related study IDs', 'Affiliation note', 'Source checked'];
      rows = visible.map(p => [p.id, p.name, p.institution, p.focus, p.url, p.email, p.studyIds, p.affiliationNote, p.checked]);
    }
    const csv = '\uFEFF' + [headers, ...rows].map(row => row.map(csvCell).join(',')).join('\r\n');
    const url = URL.createObjectURL(new Blob([csv], {type: 'text/csv;charset=utf-8'}));
    const link = document.createElement('a');
    link.href = url; link.download = `NC_education_research_compendium_${current}_filtered.csv`;
    document.body.appendChild(link); link.click(); link.remove();
    setTimeout(() => URL.revokeObjectURL(url), 1000);
  });
  byId('copyLink').addEventListener('click', async () => {
    const button = byId('copyLink');
    try {
      await navigator.clipboard.writeText(location.href);
      button.textContent = 'Link copied';
      setTimeout(() => { button.textContent = 'Copy filtered link'; }, 1800);
    } catch {
      let input = byId('copyFallback');
      if (!input) {
        input = document.createElement('input');input.id = 'copyFallback';input.readOnly = true;
        input.setAttribute('aria-label', 'Filtered link to copy');input.className = 'copy-fallback';
        button.parentElement.appendChild(input);
      }
      input.value = location.href; input.focus(); input.select();
      button.textContent = 'Select and copy the link';
    }
  });
  window.addEventListener('popstate', () => { readUrl(); render(false); });
  readUrl();
  render();
})();
