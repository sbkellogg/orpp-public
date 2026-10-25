'use strict';
const $=id=>document.getElementById(id);
const esc=s=>String(s??'').replace(/[&<>"']/g,c=>({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[c]));
const directoryItems=DATA.items.filter(a=>a.kind!=='Primary Question');
function buildOffices(){let counts={};directoryItems.forEach(a=>a.offices.forEach(o=>counts[o]=(counts[o]||0)+1));$('officeGrid').innerHTML=Object.entries(DATA.offices).sort((a,b)=>a[1].name.localeCompare(b[1].name)).map(([code,o])=>`<article class="office-card"><h2>${esc(o.name)}</h2><div class="person">${esc(o.leader)}</div><div class="role">${esc(o.role)}</div><a href="${esc(o.url)}" target="_blank" rel="noopener noreferrer">Office / leadership source ↗</a><div class="office-count">${counts[code]||0} potential research connections</div></article>`).join('')}
buildOffices();
