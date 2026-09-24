// neuromosaic montage report behaviour: map-variant tabs and the contents sidebar.
document.addEventListener('DOMContentLoaded', function () {
  document.documentElement.classList.add('nm-js');

  // ---- Map-variant tabs / select ------------------------------------------
  document.querySelectorAll('[data-nm-map-group]').forEach(function (group) {
    var variants = Array.prototype.slice.call(group.querySelectorAll('[data-nm-map-variant]'));
    var tabs = Array.prototype.slice.call(group.querySelectorAll('[role=tab]'));
    var select = group.querySelector('[data-nm-map-select]');
    if (tabs.length === 0 && !select) return;
    function activate(id, focus) {
      var mapId = null;
      variants.forEach(function (v) {
        var on = v.id === id;
        v.hidden = !on;
        if (on) mapId = v.getAttribute('data-nm-map-id');
      });
      tabs.forEach(function (t) {
        var on = t.getAttribute('aria-controls') === id;
        t.setAttribute('aria-selected', on ? 'true' : 'false');
        t.tabIndex = on ? 0 : -1;
        if (on && focus) t.focus();
      });
      if (select) select.value = id;
      if (mapId) {
        group.dispatchEvent(new CustomEvent('nm-map-change', {
          detail: { analysisId: group.getAttribute('data-nm-map-group'), mapId: mapId },
          bubbles: true
        }));
      }
    }
    tabs.forEach(function (tab, index) {
      tab.addEventListener('click', function () { activate(tab.getAttribute('aria-controls'), false); });
      tab.addEventListener('keydown', function (e) {
        var next = index;
        if (e.key === 'ArrowRight' || e.key === 'ArrowDown') next = (index + 1) % tabs.length;
        else if (e.key === 'ArrowLeft' || e.key === 'ArrowUp') next = (index + tabs.length - 1) % tabs.length;
        else if (e.key === 'Home') next = 0;
        else if (e.key === 'End') next = tabs.length - 1;
        else return;
        e.preventDefault();
        activate(tabs[next].getAttribute('aria-controls'), true);
      });
    });
    if (select) select.addEventListener('change', function () { activate(select.value, false); });
    function requestMap(e) {
      var detail = e.detail || {};
      if (detail.analysisId !== group.getAttribute('data-nm-map-group')) return;
      var variant = variants.find(function (v) { return v.getAttribute('data-nm-map-id') === detail.mapId; });
      if (variant) activate(variant.id, false);
    }
    group.addEventListener('nm-map-request', requestMap);
    group.addEventListener('nm-volume-map-request', requestMap);
    var primary = group.querySelector('[data-nm-primary=true]') || variants[0];
    if (primary) activate(primary.id, false);
  });

  // ---- Contents sidebar -----------------------------------------------------
  var main = document.querySelector('.main-container') || document.body;
  var heads = Array.prototype.slice.call(main.querySelectorAll('h1, .nm-panel-title'))
    .filter(function (h) { return !h.classList.contains('title') && !h.closest('#header') && h.offsetParent !== null; });
  if (heads.length === 0) return;
  var slug = function (text, i) {
    return 'nm-' + (text || 'section').toLowerCase().replace(/[^a-z0-9]+/g, '-').replace(/^-|-$/g, '') + '-' + i;
  };
  var nav = document.createElement('nav');
  nav.className = 'nm-toc';
  nav.setAttribute('aria-label', 'Contents');
  var title = document.querySelector('.nm-report-title');
  if (title) {
    var top = document.createElement('a');
    top.className = 'nm-toc-title';
    top.href = '#';
    top.textContent = 'Contents';
    nav.appendChild(top);
  }
  var list = document.createElement('ol');
  var current = null;
  var links = [];
  heads.forEach(function (h, i) {
    var target = h.classList.contains('nm-panel-title')
      ? (h.closest('.nm-analysis-group') || h.closest('.nm-panel-heading') || h)
      : h;
    if (!target.id) target.id = slug(h.textContent, i);
    var a = document.createElement('a');
    a.href = '#' + target.id;
    a.textContent = h.textContent;
    var li = document.createElement('li');
    li.appendChild(a);
    if (h.tagName === 'H1') {
      li.className = 'nm-toc-section';
      list.appendChild(li);
      current = null;
      var sub = document.createElement('ol');
      sub.className = 'nm-toc-sub';
      li.appendChild(sub);
      current = sub;
    } else if (current) {
      current.appendChild(li);
    } else {
      list.appendChild(li);
    }
    links.push({ a: a, el: target });
  });
  nav.appendChild(list);
  main.insertBefore(nav, main.firstChild);

  if (!('IntersectionObserver' in window)) return;
  var visible = new Map();
  var mark = function () {
    var best = null;
    links.forEach(function (l) {
      var r = visible.get(l.el);
      if (r !== undefined && (best === null || r < best.r)) best = { l: l, r: r };
    });
    if (!best) return;
    links.forEach(function (l) { l.a.removeAttribute('aria-current'); });
    best.l.a.setAttribute('aria-current', 'true');
    var box = nav.getBoundingClientRect();
    var ar = best.l.a.getBoundingClientRect();
    if (ar.top < box.top + 40 || ar.bottom > box.bottom - 40) {
      best.l.a.scrollIntoView({ block: 'nearest' });
    }
  };
  var io = new IntersectionObserver(function (entries) {
    entries.forEach(function (e) {
      if (e.isIntersecting) visible.set(e.target, e.boundingClientRect.top);
      else visible.delete(e.target);
    });
    mark();
  }, { rootMargin: '0px 0px -60% 0px' });
  links.forEach(function (l) { io.observe(l.el); });
});

// Cross-section controls: one threshold switch for every analysis, and
// summary-matrix cells that open a specific map.
document.addEventListener('DOMContentLoaded', function () {
  var groups = Array.prototype.slice.call(document.querySelectorAll('[data-nm-map-group]'));
  var requestMap = function (group, mapId) {
    group.dispatchEvent(new CustomEvent('nm-map-request', {
      detail: { analysisId: group.getAttribute('data-nm-map-group'), mapId: mapId }
    }));
  };
  var showLabel = function (label) {
    groups.forEach(function (group) {
      var tab = Array.prototype.find.call(
        group.querySelectorAll('[role=tab][data-nm-label]'),
        function (t) { return t.getAttribute('data-nm-label') === label; }
      );
      if (tab) tab.click();
    });
  };

  var bar = document.querySelector('[data-nm-global-tabs]');
  if (bar) {
    var buttons = Array.prototype.slice.call(bar.querySelectorAll('button[data-nm-label]'));
    var syncing = false;
    var setPressed = function (label) {
      buttons.forEach(function (b) {
        b.setAttribute('aria-pressed', b.getAttribute('data-nm-label') === label ? 'true' : 'false');
      });
    };
    buttons.forEach(function (b) {
      b.addEventListener('click', function () {
        syncing = true;
        showLabel(b.getAttribute('data-nm-label'));
        syncing = false;
        setPressed(b.getAttribute('data-nm-label'));
      });
    });
    // Reflect the shared state: pressed only while every section agrees.
    var reflect = function () {
      if (syncing) return;
      var labels = groups.map(function (g) {
        var sel = g.querySelector('[role=tab][aria-selected=true]');
        return sel ? sel.getAttribute('data-nm-label') : null;
      });
      var same = labels.length && labels.every(function (l) { return l === labels[0]; });
      setPressed(same ? labels[0] : null);
    };
    document.addEventListener('nm-map-change', reflect);
    reflect();
  }

  // Fade the right edge of a scrolling table only while columns remain hidden.
  document.querySelectorAll('.nm-table-wrap').forEach(function (wrap) {
    var check = function () {
      wrap.classList.toggle('is-clipped', wrap.scrollLeft + wrap.clientWidth < wrap.scrollWidth - 1);
    };
    wrap.addEventListener('scroll', check, { passive: true });
    window.addEventListener('resize', check);
    check();
  });

  document.querySelectorAll('[data-nm-goto-group]').forEach(function (cell) {
    cell.addEventListener('click', function (e) {
      e.preventDefault();
      var id = cell.getAttribute('data-nm-goto-group');
      var group = groups.find(function (g) { return g.getAttribute('data-nm-map-group') === id; });
      if (!group) return;
      requestMap(group, cell.getAttribute('data-nm-goto-map'));
      group.scrollIntoView({ block: 'start' });
    });
  });
});
