// neuromosaic montage report behaviour.
//
// Page services shared by the sections below (window.NeuroMosaicReport):
//  - figures load near the viewport, then progressively in idle time, and all
//    of them (light variants) before printing;
//  - in-page jumps (contents, glance cards, deep links, cold loads) land on
//    their target and are corrected if layout above it changes while settling;
//  - one polite live region for status messages.
(function () {
  var api = window.NeuroMosaicReport = window.NeuroMosaicReport || {};
  var ready = function (fn) {
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', fn);
    else fn();
  };

  // ---- Status announcements -------------------------------------------------
  var live = null;
  api.announce = function (text) {
    if (!live) {
      live = document.createElement('div');
      live.className = 'nm-sr-only';
      live.id = 'nm-live';
      live.setAttribute('role', 'status');
      live.setAttribute('aria-live', 'polite');
      document.body.appendChild(live);
    }
    live.textContent = '';
    window.setTimeout(function () { live.textContent = text; }, 30);
  };
  ready(function () { api.announce(''); });

  // ---- Deferred figures -------------------------------------------------------
  var deferred = function () {
    return Array.prototype.slice.call(document.querySelectorAll('img[data-nm-src]'));
  };
  var whenLoaded = function (img) {
    return new Promise(function (resolve) {
      if (img.complete && img.naturalWidth) return resolve();
      var done = function () { img.removeEventListener('load', done); img.removeEventListener('error', done); resolve(); };
      img.addEventListener('load', done);
      img.addEventListener('error', done);
      window.setTimeout(done, 20000);
    });
  };
  var loadImage = api.loadImage = function (img, priority) {
    var src = img.getAttribute('data-nm-src');
    if (!src) return whenLoaded(img);
    var pic = img.parentNode && img.parentNode.tagName === 'PICTURE' ? img.parentNode : null;
    if (pic) {
      Array.prototype.forEach.call(pic.querySelectorAll('source[data-nm-srcset]'), function (source) {
        source.setAttribute('srcset', source.getAttribute('data-nm-srcset'));
        source.removeAttribute('data-nm-srcset');
      });
    }
    if (priority) img.setAttribute('fetchpriority', priority);
    img.removeAttribute('data-nm-src');
    img.setAttribute('src', src);
    img.classList.remove('nm-defer');
    return whenLoaded(img);
  };
  api.loadAll = function () { return Promise.all(deferred().map(function (i) { return loadImage(i, 'high'); })); };

  // Light figure-of-record URLs that have finished loading (print needs them).
  var lightReady = {};
  var noteLoaded = function (img) {
    var src = img.getAttribute('src');
    if (src && img.naturalWidth && img.currentSrc && img.currentSrc.indexOf(src) !== -1) {
      lightReady[src] = true;
    }
  };

  // Which maps to prefetch: the ones on screen plus one either side (or the
  // nearest map and its neighbour when none is on screen). Print copies are
  // never fetched speculatively; beforeprint fetches what printing needs.
  var maps = function () {
    return Array.prototype.slice.call(document.querySelectorAll('article.nm-map, .nm-map'))
      .filter(function (m) { return !m.parentElement || !m.parentElement.closest('.nm-map'); });
  };
  var neighbourImages = function () {
    var all = maps();
    if (!all.length) return deferred();
    var vh = window.innerHeight || document.documentElement.clientHeight;
    var lo = -1, hi = -1, nearest = -1, best = Infinity;
    all.forEach(function (m, i) {
      var r = m.getBoundingClientRect();
      if (r.bottom > 0 && r.top < vh) { if (lo < 0) lo = i; hi = i; }
      var d = r.top >= vh ? r.top - vh : r.bottom <= 0 ? -r.bottom : 0;
      if (d < best) { best = d; nearest = i; }
    });
    if (lo < 0) { lo = hi = nearest; }
    lo = Math.max(0, lo - 1);
    hi = Math.min(all.length - 1, hi + 1);
    var out = [];
    for (var i = lo; i <= hi; i++) {
      Array.prototype.push.apply(out, all[i].querySelectorAll('img[data-nm-src]'));
    }
    return out;
  };
  // Slow or metered connections (and Save-Data) get no speculative loading.
  var frugal = function () {
    var c = navigator.connection;
    if (!c) return false;
    if (c.saveData) return true;
    if (c.type === 'cellular') return true;
    return !!c.effectiveType && c.effectiveType !== '4g';
  };
  api.prefetchNeighbours = function () {
    if (frugal()) return Promise.resolve();
    return Promise.all(neighbourImages().map(function (img) { return loadImage(img, 'low'); }));
  };

  ready(function () {
    document.addEventListener('load', function (e) {
      if (e.target && e.target.tagName === 'IMG') noteLoaded(e.target);
    }, true);
    Array.prototype.forEach.call(document.querySelectorAll('.nm-figure img'), noteLoaded);
    var imgs = deferred();
    if (!imgs.length) return;
    if ('IntersectionObserver' in window) {
      var io = new IntersectionObserver(function (entries) {
        entries.forEach(function (e) {
          if (!e.isIntersecting) return;
          io.unobserve(e.target);
          loadImage(e.target, 'high');
        });
      }, { rootMargin: '600px 0px 900px 0px' });
      imgs.forEach(function (img) { io.observe(img); });
    } else {
      imgs.forEach(function (img) { loadImage(img); });
      return;
    }
    // Neighbouring maps load shortly after the page has loaded (once the
    // first screen is complete, ~2.5 s, so a jump or an early print finds
    // them) and again whenever scrolling comes to rest. Nothing further is
    // fetched without the reader moving there.
    var idle = window.requestIdleCallback || function (fn) { return window.setTimeout(fn, 200); };
    var timer = null;
    var soon = function (ms) {
      window.clearTimeout(timer);
      timer = window.setTimeout(function () { idle(api.prefetchNeighbours, { timeout: 1500 }); }, ms);
    };
    var start = function () {
      soon(2500);
      window.addEventListener('scroll', function () { if (deferred().length) soon(400); }, { passive: true });
    };
    if (document.readyState === 'complete') start();
    else window.addEventListener('load', start);
  });

  // ---- Print: every figure, light ------------------------------------------------
  // Paper always gets the full-size light figure of record: dark and narrow
  // <source>s are switched off for the print, and restored afterwards.
  var printing = false;
  var toLight = function () {
    Array.prototype.forEach.call(document.querySelectorAll('source[data-nm-theme]'), function (src) {
      src.setAttribute('media', 'not all');
    });
  };
  var printImages = function () {
    return Array.prototype.slice.call(document.querySelectorAll('.nm-figure img, .nm-atlas-thumb img'))
      .filter(function (img) { return !img.closest('noscript'); });
  };
  // True once the image shows its own (light, full-size) source.
  var showsLight = function (img) {
    return !!(img.complete && img.naturalWidth && img.currentSrc && img.src &&
      img.currentSrc === img.src);
  };
  // Light copies that failed to load (404, unreadable file): they will never
  // arrive, so paper says the figure is unavailable instead of "loading".
  var failed = {};
  ready(function () {
    document.addEventListener('error', function (e) {
      var img = e.target;
      if (img && img.tagName === 'IMG' && img.src && (!img.currentSrc || img.currentSrc === img.src)) {
        failed[img.src] = true;
      }
    }, true);
  });
  var broken = function (img) {
    return !!(img.src && failed[img.src]) ||
      !!(img.complete && !img.naturalWidth && img.currentSrc && img.currentSrc === img.src);
  };
  var whenLight = function (img) {
    return new Promise(function (resolve) {
      var t0 = Date.now();
      var check = function () {
        if (showsLight(img)) {
          lightReady[img.getAttribute('src')] = true;
          var d = typeof img.decode === 'function' ? img.decode() : Promise.resolve();
          d.then(resolve, resolve);
          return;
        }
        // A broken image (or a 30 s stall) must not block printing.
        if (broken(img)) return resolve();
        if (Date.now() - t0 > 30000) return resolve();
        window.setTimeout(check, 60);
      };
      check();
    });
  };
  // A figure whose light copy has not arrived when printing starts says so on
  // paper (instead of leaving a silent hole); the note goes as soon as the
  // image arrives. A figure whose file failed to load says it is unavailable.
  // A glance thumbnail still showing its dark copy is left blank on paper
  // (never a black box). The report's own print (the "Print / Save PDF"
  // button, Ctrl/Cmd+P) loads every figure first, so the notes only show
  // when printing is started from the browser menu before the page has
  // loaded its figures, or when the reader chose "Print now".
  // Headless PDF pipelines (Chrome --print-to-pdf, pagedown::chrome_print())
  // should await NeuroMosaicReport.preparePrint() first; see
  // ?render_montage_report.
  var pending = function () {
    var count = 0;
    Array.prototype.forEach.call(document.querySelectorAll('.nm-figure, .nm-atlas-thumb'), function (box) {
      var img = box.querySelector('picture img, img');
      if (!img || img.closest('noscript')) return;
      var src = img.getAttribute('src') || img.getAttribute('data-nm-src');
      if (!src || src.indexOf('data:') === 0 || lightReady[src] || showsLight(img)) return;
      count++;
      if (broken(img)) {
        box.classList.add('nm-print-failed');
        return;
      }
      box.classList.add('nm-print-pending');
      var clear = function () {
        if (broken(img)) {
          box.classList.remove('nm-print-pending');
          box.classList.add('nm-print-failed');
        } else if (showsLight(img)) {
          lightReady[src] = true;
          box.classList.remove('nm-print-pending');
        } else {
          return;
        }
        img.removeEventListener('load', clear);
        img.removeEventListener('error', clear);
      };
      img.addEventListener('load', clear);
      img.addEventListener('error', clear);
    });
    return count;
  };
  var startPrint = function () {
    printing = true;
    toLight();
    printImages().forEach(function (img) { if (img.loading === 'lazy') img.loading = 'eager'; });
  };
  window.addEventListener('beforeprint', function () {
    startPrint();
    api.loadAll();
    pending();
  });
  var afterprints = 0;
  window.addEventListener('afterprint', function () {
    afterprints++;
    printing = false;
    preparing = null;
    Array.prototype.forEach.call(document.querySelectorAll('.nm-print-pending, .nm-print-failed'), function (f) {
      f.classList.remove('nm-print-pending');
      f.classList.remove('nm-print-failed');
    });
    if (typeof api.applyTheme === 'function') api.applyTheme();
  });
  // Load and decode every light figure, then resolve: the page is ready to print.
  var preparing = null;
  api.preparePrint = function () {
    if (preparing) return preparing;
    startPrint();
    preparing = api.loadAll().then(function () {
      return Promise.all(printImages().map(whenLight));
    });
    return preparing;
  };
  // Open the print dialog. Some hosts return from print() without a dialog
  // and without an afterprint event (the RStudio Viewer, sandboxed iframes,
  // a blocked print): the page then leaves print state by itself, so the
  // theme, the dark figures and the disclosures come back.
  var printPage = function () {
    pending();
    var seen = afterprints;
    window.print();
    window.setTimeout(function () {
      if (afterprints === seen && printing) window.dispatchEvent(new Event('afterprint'));
    }, 1000);
  };
  // The report's own print waits for the figures for at most PRINT_WAIT ms,
  // showing its progress and a "Print now" button; a second Ctrl/Cmd+P (or
  // the button) prints at once, and figures still missing say so on paper.
  var PRINT_WAIT = 10000;
  var printNow = null;
  var sheet = null;
  var sheetText = null;
  var showSheet = function () {
    if (!sheet) {
      sheet = document.createElement('div');
      sheet.className = 'nm-print-sheet';
      sheetText = document.createElement('span');
      sheetText.className = 'nm-print-sheet-text';
      var now = document.createElement('button');
      now.type = 'button';
      now.className = 'nm-print-now';
      now.textContent = 'Print now';
      now.title = 'Print without waiting; figures still loading are marked on paper (Ctrl/Cmd+P again does the same)';
      now.addEventListener('click', function () { if (printNow) printNow(); });
      sheet.appendChild(sheetText);
      sheet.appendChild(now);
      document.body.appendChild(sheet);
    }
    sheet.hidden = false;
  };
  var progress = function () {
    var imgs = printImages();
    var done = imgs.filter(function (img) { return showsLight(img) || broken(img); }).length;
    if (sheetText) {
      sheetText.textContent = 'Preparing the print: ' + done + ' of ' + imgs.length + ' figures loaded\u2026';
    }
  };
  api.printReport = function () {
    if (printNow) { printNow(); return Promise.resolve(); }
    var buttons = document.querySelectorAll('[data-nm-print]');
    Array.prototype.forEach.call(buttons, function (b) { b.setAttribute('aria-busy', 'true'); });
    return new Promise(function (resolve) {
      var finished = false, tick = null;
      var slow = window.setTimeout(function () {
        showSheet();
        progress();
        tick = window.setInterval(progress, 250);
        api.announce('Preparing the print: loading every figure. Press Print now, or Ctrl/Cmd+P again, to print at once.');
      }, 300);
      var go = function () {
        if (finished) return;
        finished = true;
        printNow = null;
        window.clearTimeout(slow);
        window.clearTimeout(cap);
        window.clearInterval(tick);
        if (sheet) sheet.hidden = true;
        Array.prototype.forEach.call(buttons, function (b) { b.removeAttribute('aria-busy'); });
        resolve();
        printPage();
      };
      var cap = window.setTimeout(go, PRINT_WAIT);
      printNow = go;
      api.preparePrint().then(go, go);
    });
  };
  // Ctrl/Cmd+P prints through the report, so every figure is on paper. A
  // layout without a Latin "p" is matched by the key's position.
  document.addEventListener('keydown', function (e) {
    if (!(e.ctrlKey || e.metaKey) || e.altKey || e.shiftKey) return;
    var key = e.key || '';
    if (key !== 'p' && key !== 'P' && !(e.code === 'KeyP' && !/^[a-z]$/i.test(key))) return;
    e.preventDefault();
    api.printReport();
  }, true);
  var printButton = function (cls) {
    var b = document.createElement('button');
    b.type = 'button';
    b.className = 'nm-print-action ' + cls;
    b.setAttribute('data-nm-print', '');
    b.textContent = 'Print / Save PDF';
    b.title = 'Loads every figure, then opens the print dialog (Ctrl/Cmd+P does the same)';
    b.addEventListener('click', function () { api.printReport(); });
    return b;
  };
  api.printButton = printButton;
  ready(function () {
    var header = document.querySelector('.nm-report-header');
    if (!header || header.querySelector('[data-nm-print]')) return;
    // The control joins the labels line's last unbreakable run ("Methods",
    // a middle dot, "Print / Save PDF"), so a wrap never leaves the separator
    // at the start of a line; without that run it takes a line of its own.
    var labels = header.querySelector('.nm-report-labels');
    var tail = labels && labels.lastElementChild;
    var after = tail && tail.nextSibling;
    var last = tail && tail.classList.contains('nm-nowrap') &&
      (!after || (after.nodeType === 3 && !after.nextSibling && !after.textContent.trim()));
    if (last) {
      var sep = document.createElement('span');
      sep.className = 'nm-print-sep';
      sep.setAttribute('aria-hidden', 'true');
      sep.textContent = ' \u00B7 ';
      tail.appendChild(sep);
      tail.appendChild(printButton('nm-print-header'));
    } else {
      var row = document.createElement('span');
      row.className = 'nm-print-row';
      row.appendChild(printButton('nm-print-header'));
      (labels || header).appendChild(row);
    }
  });
  api.isPrinting = function () { return printing; };

  // ---- Jumps that land ---------------------------------------------------------
  // Scroll the target to its scroll-margin line and keep it there while the
  // layout above settles (routers and viewers mount asynchronously). Any
  // reader input cancels the hold.
  var hold = null;
  var marginOf = function (el) {
    var m = parseFloat(getComputedStyle(el).scrollMarginTop);
    return isFinite(m) ? m : 0;
  };
  var release = function () { if (hold) { window.clearInterval(hold.timer); hold = null; } };
  // The last reader input (wheel, touch, key, pointer), for code that acts
  // later on the reader's behalf and must not override what they did since.
  api.lastInput = 0;
  ['wheel', 'touchstart', 'keydown', 'mousedown'].forEach(function (type) {
    window.addEventListener(type, function (e) {
      api.lastInput = performance.now();
      if (!hold || e.timeStamp - hold.started < 60) return;
      if (type === 'keydown' && (e.key === 'Tab' || e.key === 'Shift')) return;
      release();
    }, { passive: true, capture: true });
  });
  api.holdTarget = function (el, ms, instant) {
    if (!el) return;
    release();
    api.lastTarget = el;
    var last = null, still = 0;
    hold = { el: el, started: performance.now(), until: performance.now() + (ms || 2500), corrections: 0 };
    if (instant) {
      var d0 = el.getBoundingClientRect().top - marginOf(el);
      if (Math.abs(d0) > 2) window.scrollTo({ top: window.scrollY + d0, behavior: 'instant' });
    }
    hold.timer = window.setInterval(function () {
      if (!hold) return;
      var y = window.scrollY;
      still = y === last ? still + 1 : 0;
      last = y;
      if (still >= 2) {
        var delta = hold.el.getBoundingClientRect().top - marginOf(hold.el);
        var max = document.documentElement.scrollHeight - window.innerHeight;
        var canMove = delta > 0 ? y < max - 1 : y > 0;
        if (Math.abs(delta) > 2 && canMove && hold.corrections < 12) {
          hold.corrections++;
          window.scrollTo({ top: y + delta, behavior: 'instant' });
          still = 0;
        }
      }
      if (performance.now() > hold.until) release();
    }, 50);
  };
  var targetOf = function (hash) {
    if (!hash || hash.length < 2) return null;
    var id;
    try { id = decodeURIComponent(hash.slice(1)); } catch (err) { id = hash.slice(1); }
    return document.getElementById(id);
  };
  // In-page link clicks hold their target. A hash change the reader did not
  // just click (Back/Forward, a typed hash) is the browser's to scroll: any
  // earlier hold is released so it never fights history traversal.
  var clicked = null;
  document.addEventListener('click', function (e) {
    if (e.defaultPrevented && !(e.target.closest && e.target.closest('[data-nm-goto-group]'))) return;
    var a = e.target.closest && e.target.closest('a[href^="#"]');
    if (!a || e.button !== 0 || e.metaKey || e.ctrlKey || e.shiftKey || e.altKey) return;
    var el = targetOf(a.getAttribute('href'));
    if (!el) return;
    clicked = { el: el, at: performance.now() };
    // Remember where the reader was, so Back returns there exactly (the
    // browser's own restoration is lost when smooth scrolls overlap).
    try {
      var st = history.state && typeof history.state === 'object' ? history.state : {};
      var copy = {};
      for (var k in st) if (Object.prototype.hasOwnProperty.call(st, k)) copy[k] = st[k];
      copy.nmScrollY = window.scrollY;
      history.replaceState(copy, '');
    } catch (err) { /* history unavailable: the browser restores */ }
    // Let the browser start its own (smooth) scroll, then hold the landing.
    window.setTimeout(function () { api.holdTarget(el, 3000); }, 0);
  });
  var traversal = function () {
    var el = targetOf(location.hash);
    var fresh = clicked && clicked.el === el && performance.now() - clicked.at < 1000;
    if (!fresh) release();
  };
  window.addEventListener('hashchange', traversal);
  window.addEventListener('popstate', function (e) {
    traversal();
    var y = e.state && typeof e.state.nmScrollY === 'number' ? e.state.nmScrollY : null;
    if (y === null) return;
    // Back/Forward to a position the page recorded: go there at once.
    var html = document.documentElement;
    var restore = function () {
      html.style.scrollBehavior = 'auto';
      window.scrollTo(0, y);
      html.style.scrollBehavior = '';
    };
    restore();
    window.requestAnimationFrame(function () { if (Math.abs(window.scrollY - y) > 2) restore(); });
  });
  // A shared deep link (a fresh navigation to URL#hash): land exactly once
  // the page script has built the contents bar and the viewers' routers.
  // Reload and Back/Forward keep the browser's own restored position.
  var navigationType = function () {
    var nav = performance.getEntriesByType && performance.getEntriesByType('navigation')[0];
    if (nav && nav.type) return nav.type;
    var legacy = performance.navigation && performance.navigation.type;
    return legacy === 1 ? 'reload' : legacy === 2 ? 'back_forward' : 'navigate';
  };
  api.navigationType = navigationType;
  ready(function () {
    var el = targetOf(location.hash);
    if (!el || navigationType() !== 'navigate') return;
    api.holdTarget(el, 4000, true);
    window.addEventListener('load', function () {
      if (api.lastInput > 0) return;
      if (hold && hold.el === el) return;
      api.holdTarget(el, 2000, true);
    });
  });
}());

// Map-variant tabs and the contents sidebar.
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

  // ---- Contents sidebar ---------------------------------------------------
  var main = document.querySelector('.main-container') || document.body;
  var skip = document.querySelector('.nm-skip');
  if (skip && skip.parentNode !== document.body) document.body.insertBefore(skip, document.body.firstChild);
  var isEmptySection = function (h) {
    var sec = h.parentElement;
    if (!sec || !sec.classList.contains('section')) return false;
    return Array.prototype.every.call(sec.children, function (c) {
      return c === h || c.tagName === 'SCRIPT' || c.tagName === 'STYLE';
    });
  };
  // Sections are the pandoc section headings (h2 since the report title is
  // the only h1; older templates still use h1); maps are the panel titles.
  var isSectionHead = function (h) {
    return (h.tagName === 'H1' || h.tagName === 'H2') && !h.classList.contains('nm-panel-title') &&
      !!h.parentElement && h.parentElement.classList.contains('section');
  };
  var heads = Array.prototype.slice.call(main.querySelectorAll('h1, h2, .nm-panel-title'))
    .filter(function (h) { return h.classList.contains('nm-panel-title') || isSectionHead(h); })
    .filter(function (h) {
      return !h.classList.contains('title') && !h.classList.contains('nm-report-title') &&
        !h.closest('#header') && h.offsetParent !== null && !isEmptySection(h);
    });
  if (heads.length === 0) return;
  var slug = function (text, i) {
    return 'nm-' + (text || 'section').toLowerCase().replace(/[^a-z0-9]+/g, '-').replace(/^-|-$/g, '') + '-' + i;
  };
  var nav = document.createElement('nav');
  nav.className = 'nm-toc';
  nav.id = 'nm-toc';
  nav.setAttribute('aria-label', 'Contents');
  var top = document.createElement('p');
  top.className = 'nm-toc-title';
  top.setAttribute('aria-hidden', 'true');
  top.textContent = 'Contents';
  nav.appendChild(top);
  var list = document.createElement('ol');
  var current = null;
  var links = [];
  heads.forEach(function (h, i) {
    var isSection = isSectionHead(h);
    var target = isSection
      ? (h.parentElement && h.parentElement.classList.contains('section') ? h.parentElement : h)
      : (h.closest('.nm-map') || h.closest('.nm-analysis-group') || h.closest('.nm-panel-heading') || h);
    if (!target.id) target.id = slug(h.textContent, i);
    var a = document.createElement('a');
    a.href = '#' + target.id;
    a.textContent = h.textContent;
    var li = document.createElement('li');
    li.appendChild(a);
    if (isSection) {
      li.className = 'nm-toc-section';
      list.appendChild(li);
      var sub = document.createElement('ol');
      sub.className = 'nm-toc-sub';
      li.appendChild(sub);
      current = sub;
    } else if (current) {
      current.appendChild(li);
    } else {
      list.appendChild(li);
    }
    links.push({ a: a, el: target, section: isSection });
  });
  nav.appendChild(list);
  var anchor = skip && skip.parentNode === document.body ? skip.nextSibling : document.body.firstChild;
  document.body.insertBefore(nav, anchor);

  // Narrow screens and zoom: a sticky bar opens the same contents as a panel.
  var bar = document.createElement('div');
  bar.className = 'nm-minibar';
  var toggle = document.createElement('button');
  toggle.type = 'button';
  toggle.className = 'nm-minibar-toggle';
  toggle.setAttribute('aria-expanded', 'false');
  toggle.setAttribute('aria-controls', 'nm-toc');
  toggle.innerHTML = '<span class="nm-minibar-icon" aria-hidden="true"></span><span>Contents</span>';
  var here = document.createElement('span');
  here.className = 'nm-minibar-current';
  here.setAttribute('aria-hidden', 'true');
  bar.appendChild(toggle);
  bar.appendChild(here);
  var nmr = window.NeuroMosaicReport || {};
  if (nmr.printButton) bar.appendChild(nmr.printButton('nm-print-minibar'));
  document.body.insertBefore(bar, nav);
  var setOpen = function (open, focusToggle) {
    nav.classList.toggle('is-open', open);
    toggle.setAttribute('aria-expanded', open ? 'true' : 'false');
    if (open) {
      var cur = nav.querySelector('a[aria-current="true"]') || nav.querySelector('a');
      if (cur) cur.focus();
    } else if (focusToggle) {
      toggle.focus();
    }
  };
  toggle.addEventListener('click', function () { setOpen(!nav.classList.contains('is-open'), false); });
  nav.addEventListener('click', function (e) { if (e.target.closest('a')) setOpen(false, false); });
  document.addEventListener('keydown', function (e) {
    if (e.key === 'Escape' && nav.classList.contains('is-open')) setOpen(false, true);
  });
  document.addEventListener('click', function (e) {
    if (nav.classList.contains('is-open') && !nav.contains(e.target) && !bar.contains(e.target)) setOpen(false, false);
  });
  // Tabbing out of the open panel closes it, so focus never lands on content
  // hidden beneath it.
  var leave = function (e) {
    if (!nav.classList.contains('is-open')) return;
    var next = e.relatedTarget;
    if (next && (nav.contains(next) || bar.contains(next))) return;
    if (!next) return; // focus left the window, or a click handled above
    setOpen(false, false);
  };
  nav.addEventListener('focusout', leave);
  toggle.addEventListener('focusout', leave);

  // Scroll spy: the active entry is the last one whose target starts above a
  // reading line; at the very bottom it is the last entry.
  var active = null;
  var mark = function () {
    var line = Math.min(window.innerHeight * 0.3, 240);
    var best = null;
    links.forEach(function (l) {
      if (l.el.getBoundingClientRect().top <= line) best = l;
    });
    var doc = document.documentElement;
    if (window.innerHeight + window.scrollY >= doc.scrollHeight - 2) {
      // At the bottom the last sections cannot reach the reading line: keep
      // the one the reader just jumped to if it is on screen, else the last.
      var jumped = window.NeuroMosaicReport && window.NeuroMosaicReport.lastTarget;
      var hit = jumped && links.filter(function (l) { return l.el === jumped; })[0];
      var r = hit && hit.el.getBoundingClientRect();
      best = hit && r.top >= -2 && r.top < window.innerHeight ? hit : links[links.length - 1];
    }
    if (!best) best = links[0];
    if (best === active) return;
    active = best;
    links.forEach(function (l) { l.a.removeAttribute('aria-current'); });
    best.a.setAttribute('aria-current', 'true');
    here.textContent = best.a.textContent;
    if (getComputedStyle(nav).position === 'fixed' && !nav.classList.contains('is-open') &&
        getComputedStyle(bar).display !== 'none') return;
    var box = nav.getBoundingClientRect();
    var ar = best.a.getBoundingClientRect();
    if (ar.top < box.top + 40 || ar.bottom > box.bottom - 40) {
      nav.scrollTop += ar.top - box.top - box.height / 2;
    }
  };
  var queued = false;
  window.addEventListener('scroll', function () {
    if (queued) return;
    queued = true;
    window.requestAnimationFrame(function () { queued = false; mark(); });
  }, { passive: true });
  window.addEventListener('resize', mark);
  mark();
});

// The view router (Static / Slices / 3D surface) is created by the surface
// bridge; it belongs in the map's header, under the spec line, and is named
// for its map. Selecting an interactive view swaps it in for the static
// figures (article[data-nm-view] drives the CSS) and brings it into view.
// State follows the bridge's `nm-view-change` event, falling back to the
// radios' own change events and the volume panel's "Hide" event, so it holds
// wherever the router was inserted and whichever script ran first.
(function () {
  var reduceMotion = function () {
    return window.matchMedia && window.matchMedia('(prefers-reduced-motion: reduce)').matches;
  };
  var esc = function (id) { return window.CSS && CSS.escape ? CSS.escape(id) : id; };
  var routerFor = function (id) { return document.querySelector('[data-nm-view-router="' + esc(id) + '"]'); };
  var articleFor = function (id) {
    var slot = document.querySelector('[data-nm-view-slot="' + esc(id) + '"]');
    var art = slot && slot.closest('.nm-map');
    if (art) return art;
    var router = routerFor(id);
    return router ? router.closest('.nm-map') : null;
  };
  var viewerFor = function (art, id, view) {
    var sel = view === 'slices' ? '[data-nm-volume-analysis="' + esc(id) + '"]'
      : view === 'surface' ? '[data-nm-surface-analysis="' + esc(id) + '"]' : null;
    if (!sel) return null;
    return (art && art.querySelector(sel)) || document.querySelector(sel);
  };
  // Keep the opened viewer on screen. When the router and the whole viewer
  // fit in the viewport, scroll the least distance that shows all of it;
  // when they do not, put the router near the top with the viewer under it.
  var topOffset = function () {
    var bar = document.querySelector('.nm-minibar');
    return bar && getComputedStyle(bar).display !== 'none' ? bar.getBoundingClientRect().height + 8 : 12;
  };
  var reveal = function (art, id, view) {
    var host = viewerFor(art, id, view);
    if (!host || host.hidden) return;
    var box = host.getBoundingClientRect();
    if (!box.height) return;
    var vh = window.innerHeight || document.documentElement.clientHeight;
    var router = routerFor(id);
    var head = router && router.getBoundingClientRect().height ? router.getBoundingClientRect() : box;
    var offset = topOffset();
    var top = Math.min(head.top, box.top);
    var bottom = box.bottom;
    var delta = 0;
    if (bottom - top <= vh - offset - 8) {
      if (top < offset) delta = top - offset;
      else if (bottom > vh - 8) delta = bottom - (vh - 8);
    } else if (Math.abs(top - offset) > 4) {
      delta = top - offset;
    }
    if (Math.abs(delta) < 2) return;
    window.scrollBy({ top: delta, behavior: reduceMotion() ? 'auto' : 'smooth' });
  };
  var apply = function (id, view, fromUser) {
    var art = articleFor(id);
    if (!art) return;
    var previous = art.getAttribute('data-nm-view');
    art.setAttribute('data-nm-view', view || 'static');
    var router = routerFor(id);
    if (router) {
      var radio = router.querySelector('input[value="' + view + '"]');
      if (radio && !radio.checked) radio.checked = true;
    }
    if (fromUser && view && view !== 'static' && previous !== view) revealSoon(art, id, view);
  };
  // Reveal now and again as the viewer mounts and grows, unless the reader
  // scrolls away in the meantime.
  var inputSince = function (t) {
    var r = window.NeuroMosaicReport;
    return !!(r && r.lastInput && r.lastInput > t);
  };
  var revealSoon = function (art, id, view, since) {
    var expected = null;
    var settleMs = reduceMotion() ? 30 : 600;
    if (since === undefined) since = performance.now();
    var run = function () {
      if (art.getAttribute('data-nm-view') !== view) return;
      if (inputSince(since)) return;
      if (expected !== null && Math.abs(window.scrollY - expected) > 2) return;
      reveal(art, id, view);
      window.setTimeout(function () { expected = window.scrollY; }, settleMs);
    };
    window.requestAnimationFrame(run);
    [700, 1500, 2800].forEach(function (ms) { window.setTimeout(run, ms); });
  };
  var label = function (router) {
    var id = router.getAttribute('data-nm-view-router');
    var slot = document.querySelector('[data-nm-view-slot="' + esc(id) + '"]');
    if (slot && router.parentNode !== slot) slot.appendChild(router);
    var name = slot && slot.getAttribute('data-nm-view-label');
    var legend = router.querySelector('legend');
    if (legend && name && legend.textContent.indexOf(name) === -1 && !legend.querySelector('.nm-sr-only')) {
      var sr = document.createElement('span');
      sr.className = 'nm-sr-only';
      sr.textContent = ' for ' + name;
      legend.appendChild(sr);
    }
    var checked = router.querySelector('input:checked');
    apply(id, checked ? checked.value : 'static', false);
    // The router can arrive (or turn Slices off) after the tables were bound.
    if (typeof bindTables === 'function') bindTables();
  };
  var scan = function () { document.querySelectorAll('[data-nm-view-router]').forEach(label); };
  document.addEventListener('nm-view-change', function (e) {
    var d = e.detail || {};
    var id = d.analysisId || (e.target && e.target.getAttribute && e.target.getAttribute('data-nm-view-router'));
    if (id) apply(id, d.view || 'static', d.source !== 'init');
  });
  document.addEventListener('change', function (e) {
    var input = e.target;
    var router = input && input.closest && input.closest('[data-nm-view-router]');
    if (router && input.checked) apply(router.getAttribute('data-nm-view-router'), input.value, true);
  });
  document.addEventListener('nm-volume-hidden', function (e) {
    var id = e.detail && e.detail.analysisId;
    if (id) apply(id, 'static', false);
  });
  var start = function () {
    scan();
    if (!('MutationObserver' in window)) return;
    var mo = new MutationObserver(function (records) {
      for (var i = 0; i < records.length; i++) {
        var added = records[i].addedNodes;
        for (var j = 0; j < added.length; j++) {
          var n = added[j];
          if (n.nodeType !== 1) continue;
          if (n.matches('[data-nm-view-router]')) label(n);
          else if (n.querySelector) n.querySelectorAll('[data-nm-view-router]').forEach(label);
        }
      }
    });
    mo.observe(document.body, { childList: true, subtree: true });
    window.addEventListener('load', scan);
  };
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start);
  else start();

  // Peak-table rows open the Slices view at the peak. Rows become buttons
  // only where the map has an interactive volume; otherwise they stay text.
  var goTo = function (art, id, xyz, what) {
    var router = routerFor(id);
    var radio = router && router.querySelector('input[value="slices"]');
    var host = viewerFor(art, id, 'slices');
    if (!host) return;
    // What the reader is doing when they ask: if they scroll, type or move
    // focus before the viewer is ready, it must not pull them back.
    var asked = performance.now();
    var askedFocus = document.activeElement;
    var place = function () {
      var api = window.NeuroMosaicVolumeReport;
      var state = api && api.getState && api.getState(host);
      if (state && state.viewer && typeof state.viewer.setWorldCoord === 'function') {
        state.viewer.setWorldCoord(xyz);
        return true;
      }
      return false;
    };
    if (radio) {
      if (!radio.checked) radio.click();
      else apply(id, 'slices', true);
    } else {
      var toggle = host.querySelector('[data-nm-volume-toggle]');
      if (toggle && toggle.getAttribute('aria-expanded') !== 'true') toggle.click();
    }
    // Once the cursor is at the peak: say so, and move keyboard focus into
    // the viewer (the table button that was pressed is now off screen).
    var arrived = function (ok) {
      var report = window.NeuroMosaicReport;
      if (!ok) {
        if (report && report.announce) report.announce('The interactive slices could not be opened; the static figures remain.');
        return;
      }
      var fmt = function (v) { return (v > 0 ? '+' : v < 0 ? '\u2212' : '') + Math.abs(v); };
      var mapName = (art.querySelector('.nm-panel-title') || {}).textContent || '';
      var focusNow = document.activeElement;
      var moved = inputSince(asked) ||
        (focusNow !== askedFocus && focusNow !== document.body && !host.contains(focusNow));
      if (moved) {
        // The reader has moved on: say where the viewer is, leave them be.
        if (report && report.announce) {
          report.announce('Slices ready at ' + (what || 'the peak') + (mapName ? ' \u2014 ' + mapName.trim() : ''));
        }
        return;
      }
      revealSoon(art, id, 'slices', asked);
      if (report && report.announce) {
        report.announce('Slices at ' + (what || 'the peak') + ', ' + xyz.map(fmt).join(', ') + ' mm');
      }
      var visible = function (el) { return el.getClientRects().length > 0 && getComputedStyle(el).visibility !== 'hidden'; };
      var pane = Array.prototype.filter.call(host.querySelectorAll('[tabindex="0"]'), visible)[0] ||
        Array.prototype.filter.call(host.querySelectorAll('[tabindex]:not([tabindex="-1"])'), visible)[0];
      if (pane && typeof pane.focus === 'function') {
        try { pane.focus({ preventScroll: true }); } catch (err) { pane.focus(); }
      }
    };
    if (host.getAttribute('data-nm-volume-ready') === 'true' && place()) {
      arrived(true);
      return;
    }
    var tries = 0;
    var timer = window.setInterval(function () {
      tries++;
      var failed = host.getAttribute('data-nm-volume-failed') === 'true';
      var ok = !failed && host.getAttribute('data-nm-volume-ready') === 'true' && place();
      if (ok || failed || tries > 400) {
        window.clearInterval(timer);
        arrived(ok);
      }
    }, 100);
  };
  // Slices unavailable for this map (e.g. a bundle report opened from
  // file://, where the viewer router disables it): rows stay plain text.
  var slicesOffline = function (id) {
    var router = routerFor(id);
    if (!router) return false;
    var off = (router.getAttribute('data-nm-view-offline') || '').split(/\s+/);
    return off.indexOf('slices') !== -1 || !!router.querySelector('input[value="slices"]:disabled');
  };
  var unbindTable = function (art) {
    art.querySelectorAll('tr.nm-peak-row').forEach(function (tr) {
      var button = tr.querySelector('button.nm-peak-go');
      if (button) {
        var cell = button.parentNode;
        while (button.firstChild) cell.insertBefore(button.firstChild, button);
        button.remove();
      }
      tr.classList.remove('nm-peak-row');
      tr.__nmBound = false;
    });
    art.querySelectorAll('.nm-peak-hint').forEach(function (h) { h.remove(); });
  };
  var bindTables = function () {
    document.querySelectorAll('.nm-map[data-nm-map-article]').forEach(function (art) {
      var id = art.getAttribute('data-nm-map-article');
      if (!art.querySelector('[data-nm-volume-host]')) return;
      if (slicesOffline(id)) { unbindTable(art); return; }
      art.querySelectorAll('tr[data-nm-peak]').forEach(function (tr) {
        if (tr.__nmBound) return;
        tr.__nmBound = true;
        var xyz = tr.getAttribute('data-nm-peak').split(',').map(Number);
        if (xyz.length !== 3 || xyz.some(function (v) { return !isFinite(v); })) return;
        var cell = tr.querySelector('td');
        if (!cell) return;
        var button = document.createElement('button');
        button.type = 'button';
        button.className = 'nm-peak-go';
        while (cell.firstChild) button.appendChild(cell.firstChild);
        var fmt = function (v) { return (v > 0 ? '+' : v < 0 ? '\u2212' : '') + Math.abs(v); };
        button.setAttribute('aria-label', button.textContent.replace(/\s+/g, ' ').trim() +
          ': show in slices at ' + xyz.map(fmt).join(', ') + ' mm');
        cell.appendChild(button);
        tr.classList.add('nm-peak-row');
        var regionCell = tr.querySelector('.nm-col-region');
        var idText = (button.textContent || '').replace(/\s+/g, ' ').trim();
        var idMatch = idText.match(/^[PN]\d+/);
        var what = (idMatch ? idMatch[0] : idText) +
          (regionCell ? ', ' + regionCell.textContent.replace(/\s+/g, ' ').trim() : '');
        button.addEventListener('click', function () { goTo(art, id, xyz, what); });
        if (!tr.__nmListening) {
          tr.__nmListening = true;
          tr.addEventListener('click', function (e) {
            if (!tr.classList.contains('nm-peak-row') || slicesOffline(id)) return;
            if (e.target.closest('button, a') || (window.getSelection && String(window.getSelection()))) return;
            goTo(art, id, xyz, what);
          });
        }
      });
      var cap = art.querySelector('.nm-cluster-table caption');
      if (cap && !cap.querySelector('.nm-peak-hint') && art.querySelector('tr.nm-peak-row')) {
        var hint = document.createElement('span');
        hint.className = 'nm-peak-hint';
        hint.textContent = ' Select a cluster to open it in Slices.';
        cap.appendChild(hint);
      }
    });
  };
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', bindTables);
  else bindTables();
}());

// Figure actions: copy the full caption (or the Methods paragraph), download
// the figure with its title and caption composed on, and dark figure
// variants follow an explicit :root[data-theme] as well as the system
// preference.
(function () {
  var report = function () { return window.NeuroMosaicReport || {}; };
  var copyText = function (button, value, what) {
    var done = function (ok) {
      var old = button.getAttribute('data-nm-label') || button.textContent;
      button.setAttribute('data-nm-label', old);
      button.textContent = ok ? 'Copied' : 'Select the text to copy';
      if (report().announce) report().announce(ok ? what + ' copied to the clipboard' : 'Copy failed; select the text to copy it');
      window.setTimeout(function () { button.textContent = old; }, 1800);
      return ok;
    };
    var fallback = function () {
      var area = document.createElement('textarea');
      area.value = value;
      area.setAttribute('readonly', '');
      area.style.position = 'fixed';
      area.style.opacity = '0';
      document.body.appendChild(area);
      area.select();
      var ok = false;
      try { ok = document.execCommand('copy'); } catch (err) { ok = false; }
      area.remove();
      button.focus();
      return done(ok);
    };
    if (navigator.clipboard && window.isSecureContext) {
      return navigator.clipboard.writeText(value).then(function () { return done(true); }, fallback);
    }
    return Promise.resolve(fallback());
  };
  document.addEventListener('click', function (e) {
    var button = e.target.closest && e.target.closest('[data-nm-copy-caption]');
    if (button) {
      var fig = button.closest('figure');
      var text = fig && fig.querySelector('[data-nm-caption-text]');
      if (!text) return;
      copyText(button, text.textContent.replace(/\s+/g, ' ').trim(), 'Caption').then(function (ok) {
        if (!ok) fig.classList.add('nm-show-caption');
      });
      return;
    }
    var target = e.target.closest && e.target.closest('[data-nm-copy-target]');
    if (target) {
      var block = document.getElementById(target.getAttribute('data-nm-copy-target'));
      if (!block) return;
      var parts = Array.prototype.map.call(block.querySelectorAll('p, li'), function (el) {
        return el.textContent.replace(/\s+/g, ' ').trim();
      });
      copyText(target, parts.join('\n'), 'Methods');
    }
  });

  // "Download PNG": the light figure of record with the map title above and
  // the full caption below, composed in the browser on the figure's own
  // light canvas whatever the page theme. Falls back to the plain file.
  var wrap = function (ctx, text, width) {
    var words = text.split(/\s+/), lines = [], line = '';
    words.forEach(function (w) {
      var next = line ? line + ' ' + w : w;
      if (ctx.measureText(next).width > width && line) { lines.push(line); line = w; } else { line = next; }
    });
    if (line) lines.push(line);
    return lines;
  };
  var compose = function (fig, src) {
    return new Promise(function (resolve, reject) {
      var image = new Image();
      image.onload = function () {
        try {
          var W = image.naturalWidth, s = W / 880;
          var pad = Math.round(24 * s), gap = Math.round(10 * s);
          var css = getComputedStyle(document.documentElement);
          var serif = css.getPropertyValue('--nm-serif') || 'Georgia, serif';
          var sans = css.getPropertyValue('--nm-sans') || 'system-ui, sans-serif';
          var mono = css.getPropertyValue('--nm-mono') || 'monospace';
          var title = fig.getAttribute('data-nm-title') || '';
          var kicker = fig.getAttribute('data-nm-kicker') || '';
          var capEl = fig.querySelector('[data-nm-caption-text]');
          var caption = capEl ? capEl.textContent.replace(/\s+/g, ' ').trim() : '';
          var measure = document.createElement('canvas').getContext('2d');
          var fTitle = '600 ' + Math.round(24 * s) + 'px ' + serif;
          var fKick = '500 ' + Math.round(11 * s) + 'px ' + mono;
          var fCap = '400 ' + Math.round(13 * s) + 'px ' + sans;
          measure.font = fTitle;
          var titleLines = title ? wrap(measure, title, W - 2 * pad) : [];
          measure.font = fCap;
          var capLines = caption ? wrap(measure, caption, W - 2 * pad) : [];
          var lhTitle = Math.round(30 * s), lhKick = Math.round(16 * s), lhCap = Math.round(19 * s);
          var head = pad + (kicker ? lhKick + Math.round(4 * s) : 0) + titleLines.length * lhTitle + gap;
          var foot = capLines.length ? gap + capLines.length * lhCap + pad : pad;
          var canvas = document.createElement('canvas');
          canvas.width = W;
          canvas.height = Math.round(head + image.naturalHeight + foot);
          var ctx = canvas.getContext('2d');
          // The figure's own canvas colour (its corner pixel), else the page's light tone.
          var probe = document.createElement('canvas');
          probe.width = probe.height = 1;
          var pctx = probe.getContext('2d');
          pctx.drawImage(image, 2, 2, 1, 1, 0, 0, 1, 1);
          var px = pctx.getImageData(0, 0, 1, 1).data;
          ctx.fillStyle = px[3] > 200 ? 'rgb(' + px[0] + ',' + px[1] + ',' + px[2] + ')' : '#FBFBF8';
          ctx.fillRect(0, 0, canvas.width, canvas.height);
          var y = pad;
          ctx.textBaseline = 'top';
          if (kicker) {
            ctx.font = fKick; ctx.fillStyle = '#66717A';
            ctx.fillText(kicker.toUpperCase(), pad, y);
            y += lhKick + Math.round(4 * s);
          }
          ctx.font = fTitle; ctx.fillStyle = '#161A1D';
          titleLines.forEach(function (l) { ctx.fillText(l, pad, y); y += lhTitle; });
          y += gap;
          ctx.drawImage(image, 0, Math.round(y));
          y += image.naturalHeight + gap;
          ctx.font = fCap; ctx.fillStyle = '#3A444C';
          capLines.forEach(function (l) { ctx.fillText(l, pad, y); y += lhCap; });
          canvas.toBlob(function (blob) { if (blob) resolve(blob); else reject(new Error('no blob')); }, 'image/png');
        } catch (err) { reject(err); }
      };
      image.onerror = reject;
      image.src = src;
    });
  };
  var save = function (href, name) {
    var a = document.createElement('a');
    a.href = href;
    a.download = name;
    a.style.display = 'none';
    document.body.appendChild(a);
    a.click();
    a.remove();
  };
  document.addEventListener('click', function (e) {
    var link = e.target.closest && e.target.closest('a[data-nm-titled]');
    if (!link || e.button !== 0 || e.metaKey || e.ctrlKey || e.shiftKey || e.altKey) return;
    var fig = link.closest('figure');
    var img = fig && fig.querySelector('picture img, img');
    var src = img && (img.getAttribute('src') || img.getAttribute('data-nm-src'));
    if (!src || !window.HTMLCanvasElement || !HTMLCanvasElement.prototype.toBlob) return;
    // file://: the link was relabelled "Open image" and opens in a new tab.
    if (link.getAttribute('data-nm-offline') === 'true') return;
    e.preventDefault();
    var name = link.getAttribute('download') || 'figure.png';
    // A page opened from file:// cannot read its own images back from a
    // canvas, and browsers ignore `download` there: open the figure itself in
    // a new tab instead of navigating away from the report.
    var plain = function () {
      if (location.protocol === 'file:' && src.indexOf('data:') !== 0) {
        window.open(src, '_blank', 'noopener');
        if (report().announce) report().announce('Figure opened in a new tab');
      } else {
        save(src, name);
      }
    };
    if (location.protocol === 'file:' && src.indexOf('data:') !== 0) { plain(); return; }
    compose(fig, src).then(function (blob) {
      var url = URL.createObjectURL(blob);
      save(url, name);
      window.setTimeout(function () { URL.revokeObjectURL(url); }, 10000);
      if (report().announce) report().announce('Figure downloaded with its title and caption');
    }, plain);
  });
  var root = document.documentElement;
  var theme = function () {
    if (report().isPrinting && report().isPrinting()) return;
    var forced = root.getAttribute('data-theme');
    document.querySelectorAll('source[data-nm-theme]').forEach(function (src) {
      var media = src.getAttribute('data-nm-media') || '';
      var kind = src.getAttribute('data-nm-theme');
      var dark = '(prefers-color-scheme: dark)';
      var next = media;
      if (forced === 'dark' || forced === 'light') {
        if (kind === 'dark') {
          next = forced === 'dark' ? media.replace(dark, '(min-width: 0px)') : 'not all';
        }
      }
      if (src.getAttribute('media') !== next) src.setAttribute('media', next);
    });
  };
  report().applyTheme = theme;
  // A report opened from file:// cannot compose the titled PNG (the canvas
  // may not read local images back) and browsers ignore `download` there, so
  // the link says what it does: it opens the figure in a new tab.
  var offlineLinks = function () {
    if (location.protocol !== 'file:') return;
    document.querySelectorAll('a[data-nm-titled]').forEach(function (a) {
      var img = a.closest('figure') && a.closest('figure').querySelector('picture img, img');
      var src = img && (img.getAttribute('src') || img.getAttribute('data-nm-src'));
      if (src && src.indexOf('data:') === 0) return;
      a.textContent = 'Open image';
      a.setAttribute('data-nm-offline', 'true');
      a.setAttribute('target', '_blank');
      a.setAttribute('rel', 'noopener');
      a.removeAttribute('download');
      a.setAttribute('title', 'Opens the figure file in a new tab (title and caption are added only when the report is served over HTTP)');
      var plainLink = a.parentNode && a.parentNode.querySelector('a.nm-fig-plain');
      if (plainLink) plainLink.hidden = true;
    });
  };
  var startTheme = function () {
    offlineLinks();
    document.querySelectorAll('a[data-nm-download]:not([href])').forEach(function (a) {
      var img = a.closest('figure') && a.closest('figure').querySelector('img');
      if (img) a.href = img.getAttribute('src');
    });
    theme();
    if ('MutationObserver' in window) {
      new MutationObserver(theme).observe(root, { attributes: true, attributeFilter: ['data-theme'] });
    }
  };
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', startTheme);
  else startTheme();
}());

// Print the disclosures open (session details, provenance), then restore.
(function () {
  var opened = [];
  window.addEventListener('beforeprint', function () {
    opened = Array.prototype.filter.call(document.querySelectorAll('details:not([open])'), function (d) {
      d.open = true;
      return true;
    });
  });
  window.addEventListener('afterprint', function () {
    opened.forEach(function (d) { d.open = false; });
    opened = [];
  });
}());

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
