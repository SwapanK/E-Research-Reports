/* ============================================================================
   FIX: fileInput() Browse button jumping the scroll position.

   Rationale: rather than relying on CSS to stop the browser's internal
   "scroll focused element into view" behaviour (which didn't fully work),
   this actively records the scroll position of the window AND the nearest
   real scrollable ancestor the instant the Browse button is pressed, then
   forces both back to that position on every animation frame for a couple
   of seconds afterwards — covering both the moment the native file dialog
   opens (focus jump) and the moment it closes (focus returns).

   Safe no-op everywhere else in the app; only activates on ".btn-file"
   (the wrapper Shiny generates around every fileInput's Browse button).
   ============================================================================ */
(function () {

  function findScrollAncestor(el) {
    var node = el ? el.parentElement : null;
    while (node && node !== document.body) {
      var style = window.getComputedStyle(node);
      var canScroll = node.scrollHeight > node.clientHeight &&
                       (style.overflowY === "auto" || style.overflowY === "scroll");
      if (canScroll) return node;
      node = node.parentElement;
    }
    return document.scrollingElement || document.documentElement;
  }

  function lockScroll(btn) {
    var panel  = findScrollAncestor(btn);
    var winY   = window.scrollY;
    var panelY = panel ? panel.scrollTop : 0;
    var stopAt = Date.now() + 2000; // keep correcting for 2s (covers dialog open + close)

    function tick() {
      if (window.scrollY !== winY) window.scrollTo(0, winY);
      if (panel && panel.scrollTop !== panelY) panel.scrollTop = panelY;
      if (Date.now() < stopAt) requestAnimationFrame(tick);
    }
    requestAnimationFrame(tick);
  }

  document.addEventListener("mousedown", function (e) {
    var btn = e.target.closest && e.target.closest(".btn-file");
    if (btn) lockScroll(btn);
  }, true);

  // Also catch keyboard activation (Enter/Space while the button has focus)
  document.addEventListener("keydown", function (e) {
    if (e.key !== "Enter" && e.key !== " ") return;
    var btn = e.target.closest && e.target.closest(".btn-file");
    if (btn) lockScroll(btn);
  }, true);

})();











/* ============================================================================
   ADD-ON: append this to the END of www/scroll_lock_fix.js
   (keeps the existing fileInput scroll-jump fix from earlier — this is a
   second, independent IIFE, doesn't touch or depend on it)

   Syncs each Stage-1 colour field's actual hex value to a --swatch-color
   CSS variable on its wrapper, so the coloured stripe in
   .sec2-swatch-field always matches what's actually picked, instead of
   relying on colourpicker's own internal text-colour styling.
   ============================================================================ */
(function () {

  function isValidHex(v) {
    return typeof v === "string" && /^#([0-9a-f]{3}|[0-9a-f]{6})$/i.test(v.trim());
  }

  function syncField(input) {
    var wrap = input.closest(".sec2-swatch-field");
    if (!wrap) return;
    var val = input.value;
    if (isValidHex(val)) {
      wrap.style.setProperty("--swatch-color", val.trim());
    }
  }

  function wireField(input) {
    if (input.dataset.swatchWired) return;
    input.dataset.swatchWired = "1";
    syncField(input);
    ["change", "input", "blur"].forEach(function (evt) {
      input.addEventListener(evt, function () { syncField(input); });
    });
  }

  function scanAll() {
    document
      .querySelectorAll(".sec2-swatch-field input.shiny-colour-input")
      .forEach(wireField);
  }

  // Initial pass once the page/app has settled
  document.addEventListener("DOMContentLoaded", scanAll);
  if (window.Shiny) {
    document.addEventListener("shiny:sessioninitialized", scanAll);
    document.addEventListener("shiny:idle", scanAll);
  }

  // Colour fields inside a conditionalPanel can appear after initial load
  var mo = new MutationObserver(function () { scanAll(); });
  mo.observe(document.body, { childList: true, subtree: true });

})();
























Shiny.addCustomMessageHandler("show-toast", function(msg) {

  const cfg = {

    text     : msg.text || "",
    type     : msg.type || "info",
    icon     : msg.icon || "fa-info-circle",
    duration : msg.duration || 2200
  };

  let container =
    document.querySelector(
      ".toast-container.bottom-right"
    );

  if (!container) {

    container =
      document.createElement("div");

    container.className =
      "toast-container bottom-right";

    document.body.appendChild(
      container
    );
  }

  container.innerHTML = "";

  const toast =
    document.createElement("div");

  toast.className =
    `modern-toast ${cfg.type}`;

  toast.innerHTML = `
      <i class="fa ${cfg.icon}"></i>
      <span>${cfg.text}</span>
  `;

  container.appendChild(toast);

  setTimeout(() => {

      toast.classList.add("hide");

      setTimeout(
          () => toast.remove(),
          250
      );

  }, cfg.duration);

});








$(document).on('click', '.dropdown-trigger', function(e) {
  e.stopPropagation();
  $(this).closest('.user-dropdown').toggleClass('open');
});

$(document).on('click', function() {
  $('.user-dropdown.open').removeClass('open');
});

$(document).on('click', '.user-dropdown-menu', function(e) {
  e.stopPropagation();
});









Shiny.addCustomMessageHandler("show-goodbye", function(msg) {

  const overlay = document.createElement('div');
  overlay.className = 'goodbye-overlay';

  overlay.innerHTML = `
    <div class="goodbye-stage">
      <div class="goodbye-word">Goodbye</div>
      <div class="goodbye-username">${msg.username}</div>
      <div class="goodbye-sub">You may now close this tab</div>
    </div>
  `;

  document.body.appendChild(overlay);

  requestAnimationFrame(function() {
    overlay.classList.add('show');
  });

  // Hold on screen longer, then fade fully to black
  setTimeout(function() {
    overlay.classList.add('fade-out');
  }, 3800);

  setTimeout(function() {
    try { window.close(); } catch (e) { /* browser blocked it — that's fine */ }
  }, 4400);
});

















