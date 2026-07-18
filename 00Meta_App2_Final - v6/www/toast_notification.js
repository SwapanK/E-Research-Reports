/* ============================================================================
   TOAST NOTIFICATIONS + scroll fix for fileInput
   ============================================================================ */

// ---- Toast notifications ----
Shiny.addCustomMessageHandler("show-toast", function(msg) {
  const cfg = {
    text     : msg.text || "",
    type     : msg.type || "info",
    icon     : msg.icon || "fa-info-circle",
    duration : msg.duration || 2200
  };

  let container = document.querySelector(".toast-container.bottom-right");
  if (!container) {
    container = document.createElement("div");
    container.className = "toast-container bottom-right";
    document.body.appendChild(container);
  }

  container.innerHTML = "";
  const toast = document.createElement("div");
  toast.className = `modern-toast ${cfg.type}`;
  toast.innerHTML = `<i class="fa ${cfg.icon}"></i><span>${cfg.text}</span>`;
  container.appendChild(toast);

  setTimeout(() => {
    toast.classList.add("hide");
    setTimeout(() => toast.remove(), 250);
  }, cfg.duration);
});

// ---- FIX: fileInput Browse button jumping the scroll position ----
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
    var stopAt = Date.now() + 2000;

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

  document.addEventListener("keydown", function (e) {
    if (e.key !== "Enter" && e.key !== " ") return;
    var btn = e.target.closest && e.target.closest(".btn-file");
    if (btn) lockScroll(btn);
  }, true);
})();