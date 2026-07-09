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

















