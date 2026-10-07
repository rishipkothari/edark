/* ==========================================================================
   EDARK - the Export page's checklist (R/module_export.R, PRD/BUILD_Export.md)

   The checklist is rendered by the server; everything a tick does happens
   here, so ticking never round-trips or re-renders:
     - a section's box ticks / clears every available item in it, and shows
       all / some / none (the indeterminate state) from its items;
     - the ticked item ids go to input$<checklist data-input>, so a server
       re-render can restore them;
     - an item's options on its row (.edark-export-opt: data / report format,
       the session's original data) are ordinary Shiny inputs bound by id;
       here they are only disabled while their item is unticked.
   Select all / Clear sit in the config pane, outside the checklist.
   Plus one message handler: start the download once a build is ready; and
   the R Code pill's Copy button ([data-copy-target]).
   ========================================================================== */

(function () {
  "use strict";

  function leaves(scope) {
    return Array.prototype.slice.call(
      scope.querySelectorAll("input.edark-export-box:not([disabled])"));
  }

  // Section boxes from their items
  function syncSections(tree) {
    tree.querySelectorAll("[data-folder]").forEach(function (s) {
      var box = s.querySelector(".edark-export-section-head input.edark-export-folder-box");
      if (!box) return;
      var boxes = leaves(s);
      var on = boxes.filter(function (b) { return b.checked; }).length;
      box.disabled = boxes.length === 0;
      box.checked = boxes.length > 0 && on === boxes.length;
      box.indeterminate = on > 0 && on < boxes.length;
    });
  }

  // An item's options only apply when the item is ticked
  function syncOptions(tree) {
    tree.querySelectorAll(".edark-export-opt").forEach(function (o) {
      var box = o.closest(".edark-export-row").querySelector("input.edark-export-box");
      var off = !box || !box.checked;
      o.disabled = off;
      var lab = o.closest(".edark-export-opt-check");
      if (lab) lab.classList.toggle("is-disabled", off);
    });
  }

  function report(tree) {
    if (!window.Shiny || !Shiny.setInputValue) return;
    var ids = leaves(tree).filter(function (b) { return b.checked; })
                          .map(function (b) { return b.getAttribute("data-id"); });
    Shiny.setInputValue(tree.getAttribute("data-input"), ids);
  }

  function sync(tree) {
    syncSections(tree);
    syncOptions(tree);
    report(tree);
  }

  document.addEventListener("change", function (e) {
    var t = e.target;
    var tree = t.closest && t.closest(".edark-export-tree");
    if (!tree) return;
    if (t.classList.contains("edark-export-folder-box")) {
      leaves(t.closest("[data-folder]")).forEach(function (b) { b.checked = t.checked; });
    }
    if (t.classList.contains("edark-export-folder-box") || t.classList.contains("edark-export-box")) {
      sync(tree);
    }
  });

  // Select all / Clear, in the config pane
  document.addEventListener("click", function (e) {
    var link = e.target.closest && e.target.closest("[data-export-select]");
    if (!link) return;
    e.preventDefault();
    var tree = document.querySelector(".edark-export-tree");
    if (!tree) return;
    var all = link.getAttribute("data-export-select") === "all";
    leaves(tree).forEach(function (b) { b.checked = all; });
    sync(tree);
  });

  // After the server renders a checklist: set section states and report the
  // selection it was rendered with
  $(document).on("shiny:value", function () {
    setTimeout(function () {
      document.querySelectorAll(".edark-export-tree:not([data-ready])").forEach(function (tree) {
        tree.setAttribute("data-ready", "1");
        sync(tree);
      });
    }, 0);
  });

  // Build & Download: the server enables the download link, then asks for it
  // to be clicked
  function onDownload(msg) {
    var a = document.getElementById(msg.id);
    if (!a) return;
    a.classList.remove("edark-export-no-build");
    a.removeAttribute("disabled");
    a.removeAttribute("aria-disabled");
    a.removeAttribute("tabindex");
    a.click();
  }

  // R Code pill, Copy: the text of the element the button names, with a short
  // "Copied" on the button. navigator.clipboard needs a secure context
  // (localhost is one); otherwise fall back to a selected textarea.
  document.addEventListener("click", function (e) {
    var btn = e.target.closest("[data-copy-target]");
    if (!btn) return;
    var el = document.getElementById(btn.getAttribute("data-copy-target"));
    if (!el) return;
    var text = el.innerText;
    var done = function () {
      var old = btn.innerHTML;
      btn.textContent = "Copied";
      setTimeout(function () { btn.innerHTML = old; }, 1500);
    };
    if (navigator.clipboard && window.isSecureContext) {
      navigator.clipboard.writeText(text).then(done);
    } else {
      var ta = document.createElement("textarea");
      ta.value = text;
      ta.style.position = "fixed";
      ta.style.opacity = "0";
      document.body.appendChild(ta);
      ta.select();
      try { document.execCommand("copy"); done(); } finally { document.body.removeChild(ta); }
    }
  });

  if (window.Shiny && Shiny.addCustomMessageHandler) {
    Shiny.addCustomMessageHandler("edark_export_download", onDownload);
  } else {
    $(document).on("shiny:connected", function () {
      Shiny.addCustomMessageHandler("edark_export_download", onDownload);
    });
  }
})();
