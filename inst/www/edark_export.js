/* ==========================================================================
   EDARK - the Export page's zip tree (R/module_export.R, PRD/BUILD_Export.md)

   The tree is rendered by the server; everything a tick does happens here, so
   ticking never round-trips or re-renders:
     - a folder's box ticks / clears every available file under it, and shows
       all / some / none (the indeterminate state) from its files;
     - each folder shows "ticked of available";
     - the ticked file ids go to input$<tree data-input>, the open folders to
       input$<tree data-open-input>, so a server re-render can restore both.
   Plus one message handler: start the download once a build is ready.
   ========================================================================== */

(function () {
  "use strict";

  function leaves(scope) {
    return Array.prototype.slice.call(
      scope.querySelectorAll("input.edark-export-box:not([disabled])"));
  }

  // Folder boxes and counts from their files, deepest folders first
  function syncFolders(tree) {
    var folders = Array.prototype.slice.call(tree.querySelectorAll("details[data-folder]")).reverse();
    folders.forEach(function (d) {
      var boxes = leaves(d);
      var on = boxes.filter(function (b) { return b.checked; }).length;
      var box = d.querySelector(":scope > summary > input.edark-export-folder-box");
      var count = d.querySelector(":scope > summary > .edark-export-count");
      if (box) {
        box.disabled = boxes.length === 0;
        box.checked = boxes.length > 0 && on === boxes.length;
        box.indeterminate = on > 0 && on < boxes.length;
      }
      if (count) count.textContent = boxes.length === 0 ? "" : on + " of " + boxes.length;
    });
  }

  function report(tree) {
    if (!window.Shiny || !Shiny.setInputValue) return;
    var ids = leaves(tree).filter(function (b) { return b.checked; })
                          .map(function (b) { return b.getAttribute("data-id"); });
    Shiny.setInputValue(tree.getAttribute("data-input"), ids);
  }

  function reportOpen(tree) {
    if (!window.Shiny || !Shiny.setInputValue) return;
    var open = Array.prototype.slice.call(tree.querySelectorAll("details[data-folder][open]"))
                    .map(function (d) { return d.getAttribute("data-folder"); });
    Shiny.setInputValue(tree.getAttribute("data-open-input"), open);
  }

  function sync(tree) {
    syncFolders(tree);
    report(tree);
  }

  document.addEventListener("change", function (e) {
    var t = e.target;
    var tree = t.closest && t.closest(".edark-export-tree");
    if (!tree) return;
    if (t.classList.contains("edark-export-folder-box")) {
      var d = t.closest("details");
      leaves(d).forEach(function (b) { b.checked = t.checked; });
    }
    if (t.classList.contains("edark-export-folder-box") || t.classList.contains("edark-export-box")) {
      sync(tree);
    }
  });

  // A checkbox inside <summary> must not also open / close its folder. Browsers
  // differ on whether it does, so whatever happens, put the folder back.
  document.addEventListener("click", function (e) {
    var t = e.target;
    if (t.classList && t.classList.contains("edark-export-folder-box")) {
      var folder = t.closest("details");
      var was = folder.open;
      setTimeout(function () { if (folder.open !== was) folder.open = was; }, 0);
      return;
    }

    var link = t.closest && t.closest("[data-export-select], [data-export-expand]");
    if (!link) return;
    var tree = link.closest(".edark-export-tree");
    if (!tree) return;
    e.preventDefault();
    if (link.hasAttribute("data-export-select")) {
      var all = link.getAttribute("data-export-select") === "all";
      leaves(tree).forEach(function (b) { b.checked = all; });
      sync(tree);
    } else {
      var expand = link.getAttribute("data-export-expand") === "all";
      tree.querySelectorAll("details[data-folder]").forEach(function (d) { d.open = expand; });
      reportOpen(tree);
    }
  }, true);

  // <details> toggle does not bubble: listen in the capture phase
  document.addEventListener("toggle", function (e) {
    var t = e.target;
    if (!t.matches || !t.matches("details[data-folder]")) return;
    var tree = t.closest(".edark-export-tree");
    if (tree) reportOpen(tree);
  }, true);

  // After the server renders a tree: set folder states and report the
  // selection the tree was rendered with
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

  if (window.Shiny && Shiny.addCustomMessageHandler) {
    Shiny.addCustomMessageHandler("edark_export_download", onDownload);
  } else {
    $(document).on("shiny:connected", function () {
      Shiny.addCustomMessageHandler("edark_export_download", onDownload);
    });
  }
})();
