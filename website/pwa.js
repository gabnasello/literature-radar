(() => {
  const button = document.getElementById("install-app-button");
  const dialog = document.getElementById("install-help");
  const helpText = document.getElementById("install-help-text");
  const close = document.getElementById("install-help-close");
  if (!button) return;

  let installPrompt = null;
  const isIOS = /iphone|ipad|ipod/i.test(navigator.userAgent) ||
    (navigator.platform === "MacIntel" && navigator.maxTouchPoints > 1);

  if ("serviceWorker" in navigator) {
    window.addEventListener("load", () => {
      navigator.serviceWorker.register("/literature-radar/service-worker.js", {
        scope: "/literature-radar/"
      }).catch((error) => console.warn("App offline support could not be enabled:", error));
    });
  }

  window.addEventListener("beforeinstallprompt", (event) => {
    event.preventDefault();
    installPrompt = event;
    button.hidden = false;
  });

  window.addEventListener("appinstalled", () => {
    installPrompt = null;
    button.hidden = true;
  });

  button.addEventListener("click", async () => {
    if (installPrompt) {
      installPrompt.prompt();
      await installPrompt.userChoice;
      installPrompt = null;
      return;
    }

    if (isIOS) {
      helpText.textContent = "In Safari, tap Share, then choose “Add to Home Screen”. Open the saved icon to use Literature Radar like an app.";
    } else {
      helpText.textContent = "In Chrome or Edge, open the browser menu and choose “Install Literature Radar” or “Install page as app”. You can then launch it from your desktop or app launcher.";
    }
    if (dialog && typeof dialog.showModal === "function") dialog.showModal();
  });

  if (close) close.addEventListener("click", () => dialog.close());
  button.hidden = false;
})();
