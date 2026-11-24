(() => {
  const isStandalone = window.matchMedia("(display-mode: standalone)").matches ||
    window.navigator.standalone === true;
  const nav = document.querySelector(".navbar .navbar-nav.ms-auto") ||
    document.querySelector(".navbar .navbar-nav:last-child");
  if (!nav || isStandalone) return;

  const item = document.createElement("li");
  item.className = "nav-item literature-radar-install";
  const button = document.createElement("button");
  button.type = "button";
  button.className = "btn btn-primary btn-sm";
  button.textContent = "Install";
  button.hidden = true;
  item.append(button);
  nav.prepend(item);

  let installPrompt = null;
  window.addEventListener("beforeinstallprompt", event => {
    event.preventDefault();
    if (isStandalone) return;
    installPrompt = event;
    button.hidden = false;
  });

  button.addEventListener("click", async () => {
    if (!installPrompt) return;
    const prompt = installPrompt;
    installPrompt = null;
    button.hidden = true;
    await prompt.prompt();
    const choice = await prompt.userChoice;
    if (choice.outcome !== "accepted" && !isStandalone) button.hidden = false;
  });

  window.addEventListener("appinstalled", () => {
    installPrompt = null;
    button.hidden = true;
  });

  if ("serviceWorker" in navigator) {
    window.addEventListener("load", () => {
      navigator.serviceWorker.register("/literature-radar/service-worker.js", {
        scope: "/literature-radar/"
      }).catch(error => console.warn("Offline support could not be enabled:", error));
    });
  }
})();
