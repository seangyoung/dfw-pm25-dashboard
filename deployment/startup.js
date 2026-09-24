(() => {
  const startup = document.getElementById("dfw-startup");
  const status = document.getElementById("dfw-startup-status");
  if (!startup || !status) return;

  let checks = 0;
  const appIsReady = () => {
    const frame = document.querySelector("#root iframe, iframe");
    if (!frame) return false;
    try {
      const doc = frame.contentDocument;
      return Boolean(
        doc && doc.body &&
        doc.body.textContent.includes("DFW PM2.5 Monitor Explorer") &&
        doc.querySelector('[role="tablist"]')
      );
    } catch (_error) {
      return false;
    }
  };

  const timer = window.setInterval(() => {
    checks += 1;
    if (appIsReady()) {
      window.clearInterval(timer);
      startup.classList.add("dfw-startup-hidden");
      window.setTimeout(() => startup.remove(), 300);
      return;
    }
    if (checks === 90) {
      status.textContent =
        "Still loading. If you are using Safari, please open this dashboard in Chrome or Edge.";
    }
  }, 1000);
})();
