"use strict";

(() => {
  const preview = document.getElementById("markdown-body");
  const socket = new WebSocket(preview.dataset.websocketUrl);

  socket.addEventListener("open", () => {
    socket.send(`MDPM-Register-UUID: ${preview.dataset.previewUuid}`);
  });

  socket.addEventListener("message", (event) => {
    const documentFragment = new DOMParser().parseFromString(event.data, "text/html");
    const content = documentFragment.getElementById("content");
    const position = documentFragment.getElementById("position-percentage");
    if (!content || !position) {
      throw new Error("Malformed Markdown preview update");
    }

    preview.replaceChildren(...Array.from(content.childNodes, (node) => node.cloneNode(true)));
    const percentage = Number(position.textContent);
    if (!Number.isFinite(percentage)) {
      throw new Error("Invalid Markdown preview scroll position");
    }
    const scrollTop = document.documentElement.scrollHeight * percentage / 100;
    window.scrollTo({top: scrollTop, behavior: "smooth"});
  });

  socket.addEventListener("error", () => {
    console.error("Markdown preview WebSocket failed");
  });
})();
