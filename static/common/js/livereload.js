new WebSocket(`ws://${window.location.hostname}:${window.location.port}`)
    .addEventListener("close", () => setTimeout(() => window.location.reload(), 4000));