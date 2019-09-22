const STORAGE_KEY = "store";

export default {
  start(app) {
    app.ports.store.subscribe(val => {
      if (val === null) {
        localStorage.removeItem(STORAGE_KEY);
      } else {
        localStorage.setItem(STORAGE_KEY, JSON.stringify(val));
      }

      setTimeout(() => app.ports.onStoreChange.send(val), 0);
    });
  },
  key: STORAGE_KEY
};
