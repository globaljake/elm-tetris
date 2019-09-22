import { Elm } from "./Main.elm";
import Storage from "./Storage";
import "./Main.css";

const flags = { storage: localStorage.getItem(Storage.key) };
const app = Elm.Main.init({ flags });

Storage.start(app);
