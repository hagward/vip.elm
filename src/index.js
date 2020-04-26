import { Elm } from "./Main.elm";
import "./style.css";

const app = Elm.Main.init({
  node: document.getElementById("elm-root"),
});

const audio = () => document.getElementById("audio");

app.ports.scrollToTrack.subscribe((index) => {
  const track = document.querySelectorAll("#playlist li")[index];
  if (track) {
    track.scrollIntoView();
  }
});
app.ports.seek.subscribe((time) => (audio().currentTime = time));
app.ports.playPause.subscribe((play) =>
  play ? audio().play().then() : audio().pause()
);
