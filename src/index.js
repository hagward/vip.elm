import './style.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

const app = Elm.Main.init({
  node: document.getElementById('elm-root')
});

const audio = document.getElementById('audio');

app.ports.scrollToTrack.subscribe(() => {
  const selected = document.getElementsByClassName('selected')[0];
  if (selected) {
    selected.scrollIntoView();
    window.scrollBy(0, -20);
  }
});
app.ports.seek.subscribe(time => audio.currentTime = time);
app.ports.playPause.subscribe(play => play ? audio.play().then() : audio.pause());

registerServiceWorker();
