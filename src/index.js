import { Elm } from './Main.elm';

import birdSrc from '../images/bird.png';

Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    birdSrc
  }
});
