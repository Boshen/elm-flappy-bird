import { Elm } from './Main.elm';

import birdSrc from '../images/bird.png';
import backgroundSrc from '../images/background.png';

Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    birdSrc,
    backgroundSrc
  }
});
