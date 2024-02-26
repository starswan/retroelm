// Run this example by adding <%= javascript_pack_tag "hello_elm" %> to the
// head of your layout file, like app/views/layouts/application.html.erb.
// It will render "Hello Elm!" within the page.

import {Elm} from 'Main.elm'

document.addEventListener('DOMContentLoaded', () => {
  const target = document.getElementById('applet');
  const load = target.dataset.load;
  const rom = target.dataset.rom;
  const flags = 'rom=' + rom + ' tape=' + load;
  // const flags = 'rom=' + rom;
  Elm.Main.init({
    node: target,
    flags: flags
  })
});
