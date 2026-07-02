import { Elm } from './Index.elm';
import './cp-ai-selection.js';

const app = Elm.Index.init({
    node: document.querySelector<HTMLDivElement>('#app'),
    flags: window.location.href,
});

app.ports.pushUrl_.subscribe((url: string) => {
    history.replaceState(null, '', url);
});
