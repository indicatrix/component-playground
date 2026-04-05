import { Elm } from './Index.elm';

const app = Elm.Index.init({
    node: document.querySelector<HTMLDivElement>('#app'),
    flags: window.location.href,
});

app.ports.pushUrl_.subscribe((url: string) => {
    history.replaceState(null, '', url);
});
