const streamerName = window.location.pathname.split('/').at(2);

document.title = `${streamerName}'s Wands`;

const ws = new WebSocket(`wss://${location.host}/updates/${streamerName}`);

app = Elm.StreamerPage.init({
    node: document.getElementById('elm'),
    flags: {streamerName: streamerName, spellData: spellData, wandSprites: wandSprites}
});

ws.onmessage = event => {
    app.ports.streamerInfoUpdates.send(event.data);
};
