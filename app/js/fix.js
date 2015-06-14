var gui = require('nw.gui');

if (process.platform === "darwin") {
    var mb = new gui.Menu({type: 'menubar'});
    mb.createMacBuiltin('Fast Links', {
        hideEdit: false,
        hideWindow: true
    });
    gui.Window.get().menu = mb;
}
