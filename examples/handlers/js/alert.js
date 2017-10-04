function _alertBox(msg) {
    return alert(msg);
}

function wait(time, cb, kappa) {
    setTimeout(function() {
        return _applyCont(_makeCont(cb), _idy);
    }, time);
    return _applyCont(kappa, {});
}

var alertBox = LINKS.kify(_alertBox);
