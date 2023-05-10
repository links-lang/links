function _alertBox(msg) {
    return alert(msg);
}

function wait(time, cb, kappa) {
    setTimeout(function() {
        return _$K.apply(_$K.make(cb), _$K.idy);
    }, time);
    return _$K.apply(kappa, _$Constants.UNIT);
}

var alertBox = _$Links.kify(_alertBox);
