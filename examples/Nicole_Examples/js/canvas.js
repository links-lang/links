
function drawUnit(x, y, color, ctx, kappa) {
    ctx.fillStyle= color;
    ctx.fillRect(x,y,1,1);
    return _yieldCont(kappa, CONSTANTS.UNIT);
}

function drawCustomUnit(x, y, xheight, yheight, color, ctx, kappa) {
    ctx.fillStyle= color;
    ctx.fillRect(x, y, xheight, yheight);
    return _yieldCont(kappa, CONSTANTS.UNIT);

}

function getColorFromSelection(id, kappa){
    var e = document.getElementById(id);
    return _yieldCont(kappa, e.value);

}

