
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

function getValueFromSelection(id, kappa){
    var e = document.getElementById(id);
    return _yieldCont(kappa, e.value);
}

function setBorderOfRef(node, style, kappa){
    node.style.border = style;
    return _yieldCont(kappa, node);
}

CONSTANTS.UNIT