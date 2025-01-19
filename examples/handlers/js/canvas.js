function drawUnit(x, y, color, ctx, kappa) {
    ctx.fillStyle= color;
    ctx.fillRect(x,y,1,1);
    return _$K.yield(kappa, _$Constants.UNIT);
}

function drawCustomUnit(x, y, xheight, yheight, color, ctx, kappa) {
    ctx.fillStyle= color;
    ctx.fillRect(x, y, xheight, yheight);
    return _$K.yield(kappa, _$Constants.UNIT);

}

function getValueFromSelection(id, kappa){
    const e = document.getElementById(id);
    return _$K.yield(kappa, e.value);
}

function setBorderOfRef(node, style, kappa){
    node.style.border = style;
    return _$K.yield(kappa, node);
}
