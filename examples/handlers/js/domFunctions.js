function domAddStyleClassToRef(nodeRef, className, kappa){
    nodeRef.classList.add(className);
    return _yieldCont(kappa, _$Constants.UNIT);
}

function domRemoveStyleClassFromRef(nodeRef, className, kappa){
    nodeRef.classList.remove(className);
    return _yieldCont(kappa, _$Constants.UNIT);
}
