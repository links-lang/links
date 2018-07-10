function systemYield(f, kappa){
    window.requestAnimationFrame(function(){
        return f(CONSTANTS.UNIT, _idy);
    });
    return CONSTANTS.UNIT;
}

function delayExecution(delay, kappa){
    window.setTimeout(function(){return _yieldCont(kappa, CONSTANTS.UNIT);}, delay);
    return CONSTANTS.UNIT;
}

const SystemQueue = (function(){

    let queue = LINKEDLIST.Nil;

    function enqueue(fiber){
        queue = LINKEDLIST.Cons(fiber, queue);
        return CONSTANTS.UNIT;
    }
    
    function dequeue(){
        let temp = queue; 
        queue = LINKEDLIST.Nil;
        return temp;
    }

    function length(){
        return LINKEDLIST.length(queue);
    }

    return {"enqueue": enqueue, "dequeue": dequeue, "length" : length}

}());

const sysEnqueue = LINKS.kify(SystemQueue.enqueue);
const sysDequeue = LINKS.kify(SystemQueue.dequeue);
const sysQueueLength = LINKS.kify(SystemQueue.length);
