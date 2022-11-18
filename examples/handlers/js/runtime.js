function systemYield(f, kappa){
    window.requestAnimationFrame(function(){
        return f(_$Constants.UNIT, _$K.idy);
    });
    return _$Constants.UNIT;
}

function delayExecution(delay, kappa){
    window.setTimeout(function(){return _$K.yield(kappa, _$Constants.UNIT);}, delay);
    return _$Constants.UNIT;
}

function delayExecutionOfF(delay, f, kappa){
    window.setTimeout(function(){f()}, delay);
    return _$K.yield(kappa, _$Constants.UNIT);
}

function requestAnimationFrame(f, delay, kappa){
    window.requestAnimationFrame(f);
    return _$K.yield(kappa, _$Constants.UNIT);
}

function setIntervalForF(interval, f, kappa){
    const _id = setInterval(function() {
        return f(_$K.idy);
    }, interval);
    return _$K.yield(kappa, _$Constants.UNIT);
}

const SystemQueue = (function(){

    let queue = _$List.nil;

    function enqueue(fiber){
        queue = _$List.cons(fiber, queue);
        return _$Constants.UNIT;
    }

    function dequeue(){
        let temp = queue;
        queue = _$List.nil;
        return temp;
    }

    function length(){
        return _$List.length(queue);
    }

    return { "enqueue": enqueue
           , "dequeue": dequeue
           , "length" : length }

}());

const sysEnqueue = _$Links.kify(SystemQueue.enqueue);
const sysDequeue = _$Links.kify(SystemQueue.dequeue);
const sysQueueLength = _$Links.kify(SystemQueue.length);
