function decode(code) {
    const base = [1];
    for (var i = 1; i < code.length; i++) {
        // case 1 -> New number can be combined with previous (eg. '1' + '2')
        // case 2 -> New number can't be combined with previous (eg '6' + '4')
        const caseNum = base[i-1] + options(code[i] + code[i-1]);
        base.push(caseNum);
    }
    return base[code.length - 1];
}

function options(num) {
    if(num.length > 1) {
        return 1;
    } else if(Number(num[0]) < 3) {
        return 2;
    } else {
        return 3;
    }
}

const result = decode('1221');

console.log(result);
