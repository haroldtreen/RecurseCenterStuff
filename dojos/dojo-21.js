function processData(input) {
    const lines = input.split('\n');
    const cases = [];

    for (let i = 2; i < lines.length; i += 2) {
        cases.push(lines[i].split(' ').map((v) => Number(v)));
    }

    main(cases);
}

function findEqual(nums) {
    if(nums.length === 1) { return 0; }

    let leftSum = 0;
    let rightSum = nums.reduce((a, b) => a + b, 0) - nums[0];

    if (leftSum === rightSum) {
        return 0;
    }

    for (let i = 1; i < nums.length; i++) {
        leftSum += nums[i-1];
        rightSum = (rightSum - nums[i]);
        if (leftSum === rightSum) {
            return i;
        }
    }
    return -1;
}

function main(cases) {
    cases.forEach((c) => {
        const equalIndex = findEqual(c);
        if (equalIndex >= 0) {
            console.log('YES');
        } else {
            console.log('NO');
        }
    });
}

process.stdin.resume();
process.stdin.setEncoding("ascii");
_input = "";
process.stdin.on("data", function (input) {
    _input += input;
});

process.stdin.on("end", function () {
   processData(_input);
});
