const fs = require('fs')
class KeyPad {
    constructor() {
        // Part 1
        // this.pad = [
        //     [1, 2, 3],
        //     [4, 5, 6],
        //     [7, 8, 9],
        // ];
        // this.currentH = 1;
        // this.currentV = 1;

        this.pad = [
            [0, 0, 1, 0, 0],
            [0, 2, 3, 4, 0],
            [5, 6, 7, 8, 9],
            [0, 'A', 'B', 'C', 0],
            [0, 0, 'D', 0, 0],
        ];
        this.currentH = 0;
        this.currentV = 2;

        this.code = [];
    }

    L() {
        this.currentH -= this.validKey(this.currentV, this.currentH - 1) ? 1 : 0;
    }

    R() {
        this.currentH += this.validKey(this.currentV, this.currentH + 1) ? 1 : 0;
    }

    U() {
        this.currentV -= this.validKey(this.currentV - 1, this.currentH) ? 1 : 0;
    }

    D() {
        this.currentV += this.validKey(this.currentV + 1, this.currentH) ? 1 : 0;
    }

    validKey(v, h) {
        return this.pad[v] && this.pad[v][h];
    }

    current() {
        return this.pad[this.currentV][this.currentH];
    }

    remember() {
        this.code.push(this.current());
    }
}

const kp = new KeyPad();
const inputStr = fs.readFileSync(__dirname + '/day2-input.txt').toString();
const input = inputStr.trim().split('\n').forEach((line) => {
    line.trim().split('').forEach((dir) => {
        kp[dir]();
    });
    kp.remember();
});

console.log(kp.code.join(''));
