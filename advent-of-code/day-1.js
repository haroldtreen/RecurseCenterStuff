let currentDir = 0;
let totalSteps = { N: 0, E: 0, S: 0, W: 0, };
let pastDeltas = {};

const turn = (dir) => {
    if (dir === 'L') {
        currentDir -= 1;
        currentDir = currentDir < 0 ? 3 : currentDir;
    } else if (dir === 'R') {
        currentDir += 1;
        currentDir = currentDir > 3 ? 0 : currentDir;
    }
};

const delta = () => {
    return `${totalSteps.N - totalSteps.S}N ${totalSteps.E - totalSteps.W}E`;
};

const walk = (steps) => {
    const direction = (['N', 'E', 'S', 'W'])[currentDir];
    const deltas = Array.apply(null, { length: steps }).map(() => {
        totalSteps[direction] += 1;
        return delta();
    });

    return deltas;
};

const inputsStr = 'R2, L3, R2, R4, L2, L1, R2, R4, R1, L4, L5, R5, R5, R2, R2, R1, L2, L3, L2, L1, R3, L5, R187, R1, R4, L1, R5, L3, L4, R50, L4, R2, R70, L3, L2, R4, R3, R194, L3, L4, L4, L3, L4, R4, R5, L1, L5, L4, R1, L2, R4, L5, L3, R4, L5, L5, R5, R3, R5, L2, L4, R4, L1, R3, R1, L1, L2, R2, R2, L3, R3, R2, R5, R2, R5, L3, R2, L5, R1, R2, R2, L4, L5, L1, L4, R4, R3, R1, R2, L1, L2, R4, R5, L2, R3, L4, L5, L5, L4, R4, L2, R1, R1, L2, L3, L2, R2, L4, R3, R2, L1, L3, L2, L4, L4, R2, L3, L3, R2, L4, L3, R4, R3, L2, L1, L4, R4, R2, L4, L4, L5, L1, R2, L5, L2, L3, R2, L2';
const inputs = inputsStr.split(', ').map((move) => {
    const matches = move.match(/(\w)(\d+)/);
    return { dir: matches[1], steps: Number(matches[2]) };
});

inputs.forEach((input) => {
    turn(input.dir);
    const deltas = walk(input.steps);
    deltas.forEach((d) => {
        if (pastDeltas[d]) {
            console.log('Visited twice: ' + d);
        } else {
            pastDeltas[d] = true;
        }
    });
});

console.log('Final Destinateion: ' + delta());
