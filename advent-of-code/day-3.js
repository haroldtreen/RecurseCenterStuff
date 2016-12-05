const fs = require('fs');

function isTriangle(tri) {
    return tri.a + tri.b > tri.c &&
            tri.a + tri.c > tri.b &&
              tri.b + tri.c > tri.a;
}

const inputStr = fs.readFileSync(__dirname + '/day3-input.txt').toString();

const validTriangles = inputStr.trim().split('\n').map((str) => {
    return str.trim().split(/\s+/).map(Number);
// }).map((side) => ({ a: side[0], b: side[1], c: side[2] })) // Part 1: Rows -> Triangles
}).reduce((state, side) => { // Part 2: Cols -> Triangles
    state.partial.push(side);
    if (state.partial.length === 3) {
        const doneTriangles = state.partial.reduce((triangles, col, i) => {
            const sideNames = ['a', 'b', 'c'];
            return triangles.map((triangle, j) => {
                triangle[sideNames[i]] = col[j];
                return triangle;
            });
        }, [{}, {}, {}]);
        state.done = state.done.concat(doneTriangles);
        state.partial = [];
    }
    return state;
}, { done: [], partial: [] }).done
.filter((t) => {
    return isTriangle(t);
});

console.log(validTriangles.length);
