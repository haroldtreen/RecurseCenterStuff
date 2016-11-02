const input = "hello!";

function skips(word) {
    return [].map.call(word, (c, i) => { return skipsN(i + 1, word.slice(i)) });
}

function skipsN(n, word) {
    return word.split('').filter((c, i) => { return (i) % n === 0; }).join('');
}
console.log(skips(input));
