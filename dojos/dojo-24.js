function isPrime(value) {
    var limit = Math.sqrt(value);
    for(let i = 2; i < limit; i++) {
        if (value % i === 0) {
            return false;
        }
    }
    return true;
}

function hasPrimePermutations(value) {
    const oddNumbers = [1,3,5,7,9].join('|');
    return value.toString().match(new RegExp(oddNumbers, 'g')).length >= 2;
}

function findEquidistants(arr) {
    for (let i = 0; i < arr.length - 2; i++) {
        for (let j = i + 1; j < arr.length - 1; j++) {
            for(let k = j + 1; k < arr.length; k++) {
                if (arr[j] - arr[i] === arr[k] - arr[j]) {
                    return [arr[i], arr[j], arr[k]];
                }
            }
        }
    }
    return null;
}

const primes = [];
const maps = Array.apply(null, { length: 9999 })
    .map((c, i) => i)
    .filter((n) => n >= 1000)
    .filter((i) => isPrime(i))
    .reduce((map, prime) => {
        const sorted = prime.toString().split('').sort().join('');
        map[sorted] = map[sorted] || [];
        map[sorted].push(prime);
        return map;
    }, {});

Object.keys(maps).forEach((key) => {
    if (maps[key].length < 3) {
        delete maps[key];
    }
});

const found = Object.keys(maps).map((k) => maps[k]).reduce((valid, next) => {
    const equidistants = findEquidistants(next);
    if (equidistants) {
        valid.push(equidistants);
    }
    return valid;
}, []);

console.log(found);
