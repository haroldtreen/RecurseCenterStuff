/*
You are given an encoded string of the form:
<encoded_string> = "<characters a-Z><numbers 0-9>[<encoded_string>]<encoded_string>"

The string contains numbers followed by [ ] brackets containing other strings.
To decode the string, the <number>[<encoded_string>] sections must be replaced by the bracket contents repeated <number> times.

eg.

"ab2[c4[d]]e" -> "abcddddcdddde"
*/

function isNumber(num) {
    return !isNaN(parseInt(num));
}

function parseNumber(str, start) {
    for (let i = start; i < str.length; i++) {
        if (!isNumber(str[i])) {
            return str.substring(start, i);
        }
    }
}

function decodeString(str) {
    let decoded = '';
    for (let i = 0; i < str.length; i++) {
        const char = str[i];
        if (isNumber(char)) {
            const reps = parseNumber(str, i);
            const subDecoded = decodeString(str.substring(i + 1 + reps.length));
            decoded += subDecoded.repeat(parseInt(reps));
            i += 1 + reps.length + subDecoded.length;
        } else if (char === ']'){
            return decoded;
        } else {
            decoded += char;
        }
    }
    return decoded;
}

// Tests
[
    'ab2[c4[d]]e',
    'a4[b]',
    '4[ba]d',
    'Look its 2[6[Nana]Batman]!',
    '12[13[b]]',
].forEach((t) => {
    console.log(`"${t}"`);
    console.log('decodes to:');
    console.log(`"${decodeString(t)}"`);
    console.log('-----');
});
