const fs = require('fs');

function sortFreqencies(freqs) {
    return Object.keys(freqs).sort((a, b) => {
        if (freqs[b] === freqs[a]) {
            return a.localeCompare(b);
        }
        return freqs[b] - freqs[a];
    });
}

function isRealRoom(room) {
    const letters = room.codename.replace(/-/g, '');
    const letterFreqs = letters.split('').reduce((freqs, letter) => {
        if (!freqs[letter]) {
            freqs[letter] = 0;
        }
        freqs[letter]++

        return freqs;
    }, {});
    room.freqs = letterFreqs;
    room.calculatedChecksum = sortFreqencies(letterFreqs).slice(0, 5).join('');
    return room.checksum === sortFreqencies(letterFreqs).slice(0, 5).join('');
}

function toRoom(str) {
    const parts = str.match(/(.*)-(\d+)\[(.+)\]/);
    return { codename: parts[1], id: parts[2], checksum: parts[3], encrypted: str };
}

function decryptRoom(room) {
    room.name = room.codename.replace(/-/g, ' ').split('').map(c => decrcyptChar(c, room.id)).join('');
    return room;
}

function decrcyptChar(char, id) {
    const numChars = 26;
    const maxChar = 122;
    const minChar = 97;
    let charCode = char.toLowerCase().charCodeAt(0);
    if (charCode > minChar && charCode <= maxChar) {
        charCode += Number(id) % numChars;
        if (charCode > maxChar) {
            charCode = minChar + (charCode % (maxChar + 1));
        }
    }
    return String.fromCharCode(charCode);
}

const inputStr = fs.readFileSync(__dirname + '/day4-input.txt').toString();

const realRooms = inputStr.trim().split('\n')
.map(toRoom)
.filter((room) => {
    return isRealRoom(room);
})

const idSum = realRooms.reduce((sum, room) => sum + Number(room.id), 0);
console.log(idSum);

const northRooms = realRooms.map((room) => {
    return decryptRoom(room);
}).filter(r => /north/.test(r.name));

console.log(northRooms);
