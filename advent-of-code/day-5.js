const md5 = require('blueimp-md5');

const ID = 'abbhdwsy';
let index = 1739525;
let done = 0;
let password = [];

while (done < 8) {
    const test = ID + index;
    const hash = md5(test);
    const passwordChar = hash.match(/^0{5}(.)(.)/);

    if (passwordChar) {
        pos = Number(passwordChar[1]);
        char = passwordChar[2];
        if (!password[pos] && pos < 8) {
            password[pos] = char;
            done++;
        }
        console.log(passwordChar);
        console.log(password.join(''), '-', index);
    }
    index++;
}

// console.log(md5('abc3231929'));
