function waysToEat(numFries) {
    if (numFries === 1) {
        return 1;
    } else if (numFries === 2) {
        return 2;
    } else {
        const ways = [1, 2];
        for (var i = 2; i < numFries; i++) {
            ways[i] = Math.max(ways[i-1] + 1, ways[i-2] + 2);
        }
    }
    return ways[ways.length - 1];
}
