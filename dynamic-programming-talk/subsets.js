function numSubsets(numbers, target) {
    const numSubsets = [];
    const newTarget = [];

    for (var i = 0; i < numbers.length; i++) {
        numSubsets.push([]);
        for (var j = 0; j < target; j++) {
            const num = numbers[i];
            if(num > j) {
                numSubsets[i][j] = numSubsets[i-1][j];
            } else if (num < j) {

            }
        }
    }
}
