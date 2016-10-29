let SCREEN_WIDTH = 1900;
let SCREEN_HEIGHT = 1500;

let balls;
function setup() {
    //let colors = [{ r: 255, g: 0, b: 0 }, { r: 0, g: 255, b: 0 }, { r: 100, g: 50, b: 0 }, { r: 200, g: 30, b: 0 }];
    let colors = Array.apply(null, { length: 10 }).map((v, i) => {
        return {
            r: i * 25,
            g: i * 50 * Math.random(),
            b: i * 100 * Math.random() * Math.random(),
        };
    });
    balls = colors.map(c => new Ball({ colorR: c.r, colorG: c.g, colorB: c.b }));
}

function draw() {
    createCanvas(SCREEN_WIDTH, SCREEN_HEIGHT);
    balls = balls.map(b => {
        if (b.done()) {
            return new Ball(b);
        } else {
            b.move();
            b.draw();
            return b;
        }
    });
}

class Ball {

    static random(upper) {
        return Math.floor(random(upper || 200));
    }

    toward(value, toward) {
        if (value === toward) {
            return value;
        } else {
            let step = 1;
            let diff = Math.abs(value - toward);
            let ceil = value > toward ? value : toward;
            const decayRate = 1 - (this.percentComplete() + 4 * diff/ceil)/5;
            if (diff > 10 && this.speed * Math.pow(decayRate, diff) > diff - 10) {
                step = Math.floor(this.speed * Math.pow(decayRate, diff));
                //this.speed = step;
            }
            return value > toward ? value - step : value + step;
        }
    }

    constructor(params) {
        this.speed = Ball.random(50);
        this.colorR = params.colorR;
        this.colorB = params.colorB;
        this.colorG = params.colorG;
        this.maxWidth = Ball.random(SCREEN_WIDTH/5);
        this.maxRight = Ball.random(SCREEN_WIDTH/2);
        this.maxTop = Ball.random(SCREEN_HEIGHT/2);
        this.right = Ball.random(SCREEN_WIDTH);
        this.top = Ball.random(SCREEN_HEIGHT);
        this.width = Ball.random(SCREEN_WIDTH/5);
    }

    move() {
        this.right = this.toward(this.right, this.maxRight);
        this.top = this.toward(this.top, this.maxTop);
        this.width  = this.toward(this.width, this.maxWidth);
    }

    percentComplete() {
        return [
            this.right/this.maxRight,
            this.top/this.maxTop,
            this.width/this.maxWidth
        ].reduce((prev, curr) => prev + curr, 0) / 3;
    }

    draw() {
        const percentComplete = 255 * (1 - this.percentComplete());
        const red = 255 - this.colorR * percentComplete;
        const green = 255 - this.colorG * percentComplete;
        const blue = 255 - this.colorB * percentComplete;
        const colorVal = color(this.colorR, this.colorG, this.colorB, percentComplete);
        fill(colorVal);
        noStroke();
        ellipse(this.right, this.top, this.width, this.width);
    }

    done() {
        return this.percentComplete() >= 0.9999;
    }
}

