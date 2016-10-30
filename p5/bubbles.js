let SCREEN_WIDTH = 1900;
let SCREEN_HEIGHT = 1000;

let balls;
function setup() {
    //let colors = [{ r: 255, g: 0, b: 0 }, { r: 0, g: 255, b: 0 }, { r: 100, g: 50, b: 0 }, { r: 200, g: 30, b: 0 }];
    let colors = Array.apply(null, { length: 20 }).map((v, i) => {
        return {
            r: ((i % 5) * 10 * Math.random()) % 255,
            g: ((i % 7) * 90  * Math.random()) % 255,
            b: ((i % 11) * 50 * Math.random()) % 255,
        };
    });
    balls = colors.map(c => new Ball({ colorR: c.r, colorG: c.g, colorB: c.b }));
}

function draw() {
    createCanvas(SCREEN_WIDTH, SCREEN_HEIGHT);
    balls = balls.map(b => {
        if (b.done()) {
            return new Ball(Ball.generateColor());
        } else {
            b.move();
            b.draw();
            return b;
        }
    });
}

function cubicInOut(t) {
      return t < 0.5
          ? 4.0 * t * t * t
          : 0.5 * Math.pow(2.0 * t - 2.0, 3.0) + 1.0
}

function elasticInOut(t) {
      return t < 0.5
          ? 0.5 * Math.sin(+13.0 * Math.PI/2 * 2.0 * t) * Math.pow(2.0, 10.0 * (2.0 * t - 1.0))
          : 0.5 * Math.sin(-13.0 * Math.PI/2 * ((2.0 * t - 1.0) + 1.0)) * Math.pow(2.0, -10.0 * (2.0 * t - 1.0)) + 1.0
}

function backInOut(t) {
      var s = 1.70158 * 1.525
        if ((t *= 2) < 1)
                return 0.5 * (t * t * ((s + 1) * t - s))
              return 0.5 * ((t -= 2) * t * ((s + 1) * t + s) + 2)
}

class Ball {
    static generateColor() {
        const red = 500 * Math.random();
        const green = 200 * Math.random() * red;
        const blue = 300 * Math.random() * green;

        return { colorR: red % 255, colorG: green % 255, colorB: blue % 255 };
    }

    static random(upper) {
        return Math.floor(random(upper || 200));
    }

    toward(value, end, start) {
        if (value === end) {
            return value;
        } else {
            let distance = end - start;

            let progress = ++this.time / this.speed;
            progress = progress > 1 ? 1 : progress;

            const easing = this.easeFn(progress);
            const result = start + Math.floor(distance * easing);
            return result;
        }
    }

    constructor(params) {
        const easeFns = [cubicInOut, elasticInOut, backInOut];
        this.easeFn = easeFns[Ball.random() % easeFns.length];
        this.time = 0;
        this.speed = Ball.random(5000);
        this.colorR = params.colorR;
        this.colorB = params.colorB;
        this.colorG = params.colorG;
        this.maxWidth = Ball.random(SCREEN_WIDTH/5);
        this.maxRight = Ball.random(SCREEN_WIDTH/2 + this.maxWidth);
        this.maxTop = Ball.random(SCREEN_HEIGHT/2 + this.maxWidth);

        this.width = Ball.random(SCREEN_WIDTH/5);
        this.right = Ball.random(SCREEN_WIDTH) + this.width;
        this.top = Ball.random(SCREEN_HEIGHT) + this.width;

        this.widthStart = this.width;
        this.rightStart = this.right;
        this.topStart = this.top;
    }

    move() {
        this.right = this.toward(this.right, this.maxRight, this.rightStart);
        this.top = this.toward(this.top, this.maxTop, this.topStart);
        this.width  = this.toward(this.width, this.maxWidth, this.widthStart);
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
        const colorVal = color(this.colorR, this.colorG, this.colorB, percentComplete);
        fill(colorVal);
        noStroke();
        ellipse(this.right, this.top, this.width);
    }

    done() {
        return this.percentComplete() >= 0.98;
    }
}

