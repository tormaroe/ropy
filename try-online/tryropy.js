var app = new Vue({
    el: '#app',
    data: {
        source: '',
        program: make_program_object(''),
        intervalId: undefined,
        sleepLength: 100,
        intervalCount: 0,
        output: '',
        examples: [
            { title: 'Empty program', source: ''},
            { title: 'Hello World', source: '    |   __+8>____-<3>____+6>__ _________________________\n    |  |                      |                         |\n    |  |   __>-<3>__&&>>1__   |  _____   ____  _______  |  __\n    |  |  |                |  | |  __ \\ / __ \\|  __ \\ \\ | / /\n    |  |  |                |  | | |__) | |  | | |__) \\ \\_/ / \n    |  |  |__>7<-__>3+__\"  |  | |  _  /| |  | |  ___/ \\   /  \n    |  |                   |  | | | \\ \\| |__| | |      | |   \n    |  |__65*2+____67*2<+__|  | |_|  \\_\\\\____/|_|      |_|   \n    |                         |                         |\n    |____001&&________>8+_____|__+ - * / < > ? % ! & \"__|\n'}
        ]
    },
    methods: {
        runProgram: function () {
            console.log('run()' + this.source);
            var self = this;
            this.output = '';
            this.program = make_program_object(this.source);
            this.program.print = this.print;
            this.intervalId = setInterval(function () {
                self.intervalCount++;
                evaluate(self.program);
                if(self.program.done) {
                    self.stopProgram();
                }
            }, this.sleepLength);
        },
        stopProgram: function () {
            if (this.intervalId) {
                clearInterval(this.intervalId);
            }
        },
        print: function (txt) {
            this.output += txt;
        }
    },
    computed: {
        token: function () {
            return current(this.program);
        }
    }
});