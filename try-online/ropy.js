
var tokenize = function (source) {
    var result = [];
    source.split('\n').forEach(l => {
        result.push(l.split(''))
    });
    return result;
};

var current = function (state) {
    if (state.i >= state.tokens.length || state.j >= state.tokens[state.i].length) {
        state.done = true;
        return undefined;
    }
    return state.tokens[state.i][state.j];
};

var make_program_object = function (source) {
    var state = {
        stack: [],
        memory: {},
        tokens: tokenize(source),
        i: 0,
        j: 0,
        done: false,
        prev_direction: "east",
        print: function (x) { console.log(x); }
    };

    while (true) {
        var c = current(state);
        if (!state.done && (c == ' ' || c == undefined))
            seek_token(state);
        else
            break;
    }

    return state;
};

var valid_coords = function (state, coords) {
    return coords[0] >= 0 &&
        coords[1] >= 0 &&
        state.tokens.length > coords[0] &&
        state.tokens[coords[0]].length > coords[1];
};

var coords_for_direction = function (state, direction) {
    switch (direction) {
        case 'east': return [state.i, state.j + 1];
        case 'west': return [state.i, state.j - 1];
        case 'north': return [state.i - 1, state.j];
        case 'south': return [state.i + 1, state.j];
        case 'northeast': return [state.i - 1, state.j + 1];
        case 'southeast': return [state.i + 1, state.j + 1];
        case 'northwest': return [state.i - 1, state.j - 1];
        case 'southwest': return [state.i + 1, state.j - 1];
    }
};

var move = function (state, direction) {
    var tmp = coords_for_direction(state, direction);
    state.i = tmp[0];
    state.j = tmp[1];
    state.prev_direction = direction;
};

var seek_token = function (state) {
    if (state.j < (state.tokens[state.i].length - 1))
        move(state, 'east');
    else if (state.i < state.tokens.length) {
        state.i += 1
        state.j = 0
    }
};

var oposite = function (direction) {
    switch (direction) {
        case 'east': return 'west';
        case 'west': return 'east';
        case 'north': return 'south';
        case 'south': return 'north';
        case 'northeast': return 'southwest';
        case 'southeast': return 'northwest';
        case 'northwest': return 'southeast';
        case 'southwest': return 'northeast';
    }
};

var peek_token = function (state, direction) {
    var coords = coords_for_direction(state, direction);
    if (direction == oposite(state.prev_direction)
        || !valid_coords(state, coords)
        || state.tokens[coords[0]][coords[1]] === ' ') {
        return undefined;
    }
    return state.tokens[coords[0]][coords[1]];
};

var move_next = function (state) {
    var neighbors = [];
    var valid_count = 0;
    ['east', 'southeast', 'south', 'southwest', 
     'west', 'northwest', 'north', 'northeast'].forEach(d => {
         var token = peek_token(state, d);
         if (token != undefined)
            valid_count++;
         neighbors.push([d, token])
    });

    state.done = valid_count == 0;

    if (!state.done) {
        var came_from_direction = oposite(state.prev_direction);
        var came_from_index = -1;
        for (var i = 0; i < neighbors.length; i++) {
            if (neighbors[i][0] === came_from_direction) {
                came_from_index = i;
                break;
            }
        }

        if (result(state) === 0) {
            for (var i = (came_from_index + 7); i <= (came_from_index - 1); i--) {
                var ii = i % 8;
                if (neighbors[ii][1] != undefined) {
                    move(state, neighbors[ii][0]);
                    break;
                }
            }
        } else {
            for (var i = (came_from_index + 1); i <= (came_from_index + 9); i++) {
                var ii = i % 8;
                if (neighbors[ii][1] != undefined) {
                    move(state, neighbors[ii][0]);
                    break;
                }
            }
        }
    }
};

var push = function (state, value) {
    state.stack.push(value);
};

var pop = function (state) {
    return state.stack.pop();
};

var put = function (state) {
    var data = pop(state);
    var loc = pop(state);
    state.memory[loc] = data;
};

var get = function (state) {
    push(state, state.memory[pop(state)]);
};

var add = function (state) {
    push(state, parseInt(pop(state)) + parseInt(pop(state)));
};

var multiply = function (state) {
    push(state, parseInt(pop(state)) * parseInt(pop(state)));
};

var subtract = function (state) {
    push(state, parseInt(pop(state)) - parseInt(pop(state)));
};

var modulo = function (state) {
    push(state, parseInt(pop(state)) % parseInt(pop(state)));
};

var devide = function (state) {
    push(state, parseInt(pop(state)) / parseInt(pop(state)));
};

var join = function (state) {
    push(state, parseInt('' + pop(state) + pop(state)));
};

var swap = function (state) {
    var a = pop(state), b = pop(state);
    push(state, a);
    push(state, b);
};

var duplicate = function (state) {
    var x = pop(state);
    push(state, x);
    push(state, x);
};

var result = function (state) {
    return state.stack[state.stack.length - 1];
};

var not = function (state) {
    push(state, parseInt(pop(state)) > 0 ? 0 : 1);
};

var printit = function (state) {
    state.print(pop(state));
}

var stringify_stack = function (state) {
    var s = '';
    while (state.stack.length > 0) {
        s += String.fromCharCode(parseInt(pop(state)));
    }
    push(state, s);
};

var evaluate = function (state) {
    var token = current(state);
    if (token >= '0' && token <= '9')
        push(state, token);
    else {
        switch (token) {
            case '&':
                join(state);
                break;
            case '+':
                add(state);
                break;
            case '-':
                subtract(state);
                break;
            case '*':
                multiply(state);
                break;
            case '/':
                devide(state);
                break;
            case '<':
                swap(state);
                break;
            case '>':
                duplicate(state);
                break;
            case '"':
                stringify_stack(state);
                break;
            case '?':
                pop(state);
                break;
            case '%':
                modulo(state);
                break;
            case '!':
                not(state);
                break;
            case '[':
                put(state);
                break;
            case ']':
                get(state);
                break;
            case '#':
                printit(state);
                break;
        }
    }
    move_next(state);
};

var execute = function (state) {
    while (!state.done) {
        evaluate(state);
    }
};

