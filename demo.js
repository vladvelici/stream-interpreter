var Stream = function(next, sources) {
	this.procNext = next;
	this.sources = sources !== undefined ? sources : [];
	this.count = 0;
}

Stream.prototype.next = function() {
	var curr = [];
	console.log("this is called", this.sources);
	for (var i in this.sources) {
		var stream = this.sources[i];
		console.log(typeof i, "this was str");
		curr.push(stream.next());
	}

	return this.procNext(this.count++, curr);
}

Stream.fromArray = function(arr) {
	var last = 0;
	return new Stream(function(el) {
		if (last < arr.length) return arr[last++];
		return null;
	});
}

Stream.copyTwo = function(stream) {
	var q1 = [], q2 = [];
	var next = function(me, other) {
		return function(el) {
			if (me.length == 0) {
				var val = stream.next();
				other.push(val);
				return val;
			}
			return me.shift();
		}
	}
	return [new Stream(next(q1,q2)), new Stream(next(q2, q1))];	
}

var newCountStream = function() {return new Stream(function(i) { return i; }); };
var dblStream = new Stream(function(count, srcs) {
	var el = srcs[0];
	if (el == null) return null;
	return el*2;
}, [newCountStream()]);


var streams = Stream.copyTwo(Stream.fromArray([4,6,4,6,4,6,4,6,4,6,4]));
var s1 = streams[0];
var s2 = streams[1];

s1.next();

var coolAdd = new Stream(function(count, el) { return el[0]+el[1]; }, [s1, s2]);
