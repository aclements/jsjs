var ALPHA = 0.51;
var XALPHA = 0.51;

function Scapegoat() {
    this.size = 0;
    this.root = null;
}

Scapegoat.prototype.find = function(val) {
    var path = [{node: this, dir: "root"}];
    var node = this.root, dir;
    while (node) {
        if (node.val == val)
            return true;
        if (node.val > val)
            dir = "left";
        else if (node.val < val)
            dir = "right";
        path.push({node: node, dir: dir});
        node = node[dir];
    }
    return path;
};

Scapegoat.prototype.contains = function(val) {
    return this.find(val) === true;
};

Scapegoat.prototype.insert = function(val) {
    var path = this.find(val);
    if (path === true)
        return;

    var last = path[path.length - 1];
    last.node[last.dir] = {val: val};
    ++this.size;

    // Is the tree still alpha-height-balanced?
    var depth = path.length - 1;
    var maxHeight = Math.floor(Math.log(this.size) / Math.log(1/ALPHA));
    if (depth <= maxHeight)
        return;

    // This insertion pushed the tree over its maximum height, so it's
    // no longer alpha-height-balanced.  This means some ancestor of
    // the inserted node is not alpha-weight-balanced.  This node is
    // the scapegoat.  Find it by checking each ancestor.  This takes
    // time linear in the size of the subtree rooted at the scapegoat,
    // but the magic of the scapegoat tree is that this amortizes away
    // over many inserts.
    var size = 1, i;
    for (i = path.length - 1; i > 0; --i) {
        // Compute the size of path[i].node, reusing what we
        // already know about one of its children.
        var sibling = path[i].dir === "left" ? "right" : "left";
        var siblingSize = this.treeSize(path[i].node[sibling]);
        var childSize = size;
        size = 1 + childSize + siblingSize;

        // Is path[i].node alpha-weight-balanced?
        if (childSize > ALPHA*size || siblingSize > ALPHA*size)
            // Nope.  path[i].node is the scapegoat.
            break;
    }

    // Rebalance the tree at the scapegoat
    console.log(path[i].node.val, i, size, this.treeSize(path[i].node));
//    path[i-1].node[path[i-1].dir] =
//        this.balance({head: this.flatten(path[i].node, null)}, size);

    var w = {};
    this.buildTree(size, this.flatten(path[i].node, w));
    path[i-1].node[path[i-1].dir] = w.left;
};

Scapegoat.prototype.treeSize = function(node) {
    if (!node)
        return 0;
    return 1 + this.treeSize(node.left) + this.treeSize(node.right);
};

Scapegoat.prototype.flatten = function(node, rest) {
    if (!node)
        return rest;
    var left = node.left;
    node.right = this.flatten(node.right, rest);
    node.left = null;
    return this.flatten(left, node);
};

// Create a balanced tree of size 'size' from the node list starting
// at 'list.head'.  Returns the new root node and consumes the first
// 'size' elements of 'list'.
Scapegoat.prototype.balance = function(list, size) {
    if (size == 0)
        return null;
    var leftSize = Math.floor((size - 1) / 2), rightSize = size - leftSize - 1;
    var left = this.balance(list, leftSize);
    var root = list.head;
    list.head = root.right;
    root.left = left;
    root.right = this.balance(list, rightSize);
    return root;
};

Scapegoat.prototype.buildTree = function(size, node) {
    if (size == 0) {
        node.left = null;       // XXX Unnecessary?
        return node;
    }
    var r = this.buildTree(Math.ceil((size - 1) / 2), node);
    var s = this.buildTree(Math.floor((size - 1) / 2), r.right);
    r.right = s.left;
    s.left = r;
    return s;
};

Scapegoat.prototype.forEach = function(cb) {
    var othis = this;
    function rec(node, parent) {
        if (!node)
            return;
        cb.call(othis, node, parent);
        rec(node.left, node);
        rec(node.right, node);
    }
    rec(this.root, null);
};

Scapegoat.prototype.makeAnim = function(ctx, ctxDisplay) {
    var width = ctx.canvas.width;
    var nodeSize = width / 15;
    var anim = new Anim(0.5, this.draw.bind(this, ctx, ctxDisplay));

    function placeNodes(node, width, x, y) {
        if (!node)
            return;
        var size = Math.min(nodeSize, width), radius = size * 0.4;
        var nx = x + width / 2, ny = y + size / 2;
        if (node._x === undefined) {
            // Place the node and fade in
            node._x = nx;
            node._y = ny;
            node._radius = radius;
            anim.propFloat(node, "_opacity", 0, 1);
        } else {
            anim.propFloat(node, "_x", node._x, nx);
            anim.propFloat(node, "_y", node._y, ny);
            anim.propFloat(node, "_radius", node._radius, radius);
        }
        placeNodes(node.left, width / 2, x, y + size);
        placeNodes(node.right, width / 2, x + width / 2, y + size);
    }

    function markNode(node) {
        var leftSize = this.treeSize(node.left);
        var rightSize = this.treeSize(node.right);
        var size = leftSize + rightSize + 1;
        var color;
        if (leftSize > XALPHA * size || rightSize > XALPHA * size)
            color = "#ff0000";
        else
            color = "#e2ebf5";
        if (node._color === undefined)
            node._color = color;
        else
            anim.propColor(node, "_color", node._color, color);
    }

    placeNodes(this.root, width, 0, 0);

    this.forEach(markNode);

    return anim;
};

Scapegoat.prototype.draw = function(ctx, ctxDisplay) {
    function drawEdge(node, parent) {
        if (!parent)
            return;
        if (node._opacity !== 1) {
            ctx.stroke();
            ctx.beginPath();
            ctx.globalAlpha = node._opacity;
        }
        ctx.moveTo(parent._x, parent._y);
        ctx.lineTo(node._x, node._y);
        if (node._opacity !== 1) {
            ctx.stroke();
            ctx.beginPath();
            ctx.globalAlpha = 1;
        }
    }

    function drawNode(node) {
        ctx.beginPath();
        ctx.fillStyle = node._color;
        ctx.globalAlpha = node._opacity;
        ctx.moveTo(node._x + node._radius, node._y);
        ctx.arc(node._x, node._y, node._radius, 0, Math.PI * 2);
        ctx.fill();
        ctx.stroke();
    }

    function drawLabel(node) {
        if (node._radius <= 5)
            return;
        ctx.globalAlpha = node._opacity;
        ctx.fillText(node.val, node._x, node._y);
    }

    ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height);

    ctx.save();
    ctx.beginPath();
    ctx.strokeStyle = "#3465a4";
    ctx.lineWidth = 2;
    this.forEach(drawEdge);
    ctx.stroke();
    ctx.restore();

    ctx.save();
//    ctx.fillStyle = "#e2ebf5";
    ctx.strokeStyle = "#3465a4";
    this.forEach(drawNode);
    ctx.restore();

    ctx.save();
    ctx.textAlign = "center";
    ctx.textBaseline = "middle";
    ctx.fillStyle = "black";
    this.forEach(drawLabel);
    ctx.restore();

    ctxDisplay.clearRect(
        0, 0, ctxDisplay.canvas.width, ctxDisplay.canvas.height);
    ctxDisplay.save();
    ctxDisplay.shadowOffsetX = ctxDisplay.shadowOffsetY = 2;
    ctxDisplay.shadowBlur = 2;
    ctxDisplay.shadowColor = "rgba(0, 0, 0, 0.3)";
    ctxDisplay.drawImage(ctx.canvas, 0, 0);
    ctxDisplay.restore();
};

function Anim(duration, renderFn) {
    this._duration = duration;
    this._renderFn = renderFn;
    this._updaters = [];
};

Anim.prototype.start = function() {
    this._t0 = new Date().getTime();
    this._tick();
};

Anim.prototype._tick = function() {
    var now = new Date().getTime();
    var delta = (now - this._t0) / 1000;
    var t = delta / this._duration;
    if (t > 1)
        t = 1;
    for (var i = 0; i < this._updaters.length; i++)
        this._updaters[i](t);
    this._renderFn();
    if (t < 1)
        requestAnimationFrame(this._tick.bind(this));
};

Anim.prototype.propFloat = function(obj, prop, start, end) {
    this._updaters.push(function(t) {
        t = (-Math.cos(t * Math.PI) / 2) + 0.5;
        obj[prop] = start * (1 - t) + end * t;
    });
};

Anim.prototype.propColor = function(obj, prop, start, end) {
    function parse(v) {
        var m;
        if (m = /^\s*#([0-9a-fA-F]{2})([0-9a-fA-F]{2})([0-9a-fA-F]{2})\s*$/.exec(v) ||
            (m = /^\s*#([0-9a-fA-F])([0-9a-fA-F])([0-9a-fA-F])\s*$/.exec(v)))
            return [parseInt(m[1], 16),
                    parseInt(m[2], 16),
                    parseInt(m[3], 16), 1];
        if (m = /^\s*rgb\s*\(\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*\)\s*$/.exec(v))
            return [parseInt(m[1]), parseInt(m[2]), parseInt(m[3]), 1];
        if (m = /^\s*rgba\s*\(\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*\)\s*$/.exec(v))
            return [parseInt(m[1]), parseInt(m[2]), parseInt(m[3]),
                    parseFloat(m[4])];
        throw "Bad color: " + v;
    }
    start = parse(start);
    end = parse(end);
    this._updaters.push(function(t) {
        var ot = 1 - t;
        function x(n) { return Math.floor(start[n] * ot + end[n] * t); }
        obj[prop] = ("rgba(" + x(0) + "," + x(1) + "," + x(2) + "," +
                     (start[3] * ot + end[3] * t) + ")");
    });
};

var s = new Scapegoat();
//for (var i = 0; i < 2000; i++)
//    s.insert(Math.floor(Math.random() * 100));
//    s.insert(i);
var i = 0;
function addElt() {
    s.insert(i++);
    //s.insert(Math.floor(Math.random() * 100));
    s.makeAnim(document.getElementById("canvas-back").getContext("2d"),
               document.getElementById("canvas").getContext("2d")).start();
}

window.onload = function() {
//    s.draw(document.getElementById("canvas-back").getContext("2d"),
//           document.getElementById("canvas").getContext("2d"));
    document.getElementById("canvas").onclick = addElt;
};
