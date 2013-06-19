var scapegoatStr = "var ALPHA = 0.51;\n\
var XALPHA = 0.51;\n\
\n\
function Scapegoat() {\n\
    this.size = 0;\n\
    this.root = null;\n\
}\n\
\n\
Scapegoat.prototype.find = function(val) {\n\
    var path = [{node: this, dir: \"root\"}];\n\
    var node = this.root, dir;\n\
    while (node) {\n\
        if (node.val == val)\n\
            return true;\n\
        if (node.val > val)\n\
            dir = \"left\";\n\
        else if (node.val < val)\n\
            dir = \"right\";\n\
        path.push({node: node, dir: dir});\n\
        node = node[dir];\n\
    }\n\
    return path;\n\
};\n\
\n\
Scapegoat.prototype.contains = function(val) {\n\
    return this.find(val) === true;\n\
};\n\
\n\
Scapegoat.prototype.insert = function(val) {\n\
    var path = this.find(val);\n\
    if (path === true)\n\
        return;\n\
\n\
    var last = path[path.length - 1];\n\
    last.node[last.dir] = {val: val};\n\
    ++this.size;\n\
\n\
    // Is the tree still alpha-height-balanced?\n\
    var depth = path.length - 1;\n\
    var maxHeight = Math.floor(Math.log(this.size) / Math.log(1/ALPHA));\n\
    if (depth <= maxHeight)\n\
        return;\n\
\n\
    // This insertion pushed the tree over its maximum height, so it's\n\
    // no longer alpha-height-balanced.  This means some ancestor of\n\
    // the inserted node is not alpha-weight-balanced.  This node is\n\
    // the scapegoat.  Find it by checking each ancestor.  This takes\n\
    // time linear in the size of the subtree rooted at the scapegoat,\n\
    // but the magic of the scapegoat tree is that this amortizes away\n\
    // over many inserts.\n\
    var size = 1, i;\n\
    for (i = path.length - 1; i > 0; --i) {\n\
        // Compute the size of path[i].node, reusing what we\n\
        // already know about one of its children.\n\
        var sibling = path[i].dir === \"left\" ? \"right\" : \"left\";\n\
        var siblingSize = this.treeSize(path[i].node[sibling]);\n\
        var childSize = size;\n\
        size = 1 + childSize + siblingSize;\n\
\n\
        // Is path[i].node alpha-weight-balanced?\n\
        if (childSize > ALPHA*size || siblingSize > ALPHA*size)\n\
            // Nope.  path[i].node is the scapegoat.\n\
            break;\n\
    }\n\
\n\
    // Rebalance the tree at the scapegoat\n\
    console.log(path[i].node.val, i, size, this.treeSize(path[i].node));\n\
//    path[i-1].node[path[i-1].dir] =\n\
//        this.balance({head: this.flatten(path[i].node, null)}, size);\n\
\n\
    var w = {};\n\
    this.buildTree(size, this.flatten(path[i].node, w));\n\
    path[i-1].node[path[i-1].dir] = w.left;\n\
};\n\
\n\
Scapegoat.prototype.treeSize = function(node) {\n\
    if (!node)\n\
        return 0;\n\
    return 1 + this.treeSize(node.left) + this.treeSize(node.right);\n\
};\n\
\n\
Scapegoat.prototype.flatten = function(node, rest) {\n\
    if (!node)\n\
        return rest;\n\
    var left = node.left;\n\
    node.right = this.flatten(node.right, rest);\n\
    node.left = null;\n\
    return this.flatten(left, node);\n\
};\n\
\n\
// Create a balanced tree of size 'size' from the node list starting\n\
// at 'list.head'.  Returns the new root node and consumes the first\n\
// 'size' elements of 'list'.\n\
Scapegoat.prototype.balance = function(list, size) {\n\
    if (size == 0)\n\
        return null;\n\
    var leftSize = Math.floor((size - 1) / 2), rightSize = size - leftSize - 1;\n\
    var left = this.balance(list, leftSize);\n\
    var root = list.head;\n\
    list.head = root.right;\n\
    root.left = left;\n\
    root.right = this.balance(list, rightSize);\n\
    return root;\n\
};\n\
\n\
Scapegoat.prototype.buildTree = function(size, node) {\n\
    if (size == 0) {\n\
        node.left = null;       // XXX Unnecessary?\n\
        return node;\n\
    }\n\
    var r = this.buildTree(Math.ceil((size - 1) / 2), node);\n\
    var s = this.buildTree(Math.floor((size - 1) / 2), r.right);\n\
    r.right = s.left;\n\
    s.left = r;\n\
    return s;\n\
};";
