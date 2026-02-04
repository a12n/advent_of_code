package day18

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"strconv"
)

func Part1(r *bufio.Reader, w io.Writer) error {
	var err error
	var trees []*Node

	if trees, err = ReadTrees(r); err != nil {
		return err
	}

	var i int
	var sum = trees[0]

	for i = 1; i < len(trees); i++ {
		sum = Add(sum, trees[i])
		sum.Reduce()
	}

	fmt.Fprintln(w, sum.Magnitude())

	return nil
}

func ReadTrees(r *bufio.Reader) ([]*Node, error) {
	var err error
	var trees []*Node

	for {
		var line string
		var tree *Node

		if line, err = r.ReadString('\n'); err != nil {
			if err == io.EOF {
				break
			}
			return nil, err
		}

		if tree, _, err = ParseNode(line); err != nil {
			return nil, err
		}

		trees = append(trees, tree)
	}

	return trees, nil
}

const (
	Left = iota
	Right
)

type Node struct {
	Value  int
	Child  [2]*Node
	Parent *Node
}

func (n *Node) IsLeaf() bool {
	return n.Child[Left] == nil && n.Child[Right] == nil
}

func (n *Node) Most(dir int) *Node {
	for n != nil && n.Child[dir] != nil {
		n = n.Child[dir]
	}
	return n
}

func (n *Node) FirstUp(dir int) *Node {
	if n == nil || n.Parent == nil {
		return nil
	}
	if n.Parent.Child[dir] == n {
		return n.Parent.FirstUp(dir)
	}
	return n.Parent.Child[dir]
}

func (n *Node) SetParent(p *Node) {
	if n == nil {
		return
	}
	n.Parent = p
}

func (n *Node) Copy() *Node {
	if n == nil {
		return nil
	}

	var node = &Node{
		Value: n.Value,
		Child: [2]*Node{
			Left:  n.Child[Left].Copy(),
			Right: n.Child[Right].Copy(),
		},
	}

	node.Child[Left].SetParent(node)
	node.Child[Right].SetParent(node)

	return node
}

func (n *Node) String() string {
	if n == nil {
		return "Ø"
	}
	if n.IsLeaf() {
		return strconv.Itoa(n.Value)
	}
	return "[" + n.Child[Left].String() + "," + n.Child[Right].String() + "]"
}

func Add(left, right *Node) *Node {
	log.Println("Add", left, right)
	var node = &Node{Child: [2]*Node{Left: left, Right: right}}
	left.SetParent(node)
	right.SetParent(node)
	log.Println("Add=", node)
	return node
}

func (n *Node) Magnitude() int {
	if n == nil {
		return 0
	}

	if n.IsLeaf() {
		return n.Value
	}

	return 3*n.Child[Left].Magnitude() + 2*n.Child[Right].Magnitude()
}

// TODO: Copy parts of the tree on write.
func (n *Node) Reduce() {
	log.Println("Reduce", n)
	for n.TryExplode(0) || n.TrySplit() {
		log.Println("Reduce=", n)
	}
}

func (n *Node) TryExplode(level int) bool {
	if n == nil || n.IsLeaf() || level > 4 {
		return false
	}

	// Inner node, too shallow, branch to children.
	if level < 4 {
		var dir int

		for dir = range n.Child {
			if n.Child[dir].TryExplode(level + 1) {
				return true
			}
		}

		return false
	}

	// Exploding pairs will always consist of two regular numbers.
	if !(n.Child[Left].IsLeaf() && n.Child[Right].IsLeaf()) {
		return false
	}

	// Inner node at level 4, with numbers in left and right, do explode.
	log.Println("Explode", n)

	var u *Node

	// the pair's left value is added to the first regular number to
	// the left of the exploding pair (if any)
	if u = n.FirstUp(Left).Most(Right); u != nil && u.IsLeaf() {
		u.Value += n.Child[Left].Value
	}

	// and the pair's right value is added to the first regular number
	// to the right of the exploding pair (if any).
	if u = n.FirstUp(Right).Most(Left); u != nil && u.IsLeaf() {
		u.Value += n.Child[Right].Value
	}

	// the entire exploding pair is replaced with the regular number 0.
	n.Child[Left] = nil
	n.Child[Right] = nil
	n.Value = 0

	log.Println("Explode=", n)

	return true
}

func (n *Node) TrySplit() bool {
	if n == nil {
		return false
	}

	// Inner node. Branch to children.
	if !n.IsLeaf() {
		var dir int

		for dir = range n.Child {
			if n.Child[dir].TrySplit() {
				return true
			}
		}

		return false
	}

	// Leaf node, less than 10.
	if n.Value < 10 {
		return false
	}

	// Leaf node, greater or equal to 10, do split.
	log.Println("Split", n)

	n.Child[Left] = &Node{Parent: n, Value: n.Value / 2}
	n.Child[Right] = &Node{Parent: n, Value: (n.Value + 1) / 2}
	n.Value = 0

	log.Println("Split=", n)

	return true
}

func ParseNode(str string) (*Node, string, error) {
	var err error
	var node = &Node{}

	if str == "" || str[0] != '[' {
		return nil, "", strconv.ErrSyntax
	}

	if node.Child[Left], str, err = Parse(str[1:]); err != nil {
		return nil, "", err
	}

	if str == "" || str[0] != ',' {
		return nil, "", strconv.ErrSyntax
	}

	if node.Child[Right], str, err = Parse(str[1:]); err != nil {
		return nil, "", err
	}

	if str == "" || str[0] != ']' {
		return nil, "", strconv.ErrSyntax
	}

	node.Child[Left].SetParent(node)
	node.Child[Right].SetParent(node)

	return node, str[1:], nil
}

func ParseLeaf(str string) (*Node, string, error) {
	var node = &Node{}

	if str == "" || !IsDigit(str[0]) {
		return nil, "", strconv.ErrSyntax
	}

	for ; str != "" && IsDigit(str[0]); str = str[1:] {
		node.Value = node.Value*10 + int(str[0]-'0')
	}

	return node, str, nil
}

func Parse(str string) (*Node, string, error) {
	if str != "" {
		if str[0] == '[' {
			return ParseNode(str)
		} else if IsDigit(str[0]) {
			return ParseLeaf(str)
		}
	}
	return nil, "", strconv.ErrSyntax
}

func IsDigit(b byte) bool {
	return b >= '0' && b <= '9'
}
