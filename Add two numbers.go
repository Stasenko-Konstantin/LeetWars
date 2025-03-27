/*

You are given two non-empty linked lists representing two non-negative integers. The digits are stored in reverse order, and each of their nodes contains a single digit. Add the two numbers and return the sum as a linked list.

You may assume the two numbers do not contain any leading zero, except the number 0 itself.

*/

import "math/big"

/**
 * Definition for singly-linked list.
 * type ListNode struct {
 *     Val int
 *     Next *ListNode
 * }
 */

func addTwoNumbers(l1 *ListNode, l2 *ListNode) *ListNode {
	n1, n2 := listToArr(l1), listToArr(l2)
	//fmt.Println(n1, "\n", n2)
	i1, i2 := arrToInt(n1), arrToInt(n2)
	//fmt.Println(i1, i2, ", +: ", new(big.Int).Add(i1, i2))
	return numToList(&ListNode{}, new(big.Int).Add(i1, i2))
}

func listToArr(l *ListNode) []int {
	res := []int{l.Val}
	for n, next := getNum(l); ; n, next = getNum(n) {
		if !next {
			break
		}
		res = append(res, n.Val)
	}
	return res
}

func arrToInt(n []int) *big.Int {
	res := big.NewInt(int64(n[0]))
	if len(n) == 1 {
		return res
	}
	for i, num := range n[1:] {
		k := new(big.Int).Exp(big.NewInt(10), big.NewInt(int64(i+1)), nil)
		//fmt.Print("i: ", i+1, ", p: ", k)
		k = k.Mul(k, big.NewInt(int64(num)))
		//fmt.Println(", k: ", k)
		res = res.Add(res, k)
	}
	return res
}

func numToList(l *ListNode, n *big.Int) *ListNode {
	//fmt.Println("\n>10: ", n.Cmp(big.NewInt(10)))
	if n.Cmp(big.NewInt(10)) == -1 {
		l.Val = int(n.Int64())
		return l
	}
	//fmt.Println("n: ", new(big.Int).Div(n, big.NewInt(10)))
	l.Next = numToList(&ListNode{}, new(big.Int).Div(n, big.NewInt(10)))
	//fmt.Println("v: ", int(new(big.Int).Rem(n, big.NewInt(10)).Int64()))
	l.Val = int(new(big.Int).Rem(n, big.NewInt(10)).Int64())
	return l
}

func getNum(l *ListNode) (*ListNode, bool) {
	if l.Next == nil {
		return l, false
	}
	return l.Next, true
}
