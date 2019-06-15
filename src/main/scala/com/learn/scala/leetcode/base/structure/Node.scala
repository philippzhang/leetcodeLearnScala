package com.learn.scala.leetcode.base.structure

/**
  * N叉树结点
  * @param _value
  * @param _children
  */
class Node(var _value: Int,var _children: List[Node]) {
  var value:Int = _value
  var children: List[Node] = _children
}
