package com.learn.scala.leetcode.lc0173

import com.learn.scala.leetcode.base.structure.TreeNode

import scala.collection.mutable.Stack

class BSTIterator(_root: TreeNode) {
  private val stack: Stack[Int] = new Stack()
  buildStack(_root, stack)

  /** @return the next smallest number */
  def next(): Int = if (hasNext) stack.pop else -1

  /** @return whether we have a next smallest number */
  def hasNext(): Boolean = stack!=null && !stack.isEmpty

  private def buildStack(root: TreeNode, stack: Stack[Int]): Unit = {
    if (root != null) {
      buildStack(root.right, stack)
      stack.push(root.value)
      buildStack(root.left, stack)
    }
  }
}