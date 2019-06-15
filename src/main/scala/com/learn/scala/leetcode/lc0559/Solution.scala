package com.learn.scala.leetcode.lc0559

import com.learn.scala.leetcode.base.structure.Node
import com.learn.scala.leetcode.base.utils.{Format, PrintObj}

object Solution {
  def maxDepth(root: Node): Int = {
    PrintObj.printObj(root);
    println(Format.format(root))
    getHeight(root)
  }

  private def getHeight(node: Node): Int = {
    if (node == null) return 0
    var max = 0
    val children = node.children
    for (n <- children) {
      max = Math.max(max, getHeight(n))
    }
    max + 1
  }
}
