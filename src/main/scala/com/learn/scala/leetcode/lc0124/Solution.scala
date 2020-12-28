package com.learn.scala.leetcode.lc0124

import com.learn.scala.leetcode.base.structure.TreeNode

object Solution {
  var ret:Int = Int.MinValue
  def maxPathSum(root: TreeNode): Int = {
    ret = Int.MinValue
    getPathSum(root)
    ret
  }

  def getPathSum(root: TreeNode): Int = {
    if(root == null){
      return 0
    }
    val left = Math.max(0,getPathSum(root.left))
    val right = Math.max(0,getPathSum(root.right))
    ret = Math.max(ret, left+right+root.value)
    math.max(left,right) + root.value

  }
}
