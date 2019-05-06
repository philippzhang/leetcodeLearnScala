package com.learn.java.leetcode.lc0102

import com.learn.java.leetcode.base.structure.TreeNode

object Solution {
  def levelOrder(root: TreeNode): List[List[Int]] = {
    @annotation.tailrec
    def levelOrder(curLevel: List[TreeNode], res: List[List[Int]]): List[List[Int]] =
      if (curLevel.isEmpty)
        res
      else {
        val nextLevel = curLevel.foldRight(List.empty[TreeNode])((node, ls) => {
          if (node.left != null && node.right != null)
            node.left :: node.right :: ls
          else if (node.left != null)
            node.left :: ls
          else if (node.right != null)
            node.right :: ls
          else
            ls
        })
        levelOrder(nextLevel, curLevel.map(_.value) :: res)
      }

    if (root != null)
      levelOrder(List(root), List()).reverse
    else
      Nil
  }
}
