package com.learn.scala.leetcode.lc0105

import com.learn.scala.leetcode.base.structure.TreeNode

object Solution {
  def helper(preStart:Int ,preEnd:Int , inStart:Int ,inEnd:Int, preorder: Array[Int], inorder: Array[Int]): TreeNode = {
    if(preStart>preEnd || inStart>inEnd){
      return null
    }
    val root = new TreeNode(preorder(preStart))
    var inRoot = inStart
    //查找当前根节点在中序遍历的位置
    while(preorder(preStart)!=inorder(inRoot)){
      inRoot+=1
    }
    val len = inRoot - inStart
    root.left = helper(preStart+1,preStart+len,inStart,inRoot-1,preorder,inorder)
    root.right = helper(preStart+len+1,preEnd,inRoot+1,inEnd,preorder, inorder)
    root
  }
  def buildTree(preorder: Array[Int], inorder: Array[Int]): TreeNode = {
     helper(0,preorder.length-1,0,inorder.length-1,preorder,inorder)
  }
}
