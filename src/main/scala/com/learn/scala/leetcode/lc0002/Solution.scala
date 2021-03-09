package com.learn.scala.leetcode.lc0002

import com.learn.scala.leetcode.base.structure.ListNode

object Solution {
  def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {
    if(l1==null)return l2
    if(l2==null)return l1
    val ret = new ListNode()
    var s = 0 //高位值
    var r = 0 //当前位值
    var temp1 = l1
    var temp2 = l2
    var temp = ret
    while(temp1!=null && temp2!=null){
      r = temp1.x+temp2.x + s
      s = r/10
      r = r%10
      temp.next = new ListNode(r)
      temp = temp.next
      temp1 = temp1.next
      temp2 = temp2.next
    }

    while(temp1!=null || temp2!=null){
      r = (if(temp1!=null) temp1.x else temp2.x)+s
      s = r/10
      r = r%10
      temp.next = new ListNode(r)
      temp = temp.next
      if(temp1!=null) temp1 = temp1.next else temp2 = temp2.next
    }

    if(s>0){
      temp.next = new ListNode(s)
      //temp = temp.next
    }

    ret.next
  }
}
