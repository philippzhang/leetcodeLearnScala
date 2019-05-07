package com.learn.java.leetcode.lc0206

import com.learn.java.leetcode.base.structure.ListNode

object Solution {
  def reverseList(head: ListNode): ListNode = {
    @annotation.tailrec
    def reverseList(head: ListNode, reverse: ListNode): ListNode = {
      if (head ==  null) reverse
      else {
        val tail = head.next
        head.next = reverse
        reverseList(tail, head)
      }
    }

    reverseList(head, null)
  }
}
