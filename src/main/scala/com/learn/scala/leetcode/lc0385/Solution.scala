package com.learn.scala.leetcode.lc0385

import com.learn.scala.leetcode.base.structure.NestedInteger

object Solution {
  def deserialize(s: String): NestedInteger = {
    if (s.charAt(0) != '[') {
      val result = new NestedInteger
      result.setInteger(s.toInt)
      result
    }
    else {
      val result = new NestedInteger
      var index = -1
      var cnt = 0
      for (i <- 1 until s.length) {
        val ch = s.charAt(i)
        if (ch == '[') {
          if (index == -1) index = i
          cnt += 1
        }
        else if (ch == ']') cnt -= 1
        else if (ch == ',') {
          if (cnt == 0) {
            result.add(deserialize(s.substring(index, i)))
            index = -1
          }
        }
        else if (index == -1) index = i
      }
      if (index != -1) result.add(deserialize(s.substring(index, s.length - 1)))
      result
    }
  }
}
