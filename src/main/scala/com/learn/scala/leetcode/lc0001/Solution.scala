package com.learn.scala.leetcode.lc0001

import scala.collection.mutable

object Solution {
  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    val a: mutable.HashMap[Int, Int] = mutable.HashMap()
    for (i <- nums.indices) {
      val x = target - nums(i)
      if (a.contains(x)) {
        return Array(a(x), i)
      }
      a(nums(i)) = i
    }
    Array(-1, -1)
  }
}
