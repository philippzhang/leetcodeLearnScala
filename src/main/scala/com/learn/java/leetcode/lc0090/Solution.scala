package com.learn.java.leetcode.lc0090

object Solution {
  def subsetsWithDup(nums: Array[Int]): List[List[Int]] = {
    scala.util.Sorting.quickSort(nums)
    helper(nums, Nil, List(), 0)
  }

  def helper(nums: Array[Int], rst: List[List[Int]], path: List[Int], idx: Int): List[List[Int]] = {
    var nrst: List[List[Int]] = rst:+path
    for (i <- idx until nums.length) {
      if (i == idx || nums(i) != nums(i-1)) {
        nrst = helper(nums, nrst, path:+nums(i), i+1)
      }
    }
    nrst
  }
}
