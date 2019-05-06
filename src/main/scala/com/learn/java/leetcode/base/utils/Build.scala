package com.learn.java.leetcode.base.utils

object Build {
  /**
    * 字符串构建数组
    * 例如[1,2,3]
    *
    * @param data
    * @return
    */
  def buildArray(data: String): Array[Int] = {
    if (data == null || data.trim.length == 0 || data == "null" || data.indexOf("[") < 0) return null
    var ret = data.trim
    ret = ret.replaceAll("\\[", "").replaceAll("\\]", "")
    val arr = ret.split(",", -1)
    val length = arr.length
    val results = new Array[Int](length)
    var i = 0
    while (i < length) {
      results(i) = arr(i).toInt
      i += 1
    }
    results
  }
}
