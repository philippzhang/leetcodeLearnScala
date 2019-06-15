package com.learn.scala.leetcode.base.utils

object StringUtil {

  /**
    * 统计字符串中出现的某个字符个数
    *
    * @param str
    * @param s
    * @return
    */
  def countString(str: String, s: Char): Int = {
    var count = 0
    var ret = str
    while (ret.indexOf(s) != -1) {
      ret = ret.substring(ret.indexOf(s) + 1, ret.length)
      count += 1
    }
    count
  }

  /**
    * 判读字符串是否数字
    *
    * @param str
    * @return
    */
  def judgeNumber(str: String): Boolean = str.matches("[-+]?[0-9]+.*[0-9]*")

  def judgeINumber(str: String): Boolean = str.matches("^I[0-9]=.+$")

  /**
    * 处理字符串
    *
    * @param temp
    * @return
    */
  def changeStr(temp: String): String = {
    if (temp == null || temp.equals("null")) return null
    var ret: String = temp
    if (ret.startsWith("\"")) {
      ret = ret.substring(1)
    }
    if (ret.endsWith("\"")) {
      ret = ret.substring(0, ret.length - 1)
    }
    ret
  }

  def IsEqual(a: Double, b: Double): Boolean = Math.abs(a - b) < 0.000001

  def IsEqual(a: Float, b: Float): Boolean = Math.abs(a - b) < 0.000001
}