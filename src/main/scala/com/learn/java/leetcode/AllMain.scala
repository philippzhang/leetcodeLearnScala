package com.learn.java.leetcode


import com.learn.java.leetcode.base.Utilitys
import com.learn.java.leetcode.base.utils.Format

import scala.collection.mutable.ListBuffer


object AllMain {
  def main(args: Array[String]): Unit = {
    val path: String = AllMain.getClass.getResource(".").getFile
    var packageList:ListBuffer[String] = Utilitys.getAllLCFileName(path)
    packageList = packageList.sorted
    var successCount: Int = 0
    var failCount: Int = 0
    val failList: ListBuffer[String] = ListBuffer()
    val startTime: Long = System.currentTimeMillis
    var i: Int = 0
    while (i < packageList.size) {
      val packageName: String = packageList(i).toString
      val flag: Boolean = Utilitys.funcInvoke(AllMain.getClass.getPackage.getName + "." + packageName + ".Main")
      if (flag) successCount += 1
      else {
        failCount += 1
        failList += packageName.substring(2)
      }
        i += 1
    }
    val endTime: Long = System.currentTimeMillis
    println("总题数:" + packageList.size + ", 正确数:" + successCount + ", 错误数:" + failCount)
    if (failList.size > 0) {
      println("错误题目: " + Format.format(failList))
    }
    println("计算时长: " + (endTime - startTime) + "ms")


  }
}
