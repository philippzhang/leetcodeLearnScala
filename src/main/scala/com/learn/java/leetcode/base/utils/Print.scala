package com.learn.java.leetcode.base.utils

object Print {
  def print(obj: Any): Unit = {
    print(obj,null)
  }

  private def print(obj: Any,ext:String): Unit = {
    if (obj == null) {
      println("null")
      return
    }
    if (obj.isInstanceOf[Integer] || obj.isInstanceOf[String] || obj.isInstanceOf[Long] || obj.isInstanceOf[Double] || obj.isInstanceOf[Float] || obj.isInstanceOf[Boolean]) {
      println(obj)
    }

  }
}
