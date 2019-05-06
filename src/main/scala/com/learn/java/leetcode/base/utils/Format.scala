package com.learn.java.leetcode.base.utils

object Format {
  /**
    * 格式化对象为字符串
    *
    * @param obj
    * @return
    */
  def format(obj: Any): String = {
    val stringBuffer = new StringBuffer
    format(obj, stringBuffer)
    val testInputResult = stringBuffer.toString
    testInputResult
  }

  private def format(obj: Any, stringBuffer: StringBuffer): Unit = {
    if (obj == null) {
      stringBuffer.append("null")
      return
    } else if (obj.isInstanceOf[Integer] || obj.isInstanceOf[Long] || obj.isInstanceOf[Double] || obj.isInstanceOf[Float] || obj.isInstanceOf[Boolean]) {
      stringBuffer.append(obj.toString)
    }
    else if (obj.isInstanceOf[String]) {
      stringBuffer.append("\"" + StringUtil.changeStr(obj.toString()) + "\"")
    } else if (obj.getClass.isArray) {
      val className: String = obj.getClass().getName();
      if (className.equals("[I")) {
        format(obj.asInstanceOf[Array[Int]], stringBuffer)
      } else if (className.equals("[[I")) {
        format(obj.asInstanceOf[Array[Array[Int]]], stringBuffer);
      } else if (className.equals("[C")) {
        format(obj.asInstanceOf[Array[Char]], stringBuffer);
      } else {
        format(obj.asInstanceOf[Array[Any]], stringBuffer);
      }
    }
  }


  private def format(array: Array[Int], stringBuffer: StringBuffer): Unit = {
    if (array == null) {
      stringBuffer.append("null")
      return
    }
    stringBuffer.append("[")
    var i = 0
    while (i < array.length) {
      val dataObj = array(i)
      if (dataObj == null) stringBuffer.append("null")
      else {
        val data = dataObj.toString
        stringBuffer.append(data)
      }
      if (i < array.length - 1) stringBuffer.append(',')
      i += 1;
    }
    stringBuffer.append("]")
  }

  private def format(matrix: Array[Array[Int]], stringBuffer: StringBuffer): Unit = {
    if (matrix == null) return
    val row = matrix.length
    val cow = matrix(0).length
    stringBuffer.append("[")
    var i = 0
    while (i < row) {
      stringBuffer.append("[")
      var j = 0
      while (j < cow) {
        stringBuffer.append(matrix(i)(j))
        if (j < cow - 1) stringBuffer.append(',')
        j += 1
      }
      stringBuffer.append("]")
      if (i < row - 1) stringBuffer.append(',')
      i += 1
    }
    stringBuffer.append("]")
  }

  private def format(array: Array[Char], stringBuffer: StringBuffer): Unit = {
    if (array == null) {
      stringBuffer.append("null")
      return
    }
    stringBuffer.append("[")
    var i = 0
    while (i < array.length) {
      val dataObj = array(i)
      if (dataObj == null) {
        stringBuffer.append("null")
      }
      else {
        val data = dataObj.toString
        stringBuffer.append("\"").append(data).append("\"")
      }
      if (i < array.length - 1) {
        stringBuffer.append(',')
      }
      i += 1
    }
    stringBuffer.append("]")
  }

  private def format(array: Array[Any], stringBuffer: StringBuffer): Unit = {
    if (array == null) {
      stringBuffer.append("null")
      return
    }
    stringBuffer.append("[")
    var i = 0
    while (i < array.length) {
      val dataObj = array(i)
      if (dataObj == null) {
        stringBuffer.append("null")
      }
      else {
        val data = dataObj.toString
        if (StringUtil.judgeNumber(data)) stringBuffer.append(data)
        else stringBuffer.append("\"").append(StringUtil.changeStr(data)).append("\"")
      }
      if (i < array.length - 1) {
        stringBuffer.append(',')
      }
      i += 1
    }
    stringBuffer.append("]")
  }

}
