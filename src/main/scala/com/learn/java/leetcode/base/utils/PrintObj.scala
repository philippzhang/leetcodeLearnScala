package com.learn.java.leetcode.base.utils

import com.learn.java.leetcode.base.structure.ListNode

object PrintObj {
  def printObj(obj: Any): Unit = {
    printObj(obj, null)
  }

  private def printObj(obj: Any, ext: String): Unit = {
    if (obj == null) {
      println("null")
      return
    }
    if (obj.isInstanceOf[Integer] || obj.isInstanceOf[String] || obj.isInstanceOf[Long] || obj.isInstanceOf[Double] || obj.isInstanceOf[Float] || obj.isInstanceOf[Boolean]) {
      println(obj)
    } else if (obj.getClass.isArray) {
      val className = obj.getClass.getName
      if (className.equals("[I")) {
        printObj(obj.asInstanceOf[Array[Int]])
      } else if (className.equals("[[I")) {
        printObj(obj.asInstanceOf[Array[Array[Int]]])
      } else if (className.equals("[C")) {
        printObj(obj.asInstanceOf[Array[Char]])
      } else if (className.equals("[Lcom.learn.java.leetcode.base.structure.ListNode;")){
        printObj(obj.asInstanceOf[Array[ListNode]])
      } else {
        printObj(obj.asInstanceOf[Array[Any]])
      }
    } else if (obj.isInstanceOf[List[_]]) {
      val results: List[_] = obj.asInstanceOf[List[_]]
      print("[")
      var i: Int = 0
      while (i < results.size) {
        val item: Any = results(i)
        if (item == null) {
          print("null")
          if (i < results.size - 1) {
            print(',')
          }
        }
        if (item.isInstanceOf[Integer] || item.isInstanceOf[String] || item.isInstanceOf[Long] || item.isInstanceOf[Double] || item.isInstanceOf[Float] || item.isInstanceOf[Boolean]) {
          print(item)
          if (i < results.size - 1) {
            print(',')
          }
        }
        else {
          if (item.isInstanceOf[List[_]]) {
            if (i == 0) {
              println()
            }
            if (i < results.size - 1) {
              printObj(item, ",")
            }
            else {
              printObj(item, null)
            }
          }
        }
        i += 1
      }
      print("]")
      if (ext != null) {
        print(ext)
      }
      println()
    } else if (obj.isInstanceOf[ListNode]) {
      val listNode: ListNode = obj.asInstanceOf[ListNode]
      val str = new StringBuilder("[" + String.valueOf(listNode.x))
      var p: ListNode = listNode.next
      while (p != null) {
        str.append(",").append(String.valueOf(p.x))
        p = p.next
      }
      print(str.append("]"))
      if (ext != null) {
        print(ext)
      }
      println()
    }
  }

  /**
    * 打印数组
    *
    * @param array
    */
  private def printObj(array: Array[Int]): Unit = {
    if (array == null) return
    print("[")
    var i = 0
    while (i < array.length) {
      print(array(i))
      if (i < array.length - 1) {
        print(',')
      }
      i += 1
    }
    print("]")
    println()
  }

  /**
    * 打印矩阵
    *
    * @param matrix
    */
  def printObj(matrix: Array[Array[Int]]): Unit = {
    if (matrix == null) return
    val row = matrix.length
    val cow = matrix(0).length
    print("[")
    var i = 0
    while (i < row) {
      if (i == 0) println()
      print("[")
      var j = 0
      while (j < cow) {
        print(matrix(i)(j))
        if (j < cow - 1) print(',')
        j += 1
      }
      print("]")
      if (i < row - 1) print(',')
      println()
      i += 1;
    }
    print("]")
    println()
  }

  def printObj(array: Array[Char]): Unit = {
    if (array == null) {
      return
    }
    print("[")
    var i = 0
    while (i < array.length) {
      print("\"" + array(i) + "\"")
      if (i < array.length - 1) {
        print(',')
      }
      i += 1
    }
    print("]")
    println()
  }

  def printObj(array: Array[ListNode]): Unit = {
    if (array == null) {
      return
    }
    println("[")
    var i = 0
    while (i < array.length) {
      if (i < array.length - 1) {
        printObj(array(i), ",")
      }
      else printObj(array(i))
        i += 1
    }
    print("]")
    println()
  }

  def printObj(array: Array[Any]): Unit = {
    if (array == null) {
      return
    }
    print("[")
    var i = 0
    while (i < array.length) {
      val dataObj = array(i)
      if (dataObj == null) {
        print("null")
      } else {
        val data = dataObj.toString
        if (StringUtil.judgeNumber(data)) {
          print(data)
        } else {
          print("\"" + StringUtil.changeStr(data) + "\"")
        }
      }
      if (i < array.length - 1) {
        print(',')
      }
      i += 1
    }
    print("]")
    println()
  }
}
