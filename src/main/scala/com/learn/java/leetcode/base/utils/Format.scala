package com.learn.java.leetcode.base.utils

import com.learn.java.leetcode.base.structure.{ListNode, TreeNode}

import scala.collection.mutable.{ListBuffer, Queue, Stack}
import scala.util.control.Breaks

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
    } else if (obj.isInstanceOf[Int] || obj.isInstanceOf[Long] || obj.isInstanceOf[Double] || obj.isInstanceOf[Float] || obj.isInstanceOf[Boolean]) {
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
      } else if (className.equals("[Lcom.learn.java.leetcode.base.structure.ListNode;")) {
        format(obj.asInstanceOf[Array[ListNode]], stringBuffer)
      } else {
        format(obj.asInstanceOf[Array[Any]], stringBuffer)
      }
    } else if (obj.isInstanceOf[List[_]]) {
      val results: List[_] = obj.asInstanceOf[List[_]]
      stringBuffer.append("[")
      var i: Int = 0
      while (i < results.size) {
        val item: Any = results(i)
        if (item == null) {
          stringBuffer.append("null")
        }
        if (item.isInstanceOf[Int] ||  item.isInstanceOf[Long] || item.isInstanceOf[Double] || item.isInstanceOf[Float] || item.isInstanceOf[Boolean]) {
          stringBuffer.append(item)
        }else if(item.isInstanceOf[String] ){
          stringBuffer.append("\"" + StringUtil.changeStr(item.toString) + "\"")
        }
        else {
          if (item.isInstanceOf[List[_]]) {
            format(item, stringBuffer)
          }
        }
        if (i < results.size - 1) {
          stringBuffer.append(',')
        }
        i += 1
      }
      stringBuffer.append("]")
    } else if (obj.isInstanceOf[ListBuffer[_]]) {
      val results: ListBuffer[_] = obj.asInstanceOf[ListBuffer[_]]
      stringBuffer.append("[")
      var i: Int = 0
      while (i < results.size) {
        val item: Any = results(i)
        if (item == null) {
          stringBuffer.append("null")
        }
        if (item.isInstanceOf[Int] || item.isInstanceOf[String] || item.isInstanceOf[Long] || item.isInstanceOf[Double] || item.isInstanceOf[Float] || item.isInstanceOf[Boolean]) {
          stringBuffer.append(item)
        }
        else {
          if (item.isInstanceOf[ListBuffer[_]]) {
            format(item, stringBuffer)
          }
        }
        if (i < results.size - 1) {
          stringBuffer.append(',')
        }
        i += 1
      }
      stringBuffer.append("]")
    } else if (obj.isInstanceOf[List[_]]) {
      val results: List[_] = obj.asInstanceOf[List[_]]
      stringBuffer.append("[")
      var i: Int = 0
      while (i < results.size) {
        val item: Any = results(i)
        if (item == null) {
          stringBuffer.append("null")
        }
        if (item.isInstanceOf[Int] || item.isInstanceOf[String] || item.isInstanceOf[Long] || item.isInstanceOf[Double] || item.isInstanceOf[Float] || item.isInstanceOf[Boolean]) {
          stringBuffer.append(item)
        }
        else {
          if (item.isInstanceOf[List[_]]) {
            format(item, stringBuffer)
          }
        }
        if (i < results.size - 1) {
          stringBuffer.append(',')
        }
        i += 1
      }
      stringBuffer.append("]")
    }

    else if (obj.isInstanceOf[ListNode]) {
      val listNode: ListNode = obj.asInstanceOf[ListNode];
      stringBuffer.append("[" + String.valueOf(listNode.x))
      var p: ListNode = listNode.next;
      while (p != null) {
        stringBuffer.append(",").append(String.valueOf(p.x))
        p = p.next
      }
      stringBuffer.append("]")
    } else if (obj.isInstanceOf[TreeNode]) {
      format(obj.asInstanceOf[TreeNode], stringBuffer)
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

      val data = dataObj.toString
      stringBuffer.append(data)

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

      stringBuffer.append("null")


      val data = dataObj.toString
      stringBuffer.append("\"").append(data).append("\"")

      if (i < array.length - 1) {
        stringBuffer.append(',')
      }
      i += 1
    }
    stringBuffer.append("]")
  }

  private def format(array: Array[ListNode], stringBuffer: StringBuffer): Unit = {
    if (array == null) stringBuffer.append("[]")
    stringBuffer.append("[")
    var i = 0
    while (i < array.length) {
      val listNode = array(i)
      if (listNode == null) {
        stringBuffer.append("[]")
      } else format(listNode, stringBuffer)
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

  private def format(treeNode: TreeNode, stringBuffer: StringBuffer): Unit = {
    if (treeNode == null) {
      stringBuffer.append("null")
      return
    }
    stringBuffer.append("[")
    stringBuffer.append(levelOrderFormat(treeNode))
    stringBuffer.append("]")
  }

  private def levelOrderFormat(root: TreeNode): String = {
    var current: TreeNode = root
    val stringBuffer: StringBuffer = new StringBuffer
    if (current != null) {
      val queue: Queue[TreeNode] = Queue()
      val stack: Stack[TreeNode] = Stack()
      queue.enqueue(current)
      while (!queue.isEmpty) {
        current = queue.dequeue()
        if (current != null) {
          stack.push(current)
          if (current.left != null) queue.enqueue(current.left)
          else queue.enqueue(null)
          if (current.right != null) queue.enqueue(current.right)
          else queue.enqueue(null)
        }
        else stack.push(null)
      }
      val loop = Breaks
      loop.breakable {
        while (!stack.isEmpty) {
          if (stack.top == null) stack.pop
          else {
            loop.break
          }
        }
      }
      while (!stack.isEmpty) {
        val treeNode: TreeNode = stack.pop
        val item: String = if (treeNode != null) treeNode.value + "," else "null,"
        stringBuffer.insert(0, item)
      }
      if (stringBuffer.length > 0) stringBuffer.deleteCharAt(stringBuffer.length - 1)
    }
    stringBuffer.toString
  }

}
