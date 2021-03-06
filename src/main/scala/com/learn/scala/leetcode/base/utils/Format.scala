package com.learn.scala.leetcode.base.utils


import com.google.gson.{JsonArray, JsonElement, JsonObject}
import com.learn.java.leetcode.utils.UtilitysJava
import com.learn.scala.leetcode.base.structure.{ListNode, NestedInteger, Node, TreeNode}

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
    if (stringBuffer.length > 0 && stringBuffer.charAt(stringBuffer.length - 1) == ',') stringBuffer.deleteCharAt(stringBuffer.length - 1)
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
      } else if (className.equals("[D")) {
          format(obj.asInstanceOf[Array[Double]], stringBuffer)
      } else if (className.equals("[[D")) {
        format(obj.asInstanceOf[Array[Array[Double]]], stringBuffer);
      } else if (className.equals("[F")) {
        format(obj.asInstanceOf[Array[Float]], stringBuffer)
      } else if (className.equals("[[F")) {
        format(obj.asInstanceOf[Array[Array[Float]]], stringBuffer);
      } else if (className.equals("[B")) {
        format(obj.asInstanceOf[Array[Boolean]], stringBuffer)
      } else if (className.equals("[[B")) {
        format(obj.asInstanceOf[Array[Array[Boolean]]], stringBuffer);
      } else if (className.equals("[C")) {
        format(obj.asInstanceOf[Array[Char]], stringBuffer);
      } else if (className.equals("[[C")) {
          format(obj.asInstanceOf[Array[Array[Char]]], stringBuffer);
      } else if (className.equals("[Ljava.lang.String;")) {
        format(obj.asInstanceOf[Array[String]], stringBuffer);
      } else if (className.equals("[[Ljava.lang.String;")) {
        format(obj.asInstanceOf[Array[Array[String]]], stringBuffer);
      } else if (className.equals("[Lcom.learn.scala.leetcode.base.structure.ListNode;")) {
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
        } else if(item.isInstanceOf[String] ){
          stringBuffer.append("\"" + StringUtil.changeStr(item.toString) + "\"")
        } else if (item.isInstanceOf[List[_]]) {
          format(item, stringBuffer)
        } else  if (item.isInstanceOf[TreeNode]) {
          format(item.asInstanceOf[TreeNode], stringBuffer)
        } else if (item.isInstanceOf[Node]) {
          format(item.asInstanceOf[Node], stringBuffer)
        } else if (item != null) {
          throw new RuntimeException("未定义的List泛型，转换失败!")
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
        else if (item.isInstanceOf[ListBuffer[_]]) {
            format(item, stringBuffer)
        }else if (item != null && item.getClass.isArray) {
          val className: String = item.getClass().getName();
          if (className.equals("[I")) {
            format(item.asInstanceOf[Array[Int]], stringBuffer)
          } else if (className.equals("[[I")) {
            format(item.asInstanceOf[Array[Array[Int]]], stringBuffer);
          } else if (className.equals("[D")) {
            format(item.asInstanceOf[Array[Double]], stringBuffer)
          } else if (className.equals("[[D")) {
            format(item.asInstanceOf[Array[Array[Double]]], stringBuffer);
          } else if (className.equals("[F")) {
            format(item.asInstanceOf[Array[Float]], stringBuffer)
          } else if (className.equals("[[F")) {
            format(item.asInstanceOf[Array[Array[Float]]], stringBuffer);
          } else if (className.equals("[B")) {
            format(item.asInstanceOf[Array[Boolean]], stringBuffer)
          } else if (className.equals("[[B")) {
            format(item.asInstanceOf[Array[Array[Boolean]]], stringBuffer);
          } else if (className.equals("[C")) {
            format(item.asInstanceOf[Array[Char]], stringBuffer);
          } else if (className.equals("[[C")) {
            format(item.asInstanceOf[Array[Array[Char]]], stringBuffer);
          } else if (className.equals("[Ljava.lang.String;")) {
            format(item.asInstanceOf[Array[String]], stringBuffer);
          } else if (className.equals("[[Ljava.lang.String;")) {
            format(item.asInstanceOf[Array[Array[String]]], stringBuffer);
          } else {
            format(item.asInstanceOf[Array[Any]], stringBuffer)
          }
        }else if (item != null) {
          throw new RuntimeException("未定义的ListBuffer泛型，转换失败!")
        }
        if (i < results.size - 1) {
          stringBuffer.append(',')
        }
        i += 1
      }
      stringBuffer.append("]")
    } /*else if (obj.isInstanceOf[List[_]]) {
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
    }*/

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
    } else if (obj.isInstanceOf[Node]) {
      format(obj.asInstanceOf[Node], stringBuffer)
    } else if (obj.isInstanceOf[NestedInteger]) {
      format(obj.asInstanceOf[NestedInteger], stringBuffer)
    } else{
      throw new RuntimeException("未定义的类型，转换失败!")
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

  private def format(array: Array[Double], stringBuffer: StringBuffer): Unit = {
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


  private def format(array: Array[Float], stringBuffer: StringBuffer): Unit = {
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


  private def format(array: Array[Boolean], stringBuffer: StringBuffer): Unit = {
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

  private def format(matrix: Array[Array[Double]], stringBuffer: StringBuffer): Unit = {
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

  private def format(matrix: Array[Array[Float]], stringBuffer: StringBuffer): Unit = {
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


  private def format(matrix: Array[Array[Boolean]], stringBuffer: StringBuffer): Unit = {
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


  private def format(matrix: Array[Array[Char]], stringBuffer: StringBuffer): Unit = {
    if (matrix == null) return
    val row = matrix.length
    val cow = matrix(0).length
    stringBuffer.append("[")
    var i = 0
    while (i < row) {
      stringBuffer.append("[")
      var j = 0
      while (j < cow) {
        stringBuffer.append("\"").append(matrix(i)(j)).append("\"")
        if (j < cow - 1) stringBuffer.append(',')
        j += 1
      }
      stringBuffer.append("]")
      if (i < row - 1) stringBuffer.append(',')
      i += 1
    }
    stringBuffer.append("]")
  }

  private def format(array: Array[String], stringBuffer: StringBuffer): Unit = {
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
      stringBuffer.append("\"").append(StringUtil.changeStr(data)).append("\"")

      if (i < array.length - 1) {
        stringBuffer.append(',')
      }
      i += 1
    }
    stringBuffer.append("]")
  }

  private def format(matrix: Array[Array[String]], stringBuffer: StringBuffer): Unit = {
    if (matrix == null) return
    val row = matrix.length
    val cow = matrix(0).length
    stringBuffer.append("[")
    var i = 0
    while (i < row) {
      stringBuffer.append("[")
      var j = 0
      while (j < cow) {
        stringBuffer.append("\"").append(matrix(i)(j)).append("\"")
        if (j < cow - 1) stringBuffer.append(',')
        j += 1
      }
      stringBuffer.append("]")
      if (i < row - 1) stringBuffer.append(',')
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

  /**
    * 格式化TreeNode
    * @param treeNode
    * @param stringBuffer
    */
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

  /**
    * 格式化Node
    * @param node
    * @param stringBuffer
    */
  private def format(node: Node, stringBuffer: StringBuffer): Unit = {
    if (node == null) {
      stringBuffer.append("null")
      return
    }
    val jsonObject: JsonObject = node2JsonObject(node)
    nodeAddId(jsonObject)
    stringBuffer.append(UtilitysJava.sortJsonObject(jsonObject.toString))
  }


  private def node2JsonObject(cur: Node): JsonObject = {
    if (cur == null) return null

    val jsonObject: JsonObject = new JsonObject
    val jsonArray: JsonArray = new JsonArray
    if (cur.children != null) {
      val child: List[Node] = cur.children
      if (child != null && child.size > 0) {
        var i: Int = 0
        while (i < child.size) {
          val temp: Node = child(i)
          jsonArray.add(node2JsonObject(temp))
            i += 1
        }
      }
      jsonObject.add("children", jsonArray)
    }
    else jsonObject.add("children", jsonArray)
    jsonObject.addProperty("val", cur.value)
    jsonObject
  }

  private def nodeAddId(rootObject: JsonObject): Unit = {
    var index: Int = 1
    val queue: Queue[JsonObject] = new Queue()
    queue.enqueue(rootObject)
    while (!queue.isEmpty) {
      val jsonObject: JsonObject = queue.dequeue()
      jsonObject.addProperty("$id", String.valueOf({
        index += 1
      }))
      val jsonArray: JsonArray = jsonObject.getAsJsonArray("children")
      if (jsonArray != null && jsonArray.size > 0) {
        var i: Int = 0
        while (i < jsonArray.size) {
          val childJsonElement: JsonElement = jsonArray.get(i)
          val childJsonObject: JsonObject = if (childJsonElement != null && !childJsonElement.isJsonNull) childJsonElement.getAsJsonObject
          else null
          if (childJsonObject != null) queue.enqueue(childJsonObject)
            i += 1
        }
      }
    }
  }


  private def format(nestedInteger: NestedInteger, stringBuffer: StringBuffer): Unit = {
    if (nestedInteger.isInteger) stringBuffer.append(nestedInteger.getInteger).append(",")
    else {
      stringBuffer.append("[")
      val list: List[NestedInteger] = nestedInteger.getList
      if (list != null && list.size > 0) {
        var i: Int = 0
        while (i < list.size) {
          format(list(i), stringBuffer)
            i += 1
        }
      }
      if (stringBuffer.length > 0 && stringBuffer.charAt(stringBuffer.length - 1) == ',') stringBuffer.deleteCharAt(stringBuffer.length - 1)
      stringBuffer.append("]")
    }
  }

}
