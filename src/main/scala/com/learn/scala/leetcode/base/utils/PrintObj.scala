package com.learn.scala.leetcode.base.utils

import com.learn.scala.leetcode.base.structure.{ListNode, Node, TreeNode}

import scala.collection.mutable.{ListBuffer, Stack}

object PrintObj {
  def printObj(obj: Any): Unit = {
    printObj(obj, null)
  }

  private def printObj(obj: Any, ext: String): Unit = {
    if (obj == null) {
      println("null")
      return
    }
    if (obj.isInstanceOf[Int] || obj.isInstanceOf[String] || obj.isInstanceOf[Long] || obj.isInstanceOf[Double] || obj.isInstanceOf[Float] || obj.isInstanceOf[Boolean]) {
      println(obj)
    } else if (obj.getClass.isArray) {
      val className = obj.getClass.getName
      if (className.equals("[I")) {
        printObj(obj.asInstanceOf[Array[Int]])
      } else if (className.equals("[[I")) {
        printObj(obj.asInstanceOf[Array[Array[Int]]])
      } else if (className.equals("[D")) {
        printObj(obj.asInstanceOf[Array[Double]])
      } else if (className.equals("[[D")) {
        printObj(obj.asInstanceOf[Array[Array[Double]]])
      } else if (className.equals("[F")) {
        printObj(obj.asInstanceOf[Array[Float]])
      } else if (className.equals("[[F")) {
        printObj(obj.asInstanceOf[Array[Array[Float]]])
      } else if (className.equals("[B")) {
        printObj(obj.asInstanceOf[Array[Float]])
      } else if (className.equals("[[B")) {
        printObj(obj.asInstanceOf[Array[Array[Float]]])
      } else if (className.equals("[C")) {
        printObj(obj.asInstanceOf[Array[Char]])
      } else if (className.equals("[[C")) {
        printObj(obj.asInstanceOf[Array[Array[Char]]])
      } else if (className.equals("[Ljava.lang.String;")) {
        printObj(obj.asInstanceOf[Array[String]])
      } else if (className.equals("[[Ljava.lang.String;")) {
        printObj(obj.asInstanceOf[Array[Array[String]]])
      } else if (className.equals("[Lcom.learn.scala.leetcode.base.structure.ListNode;")) {
        printObj(obj.asInstanceOf[Array[ListNode]])
      } else {
        printObj(obj.asInstanceOf[Array[Any]])
      }
    } /*else if (obj.isInstanceOf[List[_]]) {
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
        if (item.isInstanceOf[Int] || item.isInstanceOf[String] || item.isInstanceOf[Long] || item.isInstanceOf[Double] || item.isInstanceOf[Float] || item.isInstanceOf[Boolean]) {
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
    } */else if (obj.isInstanceOf[ListBuffer[_]]) {
      val results: ListBuffer[_] = obj.asInstanceOf[ListBuffer[_]]
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
        if (item.isInstanceOf[Int] || item.isInstanceOf[String] || item.isInstanceOf[Long] || item.isInstanceOf[Double] || item.isInstanceOf[Float] || item.isInstanceOf[Boolean]) {
          print(item)
          if (i < results.size - 1) {
            print(',')
          }
        }
        else if (item.isInstanceOf[ListBuffer[_]]) {
            if (i == 0) {
              println()
            }
            if (i < results.size - 1) {
              printObj(item, ",")
            }
            else {
              printObj(item, null)
            }
        }else if (item != null) {
          throw new RuntimeException("未定义的ListBuffer泛型，打印失败!")
        }

        i += 1
      }
      print("]")
      if (ext != null) {
        print(ext)
      }
      println()
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
        if (item.isInstanceOf[Int] || item.isInstanceOf[String] || item.isInstanceOf[Long] || item.isInstanceOf[Double] || item.isInstanceOf[Float] || item.isInstanceOf[Boolean]) {
          print(item)
          if (i < results.size - 1) {
            print(',')
          }
        } else if (item.isInstanceOf[List[_]]) {
            if (i == 0) {
              println()
            }
            if (i < results.size - 1) {
              printObj(item, ",")
            }
            else {
              printObj(item, null)
            }
        } else if (item.isInstanceOf[TreeNode]) {
          if (i == 0) System.out.println()
          print(item.asInstanceOf[TreeNode])
          System.out.println()
        } else if (item.isInstanceOf[Node]) {
          if (i == 0) System.out.println()
          print(item.asInstanceOf[Node])
          System.out.println()
        } else if (item != null) {
          throw new RuntimeException("未定义的List泛型，打印失败!")
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
    } else if (obj.isInstanceOf[TreeNode]) {
      printObj(obj.asInstanceOf[TreeNode])
    } else if (obj.isInstanceOf[Node]) {
      printObj(obj.asInstanceOf[Node]);
    } else {
      throw new RuntimeException("未定义的类型，打印失败!")
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
    * 打印数组
    *
    * @param array
    */
  private def printObj(array: Array[Double]): Unit = {
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
    * 打印数组
    *
    * @param array
    */
  private def printObj(array: Array[Float]): Unit = {
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

  private def printObj(array: Array[Boolean]): Unit = {
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
  private def printObj(matrix: Array[Array[Int]]): Unit = {
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

  /**
    * 打印矩阵
    *
    * @param matrix
    */
  private def printObj(matrix: Array[Array[Double]]): Unit = {
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

  /**
    * 打印矩阵
    *
    * @param matrix
    */
  private def printObj(matrix: Array[Array[Float]]): Unit = {
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

  private def printObj(matrix: Array[Array[Boolean]]): Unit = {
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

  private def printObj(array: Array[Char]): Unit = {
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

  /**
    * 打印矩阵
    *
    * @param matrix
    */
  private def printObj(matrix: Array[Array[Char]]): Unit = {
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
        print("\"" + matrix(i)(j) + "\"")
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

  private def printObj(array: Array[String]): Unit = {
    if (array == null) {
      return
    }
    print("[")
    var i = 0
    while (i < array.length) {
      val data = array(i)
      print("\"" + StringUtil.changeStr(data) + "\"")
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
  private def printObj(matrix: Array[Array[String]]): Unit = {
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
        print("\"" + matrix(i)(j) + "\"")
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

  private def printObj(array: Array[ListNode]): Unit = {
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

  private def printObj(array: Array[Any]): Unit = {
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


  /**
    * 竖向打印二叉树
    *
    * @param root 二叉树根节点
    */
  private def printObj(root: TreeNode): Unit = {
    if (root == null) return
    val globalStack: Stack[TreeNode] = Stack()
    globalStack.push(root)
    val depth: Int = getDepth(root)
    var nBlank: Int = Math.pow(2, depth + 1).toInt
    val ndot: Int = nBlank * 2
    var isRowEmpty: Boolean = false
    var i: Int = 0
    while (i < ndot) {
      print('.')
      i += 1
    }
    println()
    while (!isRowEmpty) {
      val localStack: Stack[TreeNode] = Stack()
      isRowEmpty = true
      var j: Int = 0
      while (j < nBlank) {
        print(' ')
        j += 1
      }
      while (!globalStack.isEmpty) { //里面的while循环用于查看全局的栈是否为空
        val temp: TreeNode = globalStack.pop
        if (temp != null) {
          print(temp.value)
          System.out.print(' ')
          localStack.push(temp.left)
          localStack.push(temp.right)
          //如果当前的节点下面还有子节点，则必须要进行下一层的循环
          if (temp.left != null || temp.right != null) isRowEmpty = false
        }
        else { //如果全局的栈则不为空
          print("# ")
          localStack.push(null)
          localStack.push(null)
        }
        //打印一些空格
        var j: Int = 0
        while (j < nBlank * 2 - 2) {
          print(' ')
          j += 1
        }
      } //while end}
      println()
      nBlank /= 2
      //这个while循环用来判断，local栈是否为空,不为空的话，则取出来放入全局栈中
      while (!localStack.isEmpty) {
        globalStack.push(localStack.pop)
      }
    } //大while循环结束之后，输出换行}
    i = 0
    while (i < ndot) {
      print('.')
      i += 1
    }
    println()
  }

  /**
    * 二叉树的高度
    *
    * @param root
    * @return
    */
  private def getDepth(root: TreeNode): Int = {
    if (root != null) {
      val lDepth = getDepth(root.left)
      val rDepth = getDepth(root.right)
      (if (lDepth > rDepth) lDepth else rDepth) + 1
    } else {
      return 0
    }
  }

  /**
    * N叉树的高度
    *
    * @param root
    * @return
    */
  private def getDepth(root: Node): Int = {
    if (root != null) {
      var max = 0
      if (root.children != null && root.children.size > 0) {
        var i = 0
        while (i < root.children.size) {
          val depth = getDepth(root.children(i))
          max = Math.max(max, depth)
          i += 1
        }
      }
      max + 1
    }
    else 0
  }

  /**
    * 竖向打印N叉树
    *
    * @param root 二叉树根节点
    */
  private def printObj(root: Node): Unit = {
    if (root == null) return
    val globalStack: Stack[Node] = Stack()
    globalStack.push(root)
    val depth = getDepth(root)
    var nBlank = Math.pow(2, depth + 1).toInt
    val ndot = nBlank * 2
    var isRowEmpty = false
    var i = 0
    while (i < ndot) {
      System.out.print('.')
      i += 1
    }
    System.out.println()
    while (isRowEmpty == false) {
      val localStack: Stack[Node] = Stack()
      isRowEmpty = true
      var j = 0
      while (j < nBlank) {
        System.out.print(' ')
        j += 1
      }
      while (!globalStack.isEmpty) { //里面的while循环用于查看全局的栈是否为空
        val temp = globalStack.pop
        if (temp != null) {
          System.out.print(temp.value)
          System.out.print(' ')
          if (temp.children != null && temp.children.size > 0) {
            var i = 0
            while (i < temp.children.size) {
              localStack.push(temp.children(i))
              if (temp.children(i) != null) { //如果当前的节点下面还有子节点，则必须要进行下一层的循环
                isRowEmpty = false
              }
              i += 1
            }
          }
        }
        else { //如果全局的栈则不为空
          System.out.print("# ")
          localStack.push(null)
          localStack.push(null)
        }
        //打印一些空格
        var j = 0
        while (j < nBlank * 2 - 2) {
          System.out.print(' ')
          j += 1
        }
      } //while end
      System.out.println()
      nBlank /= 2
      //这个while循环用来判断，local栈是否为空,不为空的话，则取出来放入全局栈中
      while (!localStack.isEmpty) {
        globalStack.push(localStack.pop)
      }
    } //大while循环结束之后，输出换行
    i = 0
    while (i < ndot) {
      System.out.print('.')
      i += 1
    }
    println()
  }

}
