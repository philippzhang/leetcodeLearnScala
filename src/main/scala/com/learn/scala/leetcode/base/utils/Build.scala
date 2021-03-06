package com.learn.scala.leetcode.base.utils

import com.google.gson.{JsonArray, JsonElement, JsonObject, JsonParser}
import com.learn.scala.leetcode.base.structure.{ListNode, NestedInteger, Node, TreeNode}

import scala.Array._
import scala.collection.mutable.{ListBuffer, Queue}

object Build {
  /**
    * 字符串构建数组
    * 例如[1,2,3]
    *
    * @param data
    * @return
    */
  def buildArray(data: String): Array[Int] = {
    if (data == null || data.trim.length == 0 || data.equals("null") || data.indexOf("[") < 0) return null
    var ret = data.trim
    if (ret.equals("[]")){
      return new Array[Int](0)
    }
    ret = ret.replaceAll("\\[", "").replaceAll("\\]", "")
    val arr = ret.split(",", -1)
    val length = arr.length
    val results = new Array[Int](length)
    var i = 0
    while (i < length) {
      results(i) = arr(i).trim.toInt
      i += 1
    }
    results
  }

  def buildArray(list: ListBuffer[_]): Array[Int] = {
    if (list == null || list.size == 0) return null
    val results = new Array[Int](list.size)
    var i = 0
    while (i < list.size) {
      results(i) = list(i).toString.toInt
      i += 1
    }
    results
  }




  def buildArrayBoolean(data: String): Array[Boolean] = {
    if (data == null || data.trim.length == 0 || data.equals("null") || data.indexOf("[") < 0) return null
    var ret = data.trim
    if (ret.equals("[]")){
      return new Array[Boolean](0)
    }
    ret = ret.replaceAll("\\[", "").replaceAll("\\]", "")
    val arr = ret.split(",", -1)
    val length = arr.length
    val results = new Array[Boolean](length)
    var i = 0
    while (i < length) {
      results(i) = arr(i).trim.toBoolean
      i += 1
    }
    results
  }

  def buildArrayBoolean(list: ListBuffer[_]): Array[Boolean] = {
    if (list == null || list.size == 0) return null
    val results = new Array[Boolean](list.size)
    var i = 0
    while (i < list.size) {
      results(i) = list(i).toString.toBoolean
      i += 1
    }
    results
  }

  def buildArrayDouble(data: String): Array[Double] = {
    if (data == null || data.trim.length == 0 || data.equals("null") || data.indexOf("[") < 0) return null
    var ret = data.trim
    if (ret.equals("[]")){
      return new Array[Double](0)
    }
    ret = ret.replaceAll("\\[", "").replaceAll("\\]", "")
    val arr = ret.split(",", -1)
    val length = arr.length
    val results = new Array[Double](length)
    var i = 0
    while (i < length) {
      results(i) = arr(i).trim.toDouble
      i += 1
    }
    results
  }

  def buildArrayDouble(list: ListBuffer[_]): Array[Double] = {
    if (list == null || list.size == 0) return null
    val results = new Array[Double](list.size)
    var i = 0
    while (i < list.size) {
      results(i) = list(i).toString.toDouble
      i += 1
    }
    results
  }

  def buildArrayFloat(data: String): Array[Float] = {
    if (data == null || data.trim.length == 0 || data.equals("null") || data.indexOf("[") < 0) return null
    var ret = data.trim
    if (ret.equals("[]")){
      return new Array[Float](0)
    }
    ret = ret.replaceAll("\\[", "").replaceAll("\\]", "")
    val arr = ret.split(",", -1)
    val length = arr.length
    val results = new Array[Float](length)
    var i = 0
    while (i < length) {
      results(i) = arr(i).trim.toFloat
      i += 1
    }
    results
  }

  def buildArrayFloat(list: ListBuffer[_]): Array[Float] = {
    if (list == null || list.size == 0) return null
    val results = new Array[Float](list.size)
    var i = 0
    while (i < list.size) {
      results(i) = list(i).toString.toFloat
      i += 1
    }
    results
  }


  /**
    * 构建字符数组
    *
    * @param data
    * @return
    */
  def buildArrayChar(data: String): Array[Char] = {
    if (data == null || data.trim.length == 0 || data.equals("null") || data.indexOf("[") < 0) return null
    var ret = data.trim
    if (ret.equals("[]")){
      return new Array[Char](0)
    }
    ret = ret.replaceAll("\\[", "").replaceAll("\\]", "")
    val arr = ret.split(",", -1)
    val length = arr.length
    val results = new Array[Char](length)
    var i = 0
    while (i < length) {
      results(i) = StringUtil.changeStr(arr(i).trim).charAt(0)
      i += 1
    }
    results
  }

  def buildArrayChar(list: ListBuffer[_]): Array[Char] = {
    if (list == null || list.size == 0) return null
    val results = new Array[Char](list.size)
    var i = 0
    while (i < list.size) {
      results(i) = StringUtil.changeStr(list(i).toString).charAt(0)
      i += 1
    }
    results
  }

  /**
    * 构建二维数组
    * 例如[[1,2,3],[4,5,6],[7,8,9]]
    *
    * @param data
    * @return
    */
  def buildMatrix(data: String): Array[Array[Int]] = {
    if (data == null || data.trim.length == 0 || data.equals("null") || data.indexOf("[") < 0) return null
    if (data.equals("[]")){
      return ofDim[Int](0,0)
    }
    var ret = data.replaceAll(" ", "")
    ret = ret.substring(2, ret.length - 2)
    val arr = ret.split("],\\[", -1)
    val row = arr.length
    val cow = StringUtil.countString(arr(0), ',') + 1
    val results = ofDim[Int](row, cow)
    var i = 0
    while (i < row) {
      val arr2 = arr(i).split(",", -1)
      var j = 0
      while (j < cow) {
        results(i)(j) = arr2(j).trim.toInt
        j += 1
      }
      i += 1
    }
    results
  }

  def buildMatrix(list: ListBuffer[_]): Array[Array[Int]] = {
    if (list==null) return null
    if (list.size == 0) return ofDim[Int](0,0)
    if (list(0) == null || !list(0).isInstanceOf[ListBuffer[_]] || list(0).asInstanceOf[ListBuffer[_]].size == 0) return ofDim[Int](0,0)

    val row = list.size
    val cow = list(0).asInstanceOf[ListBuffer[_]].size
    val results = ofDim[Int](row, cow)

    var i = 0
    while (i < row) {
      val childList = list(i).asInstanceOf[ListBuffer[_]]
      var j = 0
      while (j < cow) {
        results(i)(j) = childList(j).toString.toInt;
        j += 1
      }
      i += 1
    }
    results
  }


  def buildMatrixBoolean(data: String): Array[Array[Boolean]] = {
    if (data == null || data.trim.length == 0 || data.equals("null") || data.indexOf("[") < 0) return null
    if (data.equals("[]")){
      return ofDim[Boolean](0,0)
    }
    var ret = data.replaceAll(" ", "")
    ret = ret.substring(2, ret.length - 2)
    val arr = ret.split("],\\[", -1)
    val row = arr.length
    val cow = StringUtil.countString(arr(0), ',') + 1
    val results = ofDim[Boolean](row, cow)
    var i = 0
    while (i < row) {
      val arr2 = arr(i).split(",", -1)
      var j = 0
      while (j < cow) {
        results(i)(j) = arr2(j).trim.toBoolean
        j += 1
      }
      i += 1
    }
    results
  }


  def buildMatrixBoolean(list: ListBuffer[_]): Array[Array[Boolean]] = {
    if (list==null) return null
    if (list.size == 0) return ofDim[Boolean](0,0)
    if (list(0) == null || !list(0).isInstanceOf[ListBuffer[_]] || list(0).asInstanceOf[ListBuffer[_]].size == 0) return ofDim[Boolean](0,0)

    val row = list.size
    val cow = list(0).asInstanceOf[ListBuffer[_]].size
    val results = ofDim[Boolean](row, cow)
    var i = 0
    while (i < row) {
      val childList = list(i).asInstanceOf[ListBuffer[_]]
      var j = 0
      while (j < cow) {
        results(i)(j) = childList(j).toString.toBoolean
        j += 1
      }
      i += 1
    }
    results
  }

  def buildMatrixDouble(data: String): Array[Array[Double]] = {
    if (data == null || data.trim.length == 0 || data.equals("null") || data.indexOf("[") < 0) return null
    if (data.equals("[]")){
      return ofDim[Double](0,0)
    }
    var ret = data.replaceAll(" ", "")
    ret = ret.substring(2, ret.length - 2)
    val arr = ret.split("],\\[", -1)
    val row = arr.length
    val cow = StringUtil.countString(arr(0), ',') + 1
    val results = ofDim[Double](row, cow)
    var i = 0
    while (i < row) {
      val arr2 = arr(i).split(",", -1)
      var j = 0
      while (j < cow) {
        results(i)(j) = arr2(j).trim.toDouble
        j += 1
      }
      i += 1
    }
    results
  }

  def buildMatrixDouble(list: ListBuffer[_]): Array[Array[Double]] = {
    if (list==null) return null
    if (list.size == 0) return ofDim[Double](0,0)
    if (list(0) == null || !list(0).isInstanceOf[ListBuffer[_]] || list(0).asInstanceOf[ListBuffer[_]].size == 0) return ofDim[Double](0,0)

    val row = list.size
    val cow = list(0).asInstanceOf[ListBuffer[_]].size
    val results = ofDim[Double](row, cow)
    var i = 0
    while (i < row) {
      val childList = list(i).asInstanceOf[ListBuffer[_]]
      var j = 0
      while (j < cow) {
        results(i)(j) = childList(j).toString.toDouble
        j += 1
      }
      i += 1
    }
    results
  }

  def buildMatrixFloat(data: String): Array[Array[Float]] = {
    if (data == null || data.trim.length == 0 || data.equals("null") || data.indexOf("[") < 0) return null
    if (data.equals("[]")){
      return ofDim[Float](0,0)
    }
    var ret = data.replaceAll(" ", "")
    ret = ret.substring(2, ret.length - 2)
    val arr = ret.split("],\\[", -1)
    val row = arr.length
    val cow = StringUtil.countString(arr(0), ',') + 1
    val results = ofDim[Float](row, cow)
    var i = 0
    while (i < row) {
      val arr2 = arr(i).split(",", -1)
      var j = 0
      while (j < cow) {
        results(i)(j) = arr2(j).trim.toFloat
        j += 1
      }
      i += 1
    }
    results
  }


  def buildMatrixFloat(list: ListBuffer[_]): Array[Array[Float]] = {
    if (list==null) return null
    if (list.size == 0) return ofDim[Float](0,0)
    if (list(0) == null || !list(0).isInstanceOf[ListBuffer[_]] || list(0).asInstanceOf[ListBuffer[_]].size == 0) return ofDim[Float](0,0)

    val row = list.size
    val cow = list(0).asInstanceOf[ListBuffer[_]].size
    val results = ofDim[Float](row, cow)
    var i = 0
    while (i < row) {
      val childList = list(i).asInstanceOf[ListBuffer[_]]
      var j = 0
      while (j < cow) {
        results(i)(j) = childList(j).toString.toFloat
        j += 1
      }
      i += 1
    }
    results
  }


  /**
    * 构建二维字符数组
    * 例如[["1","1","0","0","0"],["1","1","0","0","0"],["0","0","1","0","0"],["0","0","0","1","1"]]
    *
    * @param data
    * @return
    */
  def buildMatrixChar(data: String): Array[Array[Char]] = {
    if (data == null || data.trim.length == 0 || data.equals("null") || data.indexOf("[") < 0) return null
    if (data.equals("[]")){
      return ofDim[Char](0,0)
    }
    var ret = data.replaceAll(" ", "")
    ret = ret.substring(2, ret.length - 2)
    val arr = ret.split("],\\[", -1)
    val row = arr.length
    val cow = StringUtil.countString(arr(0), ',') + 1
    val results = ofDim[Char](row, cow)
    var i = 0
    while (i < row) {
      val arr2 = arr(i).split(",", -1)
      var j = 0
      while (j < cow) {
        results(i)(j) = StringUtil.changeStr(arr2(j).trim).charAt(0)
        j += 1
      }
      i += 1
    }
    results
  }


  def buildMatrixChar(list: ListBuffer[_]): Array[Array[Char]] = {
    if (list==null) return null
    if (list.size == 0) return ofDim[Char](0,0)
    if (list(0) == null || !list(0).isInstanceOf[ListBuffer[_]] || list(0).asInstanceOf[ListBuffer[_]].size == 0) return ofDim[Char](0,0)

    val row = list.size
    val cow = list(0).asInstanceOf[ListBuffer[_]].size
    val results = ofDim[Char](row, cow)
    var i = 0
    while (i < row) {
      val childList = list(i).asInstanceOf[ListBuffer[_]]
      var j = 0
      while (j < cow) {
        results(i)(j) = StringUtil.changeStr(childList(j).toString).charAt(0);
        j += 1
      }
      i += 1
    }
    results
  }


  /**
    * 构建二维字符串数组
    * 例如[["1","1","0","0","0"],["1","1","0","0","0"],["0","0","1","0","0"],["0","0","0","1","1"]]
    *
    * @param data
    * @return
    */
  def buildMatrixString(data: String): Array[Array[String]] = {
    if (data == null || data.trim.length == 0 || data.equals("null") || data.indexOf("[") < 0) return null
    if (data.equals("[]")){
      return ofDim[String](0,0)
    }
    var ret = data.replaceAll(" ", "")
    ret = ret.substring(2, ret.length - 2)
    val arr = ret.split("],\\[", -1)
    val row = arr.length
    val cow = StringUtil.countString(arr(0), ',') + 1
    val results = ofDim[String](row, cow)
    var i = 0
    while (i < row) {
      val arr2 = arr(i).split(",", -1)
      var j = 0
      while (j < cow) {
        results(i)(j) = StringUtil.changeStr(arr2(j).trim)
        j += 1
      }
      i += 1
    }
    results
  }

  def buildMatrixString(list: ListBuffer[_]): Array[Array[String]] = {
    if (list==null) return null
    if (list.size == 0) return ofDim[String](0,0)
    if (list(0) == null || !list(0).isInstanceOf[ListBuffer[_]] || list(0).asInstanceOf[ListBuffer[_]].size == 0) return ofDim[String](0,0)

    val row = list.size
    val cow = list(0).asInstanceOf[ListBuffer[_]].size
    val results = ofDim[String](row, cow)
    var i = 0
    while (i < row) {
      val childList = list(i).asInstanceOf[ListBuffer[_]]
      var j = 0
      while (j < cow) {
        results(i)(j) = StringUtil.changeStr(childList(j).toString.trim)
        j += 1
      }
      i += 1
    }
    results
  }

  /**
    * 字符串构建字符串数组
    * 例如["flower","flow","flight"]
    *
    * @param data
    * @return
    */
  def buildArrayString(data: String): Array[String] = {
    if (data == null || data.trim.length == 0 || data.equals("null") || data.indexOf("[") < 0) return null
    var ret = data.trim
    if (ret.equals("[]")){
      return new Array[String](0)
    }
    ret = ret.replaceAll("\\[", "").replaceAll("\\]", "")
    val arr = ret.split(",", -1)
    val length = arr.length
    val results = new Array[String](length)
    var i = 0
    while (i < length) {
      results(i) = StringUtil.changeStr(arr(i).trim)
      i += 1
    }
    results
  }

  def buildArrayString(list: ListBuffer[_]): Array[String] = {
    if (list == null || list.size == 0) return null
    val results = new Array[String](list.size)
    var i = 0
    while (i < list.size) {
      results(i) = StringUtil.changeStr(list(i).toString)
      i += 1
    }
    results
  }

  /**
    * 构建链表
    *
    * @param data
    * @return
    */
  def buildListNode(data: String): ListNode = {
    if (data == null || data.trim.length == 0 || data.equals("null") || data.equals("[]")) return null
    var ret = data.replaceAll(" ", "")
    ret = ret.substring(1, ret.length - 1)
    val split = ret.split(",", -1)
    val len = split.length
    val listNode = new Array[ListNode](len + 1)
    listNode(0) = new ListNode(split(0).toInt)
    var i = 1
    while (i < len) {
      listNode(i) = new ListNode(split(i).trim.toInt)
      listNode(i - 1).next = listNode(i)
      i += 1
    }
    listNode(0)
  }

  def buildListNode(list: ListBuffer[_]): ListNode = {
    if (list == null || list.size == 0) return null
    val len = list.size
    val listNode = new Array[ListNode](len + 1)
    listNode(0) = new ListNode(list(0).toString.toInt)
    var i: Int = 1
    while (i < len) {
      listNode(i) = new ListNode(list(i).toString.toInt)
      listNode(i - 1).next = listNode(i)
      i += 1
    }
    listNode(0)
  }

  /**
    * 构建链表数组
    *
    * @param data
    * @return
    */
  def buildListNodeArray(data: String): Array[ListNode] = {
    if (data == null || data.trim.length == 0 || data.equals("null") || data.equals("[]")) return null
    var ret = data.replaceAll(" ", "")
    ret = ret.substring(2, ret.length - 2)
    val arr = ret.split("],\\[", -1)
    val row = arr.length
    val results = new Array[ListNode](row)
    var i = 0
    while (i < row) {
      val listNode = buildListNode("[" + arr(i).trim + "]")
      results(i) = listNode
      i += 1
    }
    results
  }

  def buildListNodeArray(list: ListBuffer[_]): Array[ListNode] = {
    if (list == null || list.size == 0) return null
    val row = list.size
    val results = new Array[ListNode](row)
    var i: Int = 0
    while (i < row) {
      val listNode = buildListNode(list(i).asInstanceOf[ListBuffer[_]])
      results(i) = listNode
      i += 1
    }
    results
  }

  /**
    * 构建树
    *
    * @param data
    * @return
    */
  def buildBinaryTree(data: String): TreeNode = {
    if (data == null || data.trim.length == 0 || data.equals("null") || data.equals("[]")) return null
    var ret = data.replaceAll(" ", "")
    val s1 = ret.substring(1, ret.length - 1)
    val partTree = s1.split(",", -1)
    val root = new TreeNode(partTree(0).toInt)
    val queue = new Queue[TreeNode]
    queue.enqueue(root)
    var i = 1
    while (!queue.isEmpty && i < partTree.length) {
      val node = queue.dequeue()
      if (i < partTree.length && partTree(i) != null && !(partTree(i).trim.equals("null"))) {
        node.left = new TreeNode(partTree(i).trim.toInt)
        queue.enqueue(node.left)
      }
      if (i + 1 < partTree.length && partTree(i + 1) != null && !(partTree(i + 1).trim.equals("null"))) {
        node.right = new TreeNode(partTree(i + 1).trim.toInt)
        queue.enqueue(node.right)
      }
      i += 2
    }
    queue.clear()
    root
  }

  def buildBinaryTree(list: ListBuffer[_]): TreeNode = {
    if (list == null || list.size == 0) return null
    val root: TreeNode = new TreeNode(list(0).toString.toInt)
    val queue: Queue[TreeNode] = new Queue[TreeNode]
    queue.enqueue(root)
    var i: Int = 1
    while ( {
      !queue.isEmpty && i < list.size
    }) {
      val node: TreeNode = queue.dequeue()
      if (i < list.size && (list(i) != null) && !(list(i).toString.equals("null"))) {
        node.left = new TreeNode(list(i).toString.toInt)
        queue.enqueue(node.left)
      }
      if (i + 1 < list.size && list(i + 1) != null && !(list(i + 1).toString.equals("null"))) {
        node.right = new TreeNode(list(i + 1).toString.toInt)
        queue.enqueue(node.right)
      }
      i += 2
    }
    queue.clear()
    root
  }


  /**
    * 构建N叉树
    *
    * @param data
    * @return
    */
  def buildMultiTree(data: String): Node = {
    if (data == null || data.trim.length <= 0 || data == "null" || data == "{}") return null
    //创建json解析器,注意这里如果用fastJson解析不了
    val parse: JsonParser = new JsonParser
    val jsonObject: JsonObject = parse.parse(data).asInstanceOf[JsonObject]
    val value: Int = jsonObject.get("val").getAsInt
    //根节点
    val root: Node = new Node(value, null)
    val childJsonJsonArray: JsonArray = jsonObject.getAsJsonArray("children")
    if (childJsonJsonArray != null && !childJsonJsonArray.isJsonNull) {
      var tempList: List[Node] = List()
      var j: Int = 0
      while (j < childJsonJsonArray.size) {
        val childJsonElement: JsonElement = childJsonJsonArray.get(j)
        val childJsonObject: JsonObject = if (childJsonElement != null && !childJsonElement.isJsonNull) childJsonElement.getAsJsonObject
        else null
        if (childJsonObject != null) {
          val childData: String = childJsonObject.toString
          val temp: Node = buildMultiTree(childData)
          tempList = tempList:+(temp)
        }

          j += 1

      }
      root.children = tempList
    }
    root
  }

  /**
    * 字符串构建ListBuffer
    * 例如[1,2,3]
    *
    * @param data
    * @return
    */
  def buildListBuffer(data: String): ListBuffer[Any] = {
    if (data == null || data.trim.length == 0 || data.equals("null") || data.indexOf("[") < 0) return null
    if (data.equals("[]")) return ListBuffer()
    var ret = data.trim
    //去掉最外层的[]
    ret = ret.substring(1, ret.length - 1)
    var splitStr: String = null
    var arr: Array[String] = null
    if (ret.indexOf("[") >= 0) {
      val vList: ListBuffer[String] = ListBuffer()
      var count: Int = 0
      var stringBuffer: StringBuffer = new StringBuffer
      var i: Int = 0
      while (i < ret.length) {
        val c: Char = ret.charAt(i)
        stringBuffer.append(c)
        if (c.equals('[')) count += 1
        else if (c.equals(']')) count -= 1
        else if (c.equals(',') && count == 0) {
          stringBuffer.deleteCharAt(stringBuffer.length - 1)
          vList += (stringBuffer.toString)
          stringBuffer = new StringBuffer
        }
        i += 1
      }
      vList += (stringBuffer.toString)
      arr = new Array[String](vList.size)
      i = 0
      while (i < vList.size) {
        arr(i) = vList(i)
        i += 1
      }
    } else {
      splitStr = ","
      arr = ret.split(splitStr, -1)
    }
    var flag: Boolean = false
    val length: Int = arr.length
    val list: ListBuffer[Any] = ListBuffer()
    var i: Int = 0
    while (i < length) {
      var newData: String = arr(i)
      if(newData!=null){
        newData = newData.trim
      }
      flag = false
      if (newData.indexOf("[") >= 0) flag = true
      if (flag) list += (buildListBuffer(newData))
      else if (newData == null || newData.length == 0) list += (null)
      else if (StringUtil.judgeNumber(newData)) list += (newData.toInt)
      else list += (StringUtil.changeStr(newData))
      i += 1
    }
    list
  }



  /**
    * 字符串构建List
    * 例如[1,2,3]
    *
    * @param data
    * @return
    */
  def buildList(data: String): List[Any] = {
    if (data == null || data.trim.length == 0 || data.equals("null") || data.indexOf("[") < 0) return null
    if (data.equals("[]")) return List()
    var ret = data.trim
    //去掉最外层的[]
    ret = ret.substring(1, ret.length - 1)
    var splitStr: String = null
    var arr: Array[String] = null
    if (ret.indexOf("[") >= 0) {
      var vList: List[_] = List()
      var count: Int = 0
      var stringBuffer: StringBuffer = new StringBuffer
      var i: Int = 0
      while (i < ret.length) {
        val c: Char = ret.charAt(i)
        stringBuffer.append(c)
        if (c.equals('[')) count += 1
        else if (c.equals(']')) count -= 1
        else if (c.equals(',') && count == 0) {
          stringBuffer.deleteCharAt(stringBuffer.length - 1)
          vList = vList :+ (stringBuffer.toString)
          stringBuffer = new StringBuffer
        }
        i += 1
      }
      vList = vList :+ (stringBuffer.toString)
      arr = new Array[String](vList.size)
      i = 0
      while (i < vList.size) {
        arr(i) = vList(i).toString
        i += 1
      }
    } else {
      splitStr = ","
      arr = ret.split(splitStr, -1)
    }
    var flag: Boolean = false
    val length: Int = arr.length
    var list: List[Any] = List()
    var i: Int = 0
    while (i < length) {
      var newData: String = arr(i)
      if(newData!=null){
        newData = newData.trim();
      }
      flag = false
      if (newData.indexOf("[") >= 0) flag = true
      if (flag) list = list :+ (buildList(newData))
      else if (newData == null || newData.length == 0) list = list :+ (null)
      else if (StringUtil.judgeNumber(newData)) list = list :+ (newData.toInt)
      else list = list :+ (StringUtil.changeStr(newData))
      i += 1
    }
    list
  }


  /**
    * 字符串构建List
    * 例如[1,2,3]
    *
    * @param data
    * @return
    */
  def buildNestedIntegerList(data: String): List[NestedInteger] = {
    if (data == null || data.trim.length == 0 || data.equals("null") || data.indexOf("[") < 0) return null
    if (data.equals("[]")) return List()
    var ret = data.trim
    //去掉最外层的[]
    ret = ret.substring(1, ret.length - 1)
    var splitStr: String = null
    var arr: Array[String] = null
    if (ret.indexOf("[") >= 0) {
      var vList: List[_] = List()
      var count: Int = 0
      var stringBuffer: StringBuffer = new StringBuffer
      var i: Int = 0
      while (i < ret.length) {
        val c: Char = ret.charAt(i)
        stringBuffer.append(c)
        if (c.equals('[')) count += 1
        else if (c.equals(']')) count -= 1
        else if (c.equals(',') && count == 0) {
          stringBuffer.deleteCharAt(stringBuffer.length - 1)
          vList = vList :+ (stringBuffer.toString)
          stringBuffer = new StringBuffer
        }
        i += 1
      }
      vList = vList :+ (stringBuffer.toString)
      arr = new Array[String](vList.size)
      i = 0
      while (i < vList.size) {
        arr(i) = vList(i).toString
        i += 1
      }
    } else {
      splitStr = ","
      arr = ret.split(splitStr, -1)
    }
    var flag: Boolean = false
    val length: Int = arr.length
    var list: List[NestedInteger] = List()
    var i: Int = 0
    while (i < length) {
      var newData: String = arr(i)
      if(newData!=null){
        newData = newData.trim
      }
      flag = false
      if (newData.indexOf("[") >= 0) flag = true
      if (flag) list = list :+ (new NestedInteger(buildNestedIntegerList(newData)))
      else if (newData == null || newData.length == 0) list = list :+ (null)
      else if (StringUtil.judgeNumber(newData)) list = list :+ (new NestedInteger(newData.toInt))
      i += 1
    }
    list
  }
}
