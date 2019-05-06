package com.learn.java.leetcode.base


import com.learn.java.leetcode.base.structure.{ListNode, TreeNode}
import com.learn.java.leetcode.base.utils.{Build, Format, PrintObj, StringUtil}

import scala.collection.mutable.ListBuffer

class CallBack {
  /**
    * 打印输入参数方法
    *
    * @param dataList    读入数据列表
    * @param paramLength 算法方法中参数个数
    */
  def printInput(dataList: ListBuffer[String], paramLength: Int): Unit = {
    if (dataList == null) return
    var i: Int = 0
    while (i < dataList.length && i < paramLength) {
      if (i < paramLength) PrintObj.printObj(dataList(i))
      i += 1
    }
  }

  /**
    * 入参构建方法
    *
    * @param parameterTypes 算法方法中参数列表
    * @param inputObjArr    调用算法的参数值列表
    * @param dataList       读入数据列表
    * @param tempList       临时缓存，用于数据传递
    */
  def inputBuild(parameterTypes: Array[Class[_]], inputObjArr: Array[Any], dataList: ListBuffer[String], tempList: ListBuffer[_]): Unit = {
    var j: Int = 0
    while (j < parameterTypes.length) {
      val parameterName = parameterTypes(j).getName
      val data: String = dataList(j)
      if (parameterName.equals("[I")) {
        val array = Build.buildArray(data)
        inputObjArr(j) = array
      } else if (parameterName.equals("[C")) {
        val array = Build.buildArrayChar(data)
        inputObjArr(j) = array
      } else if (parameterName.equals("[[I")) {
        val matrix: Array[Array[Int]] = Build.buildMatrix(data)
        inputObjArr(j) = matrix
      } else if (parameterName.equals("[Ljava.lang.String;")) {
        val array: Array[String] = Build.buildArrayString(data);
        inputObjArr(j) = array;
      } else if (parameterName.equals("int")) {
        val v = data.toInt
        inputObjArr(j) = v
      } else if (parameterName.equals("long")) {
        val v = data.toLong
        inputObjArr(j) = v
      } else if (parameterName.equals("double")) {
        val v = data.toDouble
        inputObjArr(j) = v
      } else if (parameterName.equals("float")) {
        val v = data.toFloat
        inputObjArr(j) = v
      } else if (parameterName.equals("boolean")) {
        val v = data.toBoolean
        inputObjArr(j) = v
      } else if (parameterName.equals("short")) {
        val v = data.toShort
        inputObjArr(j) = v
      } else if (parameterName.equals("java.lang.String")) {
        inputObjArr(j) = StringUtil.changeStr(data)
      } else if (parameterName.equals("com.learn.java.leetcode.base.structure.ListNode")) {
        val listNode = Build.buildListNode(data)
        inputObjArr(j) = listNode
      } else if (parameterName.equals("[Lcom.learn.java.leetcode.base.structure.ListNode;")) {
        val listNode: Array[ListNode] = Build.buildListNodeArray(data)
        inputObjArr(j) = listNode
      } else if (parameterName.equals("com.learn.java.leetcode.base.structure.TreeNode")) {
        val treeNode: TreeNode = Build.buildBinaryTree(data)
        inputObjArr(j) = treeNode
      } else if (parameterName.equals( "java.util.List")) {
        val list = Build.buildList(data)
        inputObjArr(j) = list
      }
      j += 1
    }
  }

  /**
    * 打印输出参数方法
    *
    * @param outputObj 算法输出值
    */
  def printOutput(outputObj: Any): Unit = {
    System.out.println("格式输出:")
    PrintObj.printObj(outputObj)
  }

  /**
    * 输出参数验证方法
    *
    * @param inputObjArr    调用算法的参数值列表
    * @param trueResultList 正确结果集，如果存在多个正确值，任意结果均正确
    * @param outputObj      算法输出值
    * @param dataList       读入数据列表
    * @param tempList       临时缓存，用于数据传递
    */
  def outputVerify(inputObjArr: Array[Any], trueResultList: ListBuffer[String], outputObj: Any, dataList: ListBuffer[String], tempList: ListBuffer[_]): Boolean = {
    var resultFlag = false
    val testResult = Format.format(outputObj)
    var i = 0
    while (i < trueResultList.size) {
      val trueResult = trueResultList(i)
      if (trueResult.equals("null") && outputObj == null) {
        printOutVerify(trueResultList, null, true)
        return true
      }
      try {
        resultFlag = trueResult == testResult
        if (resultFlag) {
          printOutVerify(trueResultList, testResult, resultFlag)
          return true
        }
      } catch {
        case e: Exception =>
          e.printStackTrace()
          printOutVerify(trueResultList, e.getMessage, false)
          return false
      }
      i += 1
    }
    printOutVerify(trueResultList, testResult, resultFlag)
    return resultFlag
  }

  /**
    * 打印校验结果
    *
    * @param trueResultList 正确结果集，如果存在多个正确值，任意结果均正确
    * @param testResult     算法运行结果
    * @param resultFlag     验证结果
    */
  def printOutVerify(trueResultList: ListBuffer[String], testResult: String, resultFlag: Boolean): Unit = {
    println("输出结果:")
    println(testResult)
    println("预期结果" + (if (trueResultList.size > 1) " (以下任意结果均正确) " else "") + ":")
    var i = 0
    while (i < trueResultList.size) {
      println(trueResultList(i))
      i += 1
    }
    print("验证结果: ")
    if (resultFlag) {
      println("正确")
    }
    else {
      println("错误")
    }
  }

  /**
    * 输入参数验证方法
    *
    * @param inputObjArr     调用算法的参数值列表
    * @param trueInputResult 正确输入结果
    * @param outputObj       算法输出值
    * @param inputIndex      需要验证的入参参数序号
    * @param tempList        临时缓存，用于数据传递
    */
  def inputVerify(inputObjArr: Array[Any], trueInputResult: String, outputObj: Any, inputIndex: Int, tempList: ListBuffer[_]): Boolean = {
    try {
      val inputObj = inputObjArr(inputIndex)
      val testInputResult = Format.format(inputObj)
      val resultFlag = trueInputResult == testInputResult
      printInputVerify(trueInputResult, testInputResult, resultFlag)
      return resultFlag
    } catch {
      case e: Exception =>
        e.printStackTrace()
        printInputVerify(trueInputResult, e.getMessage, false)
        return false
    }
  }

  /**
    * 打印输入结果
    *
    * @param trueInputResult 正确输入结果
    * @param testInputResult 算法运行结果
    * @param resultFlag      验证结果
    */
  def printInputVerify(trueInputResult: String, testInputResult: String, resultFlag: Boolean): Unit = {
    println("入参输出:")
    println(testInputResult)
    println("入参预期结果:")
    println(trueInputResult)
    print("入参验证结果: ")
    if (resultFlag) {
      println("正确")
    }
    else {
      println("错误")
    }
  }

  /**
    * 列表式方式参数调用
    * 例如
    * ["LRUCache","put","put","get","put","get","put","get","get","get"]
    * [[2],[1,1],[2,2],[1],[3,3],[2],[4,4],[1],[3],[4]]
    * 第一个参数第一个值是构造函数
    *
    * @param funcList  方法列表
    * @param paramList 参数列表
    * @return
    */
  def funcListTest(funcList: ListBuffer[_], paramList: ListBuffer[_]): ListBuffer[_] = {
    //val retList = Utilitys.funcListTest(getClass, funcList, paramList)
    //return retList
    null
  }
}
