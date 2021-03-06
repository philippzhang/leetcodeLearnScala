package com.learn.scala.leetcode.base

import com.learn.scala.leetcode.base.structure.{ListNode, TreeNode}
import com.learn.scala.leetcode.base.utils.{Build, Format, PrintObj, StringUtil}

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
      if (parameterName.equals("int")) {
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
      } else if (parameterName.equals("[I")) {
        val array = Build.buildArray(data)
        inputObjArr(j) = array
      } else if (parameterName.equals("[[I")) {
        val matrix: Array[Array[Int]] = Build.buildMatrix(data)
        inputObjArr(j) = matrix
      } else if (parameterName.equals("[B")) {
        val array = Build.buildArrayBoolean(data)
        inputObjArr(j) = array
      } else if (parameterName.equals("[[B")) {
        val matrix: Array[Array[Boolean]] = Build.buildMatrixBoolean(data)
        inputObjArr(j) = matrix
      } else if (parameterName.equals("[D")) {
        val array = Build.buildArrayDouble(data)
        inputObjArr(j) = array
      } else if (parameterName.equals("[[D")) {
        val matrix: Array[Array[Double]] = Build.buildMatrixDouble(data)
        inputObjArr(j) = matrix
      } else if (parameterName.equals("[F")) {
        val array = Build.buildArrayFloat(data)
        inputObjArr(j) = array
      } else if (parameterName.equals("[[F")) {
        val matrix: Array[Array[Float]] = Build.buildMatrixFloat(data)
        inputObjArr(j) = matrix
      } else if (parameterName.equals("[C")) {
        val array = Build.buildArrayChar(data)
        inputObjArr(j) = array
      } else if (parameterName.equals("[[C")) {
        val matrix: Array[Array[Char]] = Build.buildMatrixChar(data)
        inputObjArr(j) = matrix
      } else if (parameterName.equals("java.lang.String")) {
        inputObjArr(j) = StringUtil.changeStr(data)
      } else if (parameterName.equals("[Ljava.lang.String;")) {
        val array: Array[String] = Build.buildArrayString(data);
        inputObjArr(j) = array;
      } else if (parameterName.equals("[[[Ljava.lang.String;")) {
        val matrix: Array[Array[String]] = Build.buildMatrixString(data)
        inputObjArr(j) = matrix
      } else if (parameterName.equals("com.learn.scala.leetcode.base.structure.ListNode")) {
        val listNode = Build.buildListNode(data)
        inputObjArr(j) = listNode
      } else if (parameterName.equals("[Lcom.learn.scala.leetcode.base.structure.ListNode;")) {
        val listNode: Array[ListNode] = Build.buildListNodeArray(data)
        inputObjArr(j) = listNode
      } else if (parameterName.equals("com.learn.scala.leetcode.base.structure.TreeNode")) {
        val treeNode: TreeNode = Build.buildBinaryTree(data)
        inputObjArr(j) = treeNode
      } else if (parameterName.equals("scala.collection.immutable.List")) {
        val list = Build.buildList(data)
        inputObjArr(j) = list
      } else if (parameterName.equals("scala.collection.mutable.ListBuffer")) {
        val list = Build.buildListBuffer(data)
        inputObjArr(j) = list
      } else if (parameterName .equals( "com.learn.scala.leetcode.base.structure.Node")) {
        val node = Build.buildMultiTree(data);
        inputObjArr(j) = node
      } else {
        throw new RuntimeException("未定义的类型，构建失败!")
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
    System.out.println("格式化输出参数:")
    PrintObj.printObj(outputObj)
  }

  /**
    * 打印输入参数方法
    *
    * @param inputObj 算法输入值
    */
  def printInput(inputObj: Any): Unit = {
    System.out.println("格式化输入参数:")
    PrintObj.printObj(inputObj)
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
      val trueResult = trueResultList(i).trim
      if (trueResult.equals("null") && outputObj == null) {
        printOutVerify(trueResultList, null, true)
        return true
      }
      try {

        var disOrder = false
        var j = inputObjArr.length
        while (j < dataList.size) {
          if (dataList(j).equals("$disorder")) { //List 无序标志
            disOrder = true
          }
          j += 1
        }
        val parameterName = if (outputObj != null) outputObj.getClass.getName else null

        if (outputObj != null && outputObj.isInstanceOf[List[_]]) {
          if (disOrder) {
            val testResultsList = outputObj.asInstanceOf[List[_]]
            val trueResultsNewList = Build.buildList(trueResult)
            resultFlag = Utilitys.compareListsIgnoreOrder(trueResultsNewList, testResultsList)
          } else {
            resultFlag = trueResult.equals(testResult)
          }
        } else if (outputObj != null && parameterName == "[I") {
          if (disOrder) {
            val testResultsArray: Array[Int] = outputObj.asInstanceOf[Array[Int]]
            val trueResultsArray: Array[Int] = Build.buildArray(trueResult)
            resultFlag = Utilitys.compareArrays(trueResultsArray, testResultsArray)
          }
          else resultFlag = trueResult == testResult
        } else if (outputObj != null && parameterName.equals("[Ljava.lang.String;")) {
          if (disOrder) {
            val testResultsArray: Array[String] = outputObj.asInstanceOf[Array[String]]
            val trueResultsArray: Array[String] = Build.buildArrayString(trueResult)
            resultFlag = Utilitys.compareArraysString(trueResultsArray, testResultsArray)
          } else resultFlag = trueResult.equals(testResult)
        } else if (outputObj != null && parameterName.equals("[C")) {
          if (disOrder) {
            val testResultsArray: Array[Char] = outputObj.asInstanceOf[Array[Char]]
            val trueResultsArray: Array[Char] = Build.buildArrayChar(trueResult);
            resultFlag = Utilitys.compareArraysChar(trueResultsArray, testResultsArray);
          } else {
            resultFlag = trueResult.equals(testResult);
          }
        } else if (outputObj != null && parameterName.equals("[[I")) {
          if (disOrder) {
            val testResultsMatrix: Array[Array[Int]] = outputObj.asInstanceOf[Array[Array[Int]]];
            val trueResultsMatrix: Array[Array[Int]] = Build.buildMatrix(trueResult);
            resultFlag = Utilitys.compareMatrix(trueResultsMatrix, testResultsMatrix);
          } else {
            resultFlag = trueResult.equals(testResult);
          }
        } else if (outputObj != null && outputObj.isInstanceOf[Double]) {
          val testResultDouble: Double = outputObj.toString.toDouble
          val trueResultDouble: Double = trueResult.toDouble
          resultFlag = StringUtil.IsEqual(testResultDouble, trueResultDouble)
        } else if (outputObj != null && outputObj.isInstanceOf[Float]) {
          val testResultFloat: Float = outputObj.toString().toFloat
          val trueResultFloat: Float = trueResult.toFloat
          resultFlag = StringUtil.IsEqual(testResultFloat, trueResultFloat);
        } else {
          resultFlag = trueResult.equals(testResult)
        }
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
  def inputVerify(inputObjArr: Array[Any], trueInputResult: String, outputObj: Any, inputIndex: Int, dataList: ListBuffer[String], tempList: ListBuffer[_]): Boolean = {
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
    val retList = Utilitys.funcListTest(this, funcList, paramList)
    return retList
  }
}
