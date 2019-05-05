package com.learn.java.leetcode.base


import com.learn.java.leetcode.base.utils.Print

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
      if (i < paramLength) Print.print(dataList(i))
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
  def inputBuild(parameterTypes: Array[Class[_]], inputObjArr: Array[AnyRef], dataList: ListBuffer[String], tempList: ListBuffer[_]): Unit = {

  }

  /**
    * 打印输出参数方法
    *
    * @param outputObj 算法输出值
    */
  def printOutput(outputObj: Any): Unit = {
    System.out.println("格式输出:")
    Print.print(outputObj)
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
  def outputVerify(inputObjArr: Array[AnyRef], trueResultList: ListBuffer[String], outputObj: Any, dataList: ListBuffer[String], tempList: ListBuffer[_]): Boolean = {
    true
  }

  /**
    * 打印校验结果
    *
    * @param trueResultList 正确结果集，如果存在多个正确值，任意结果均正确
    * @param testResult     算法运行结果
    * @param resultFlag     验证结果
    */
  def printOutVerify(trueResultList: ListBuffer[String], testResult: String, resultFlag: Boolean): Unit = {

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
  def inputVerify(inputObjArr: Array[AnyRef], trueInputResult: String, outputObj: Any, inputIndex: Int, tempList: ListBuffer[_]): Boolean = {
    true
  }

  /**
    * 打印输入结果
    *
    * @param trueInputResult 正确输入结果
    * @param testInputResult 算法运行结果
    * @param resultFlag      验证结果
    */
  def printInputVerify(trueInputResult: String, testInputResult: String, resultFlag: Boolean): Unit = {

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
     null
  }
}
