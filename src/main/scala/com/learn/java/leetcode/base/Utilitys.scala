package com.learn.java.leetcode.base


import java.io.IOException
import java.lang.reflect.Method

import com.learn.java.leetcode.base.utils.StringUtil
import org.apache.commons.lang.StringUtils

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.control.Breaks
import scala.reflect.runtime.{universe => ru}

object Utilitys {
  val classMirror = ru.runtimeMirror(getClass.getClassLoader)         //获取运行时类镜像
  def test(callBack: CallBack): Boolean = {
    val testList = readTxtFile(callBack)
    //类方法定义
    val classList = testList(0)
    //备注
    val algorithmRemark = classList(0)

    println(algorithmRemark)
    println("-----------------------------")
    var testFlag = true
    if (classList.size <= 1) { //throw new RuntimeException("未定义算法主类和方法!");
      println("未定义算法主类和方法!")
      println("-----------------------------")
      return false
    }

    for (i <- Range(1, classList.size)) {
      var funcStr = classList(i)
      if (funcStr.trim.length > 0) {
        var funcRemark: String = null
        val index = funcStr.indexOf("#")
        if (index >= 0) {
          funcRemark = funcStr.substring(index + 1)
          funcStr = funcStr.substring(0, index)
        }
        val funcArr = funcStr.split("\\.")
        if (funcArr.length < 2) { //throw new RuntimeException("方法参数定义错误,应该是: className.funcName");
          println("方法参数定义错误,应该是: className.funcName")
          return false
        }
        val algorithmClassName = funcArr(0)
        val algorithmFuncName = funcArr(1).trim
        if (StringUtils.isBlank(funcRemark)) {
          funcRemark = algorithmFuncName
        }
        else {
          funcRemark = algorithmFuncName + " " + funcRemark
        }

        println(funcRemark)
        println("-----------------------------")

        var jCount: Int = 1
        for (j <- Range(1, testList.size)) {
          //数据
          val dataList: ListBuffer[String] = testList(j)
          if (dataList != null && dataList.size > 0) {
            System.out.println("第" + jCount + "组数据:")
            val resultFlag: Boolean = test(callBack, algorithmClassName, algorithmFuncName, dataList)
            if (!resultFlag) testFlag = false
            System.out.println("-----------------------------")
            jCount += 1
          }
        }
      }
    }

    if (!testFlag) {
      System.out.println("存在错误!")
      System.out.println("-----------------------------")
    }
    return testFlag
  }

  private def test(callBack: CallBack, algorithmClassName: String, algorithmFuncName: String, dataList: ListBuffer[String]): Boolean = {
    var testFlag = true
    try {
      val packageName = callBack.getClass.getPackage.getName

      val algorithmClass = Class.forName(packageName + "." + algorithmClassName)
      val methods = algorithmClass.getMethods

      val classTest = classMirror.staticModule(packageName + "." + algorithmClassName)          //获取需要反射object
      val methodsTest = classMirror.reflectModule(classTest)                  //构造获取方式的对象
      val objMirror = classMirror.reflect(methodsTest.instance)               //反射结果赋予对象
      val methodTest = methodsTest.symbol.typeSignature.member(ru.TermName(algorithmFuncName)).asMethod  //反射调用函数

      var i:Int = 0
      while (i < methods.size ){
        val method:Method = methods(i)
        var parameterTypes: Array[Class[_]] = null
        if (algorithmFuncName .equals(method.getName)) {
          var invokeFlag = true
          // 得到方法的返回值类型的类型
          val returnType = methods(i).getReturnType
          val returnTypeName = returnType.getName

           parameterTypes = methods(i).getParameterTypes
          val paramLength = parameterTypes.length
          val inputObjArr = new Array[Any](paramLength)

          //临时集合，用于输入和输出之间传递
          val tempList:ListBuffer[_] = new ListBuffer()

          println("输入:")
          //打印输入参数
          try
            callBack.printInput(dataList, paramLength)
          catch {
            case e: Exception =>
              e.printStackTrace()
              testFlag = false
              invokeFlag = false
          }

          //如果入参需要重构
          try
            callBack.inputBuild(parameterTypes, inputObjArr, dataList, tempList)
          catch {
            case e: Exception =>
              e.printStackTrace()
              testFlag = false
              invokeFlag = false
          }



          val startTime = System.currentTimeMillis
          //调用算法
          var outputObj:Any = null
          try
              if (invokeFlag){
                //val m: Method = algorithmClass.getDeclaredMethod(algorithmFuncName,classOf[Array[Int]],classOf[Int])
                //val o =algorithmClass.newInstance
                //outputObj = method.invoke(o, inputObjArr(0),inputObjArr(1))
                if(inputObjArr.length==0){
                  outputObj = objMirror.reflectMethod(methodTest)()
                }else if(inputObjArr.length==1){
                  outputObj = objMirror.reflectMethod(methodTest)(inputObjArr(0))
                }else if(inputObjArr.length==2){
                  outputObj = objMirror.reflectMethod(methodTest)(inputObjArr(0),inputObjArr(1))
                }else if(inputObjArr.length==3){
                  outputObj = objMirror.reflectMethod(methodTest)(inputObjArr(0),inputObjArr(1),inputObjArr(2))
                }else if(inputObjArr.length==4){
                  outputObj = objMirror.reflectMethod(methodTest)(inputObjArr(0),inputObjArr(1),inputObjArr(2),inputObjArr(3))
                }else if(inputObjArr.length==5){
                  outputObj = objMirror.reflectMethod(methodTest)(inputObjArr(0),inputObjArr(1),inputObjArr(2),inputObjArr(3),inputObjArr(4))
                }

              }
          catch {
            case e: Exception =>
              e.printStackTrace()
              testFlag = false
          }
          val endTime = System.currentTimeMillis

          //格式化打印
          if (!("Unit".equals( returnTypeName))) { //打印输出
            try
              callBack.printOutput(outputObj)
            catch {
              case e: Exception =>
                e.printStackTrace()
                testFlag = false
            }
          }

          val trueResultOutputList: ListBuffer[String] = ListBuffer()
          var k: Int = paramLength
          while (k < dataList.size) {
            var trueResult: String = dataList(k)
            if (StringUtils.isNotBlank(trueResult)) if (trueResult.startsWith("=")) {
              trueResult = trueResult.substring(1)
              if (StringUtils.isNotBlank(trueResult)) trueResultOutputList+=(trueResult)
            }
            else if (StringUtil.judgeINumber(trueResult)) {
              /**
                * 验证输入参数
                */
              val inputIndex: Int = trueResult.substring(1, 2).toInt
              val trueInputResult: String = trueResult.substring(3)
              if (StringUtils.isNotBlank(trueInputResult) && inputIndex >= 0 && inputIndex < inputObjArr.length) try {
                val resultFlag: Boolean = callBack.inputVerify(inputObjArr, trueInputResult, outputObj, inputIndex, tempList)
                if (!resultFlag) testFlag = false
              } catch {
                case e: Exception =>
                  e.printStackTrace()
                  testFlag = false
                  callBack.printInputVerify(trueInputResult, e.getMessage, false)
              }
            }
              k += 1
          }

          if (trueResultOutputList.size > 0) {
            /**
              * 验证输出结果
              */
            try {
              val resultFlag: Boolean = callBack.outputVerify(inputObjArr, trueResultOutputList, outputObj, dataList, tempList)
              if (!resultFlag) testFlag = false
            } catch {
              case e: Exception =>
                e.printStackTrace()
                testFlag = false
                callBack.printOutVerify(trueResultOutputList, e.getMessage, false)
            }
          }

          println("计算时长: " + (endTime - startTime) + "ms")

        }
        i+=1
      }

    } catch {
      case e1: Exception =>
        e1.printStackTrace()
    }
    testFlag
  }

  private def readTxtFile(callBack: CallBack): ListBuffer[ListBuffer[String]] = {
    val packageName: String = callBack.getClass.getPackage.getName
    val path: String = "/" + packageName.replaceAll("\\.", "/") + "/README.md"
    val str: String = readTxtFile(path)
    val strArr: Array[String] = str.split("---\r", -1)
    val dataList: ListBuffer[ListBuffer[String]] = ListBuffer()
    for (itemI <- strArr) {
      val itemsList: ListBuffer[String] = ListBuffer()
      val strArr2: Array[String] = itemI.split("\r", -1)
      for (itemJ <- strArr2) {
        val tempTrim: String = itemJ.trim
        if (tempTrim.length > 0 && !tempTrim.startsWith("#")) {
          val temp: String = itemJ
          itemsList += temp
        }
      }
      dataList += (itemsList)
    }
    dataList
  }

  private def readTxtFile(path: String) = {
    val lastJson = new StringBuilder
    val url = getClass.getResource(path)
    val file = Source.fromFile(url.getPath())
    try {
      var i = 0
      // 一次读入一行，直到读入null为文件结束
      var flag = false
      val loop = Breaks
      loop.breakable {
        for (tempString <- file.getLines) {
          if (tempString.equals("# 测试用例")) flag = true
          if (flag && tempString.equals("```")) i += 1
          else if (i == 1 && tempString.length > 0 && !tempString.startsWith("#")) lastJson.append(tempString).append("\r")
          if (i == 2) {
            loop.break
          }
        }
      }
    } catch {
      case e1: IOException =>
        e1.printStackTrace()
    } finally {
      file.close()
    }
    lastJson.toString
  }
}
