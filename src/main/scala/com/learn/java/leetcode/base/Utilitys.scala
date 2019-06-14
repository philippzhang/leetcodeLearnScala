package com.learn.java.leetcode.base


import java.io.{File, IOException}
import java.lang.reflect.{Constructor, InvocationTargetException, Method}
import java.util

import com.google.gson.{JsonArray, JsonElement, JsonParser}
import com.learn.java.leetcode.base.utils.{Build, NoImplException, PrintObj, StringUtil}
import org.apache.commons.lang.StringUtils

import scala.collection.immutable.TreeMap
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.control.Breaks

object Utilitys {
  //val classMirror = ru.runtimeMirror(getClass.getClassLoader) //获取运行时类镜像
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
      //return false
      throw new NoImplException("未定义算法主类和方法!")
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

      //val classTest = classMirror.staticModule(packageName + "." + algorithmClassName) //获取需要反射object
      //val methodsTest = classMirror.reflectModule(classTest) //构造获取方式的对象
      //val objMirror = classMirror.reflect(methodsTest.instance) //反射结果赋予对象
      //val methodTest = methodsTest.symbol.typeSignature.member(ru.TermName(algorithmFuncName)).asMethod //反射调用函数

      var i: Int = 0
      while (i < methods.size) {
        val method: Method = methods(i)
        var parameterTypes: Array[Class[_]] = null
        if (algorithmFuncName.equals(method.getName)) {
          var invokeFlag = true
          // 得到方法的返回值类型的类型
          val returnType = methods(i).getReturnType
          val returnTypeName = returnType.getName

          parameterTypes = methods(i).getParameterTypes
          val paramLength = parameterTypes.length
          val inputObjArr = new Array[Any](paramLength)

          //临时集合，用于输入和输出之间传递
          val tempList: ListBuffer[_] = new ListBuffer()

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
          var outputObj: Any = null
          try
              if (invokeFlag) {
                //val m: Method = algorithmClass.getDeclaredMethod(algorithmFuncName,classOf[Array[Int]],classOf[Int])
                //val o =algorithmClass.newInstance

                /*if (inputObjArr.length == 0) {
                  outputObj = objMirror.reflectMethod(methodTest)()
                } else if (inputObjArr.length == 1) {
                  outputObj = objMirror.reflectMethod(methodTest)(inputObjArr(0))
                } else if (inputObjArr.length == 2) {
                  outputObj = objMirror.reflectMethod(methodTest)(inputObjArr(0), inputObjArr(1))
                } else if (inputObjArr.length == 3) {
                  outputObj = objMirror.reflectMethod(methodTest)(inputObjArr(0), inputObjArr(1), inputObjArr(2))
                } else if (inputObjArr.length == 4) {
                  outputObj = objMirror.reflectMethod(methodTest)(inputObjArr(0), inputObjArr(1), inputObjArr(2), inputObjArr(3))
                } else if (inputObjArr.length == 5) {
                  outputObj = objMirror.reflectMethod(methodTest)(inputObjArr(0), inputObjArr(1), inputObjArr(2), inputObjArr(3), inputObjArr(4))
                }
                if (inputObjArr.length == 0) {
                  outputObj = algorithmClass.getDeclaredMethod(algorithmFuncName).invoke(o)
                } else if (inputObjArr.length == 1) {
                  outputObj = algorithmClass.getDeclaredMethod(algorithmFuncName,parameterTypes(0)).invoke(o, inputObjArr(0).asInstanceOf[Object])
                } else if (inputObjArr.length == 2) {
                  outputObj = algorithmClass.getDeclaredMethod(algorithmFuncName,parameterTypes(0),parameterTypes(1)).invoke(o, inputObjArr(0).asInstanceOf[Object],inputObjArr(1).asInstanceOf[Object])
                } else if (inputObjArr.length == 3) {
                  outputObj = algorithmClass.getDeclaredMethod(algorithmFuncName,parameterTypes(0),parameterTypes(1),parameterTypes(2)).invoke(o, inputObjArr(0).asInstanceOf[Object],inputObjArr(1).asInstanceOf[Object],inputObjArr(2).asInstanceOf[Object])
                } else if (inputObjArr.length == 4) {
                  outputObj = algorithmClass.getDeclaredMethod(algorithmFuncName,parameterTypes(0),parameterTypes(1),parameterTypes(2),parameterTypes(3)).invoke(o, inputObjArr(0).asInstanceOf[Object],inputObjArr(1).asInstanceOf[Object],inputObjArr(2).asInstanceOf[Object],inputObjArr(3).asInstanceOf[Object])
                } else if (inputObjArr.length == 5) {
                  outputObj = algorithmClass.getDeclaredMethod(algorithmFuncName,parameterTypes(0),parameterTypes(1),parameterTypes(2),parameterTypes(3),parameterTypes(4)).invoke(o, inputObjArr(0).asInstanceOf[Object],inputObjArr(1).asInstanceOf[Object],inputObjArr(2).asInstanceOf[Object],inputObjArr(3).asInstanceOf[Object],inputObjArr(4).asInstanceOf[Object])
                }
                */
                var o: Any = null
                if (algorithmFuncName.endsWith("funcListTest")) {
                  o = algorithmClass.newInstance()
                }

                if (inputObjArr.length == 0) {
                  outputObj = method.invoke(o)
                } else if (inputObjArr.length == 1) {
                  outputObj = method.invoke(o, inputObjArr(0).asInstanceOf[Object])
                } else if (inputObjArr.length == 2) {
                  outputObj = method.invoke(o, inputObjArr(0).asInstanceOf[Object], inputObjArr(1).asInstanceOf[Object])
                } else if (inputObjArr.length == 3) {
                  outputObj = method.invoke(o, inputObjArr(0).asInstanceOf[Object], inputObjArr(1).asInstanceOf[Object], inputObjArr(2).asInstanceOf[Object])
                } else if (inputObjArr.length == 4) {
                  outputObj = method.invoke(o, inputObjArr(0).asInstanceOf[Object], inputObjArr(1).asInstanceOf[Object], inputObjArr(2).asInstanceOf[Object], inputObjArr(3).asInstanceOf[Object])
                } else if (inputObjArr.length == 5) {
                  outputObj = method.invoke(o, inputObjArr(0).asInstanceOf[Object], inputObjArr(1).asInstanceOf[Object], inputObjArr(2).asInstanceOf[Object], inputObjArr(3).asInstanceOf[Object], inputObjArr(4).asInstanceOf[Object])
                }

              }
          catch {
            case e: Exception =>
              e.printStackTrace()
              testFlag = false
          }
          val endTime = System.currentTimeMillis

          //格式化打印
          if (!("Unit".equals(returnTypeName))) { //打印输出
            var enprint: Boolean = true
            var l: Int = inputObjArr.length
            while (l < dataList.size) {
              if (dataList(l).equals("$disprint")) {
                enprint = false
              }
              l += 1
            }
            if (enprint) {
              try
                callBack.printOutput(outputObj)
              catch {
                case e: Exception =>
                  e.printStackTrace()
                  testFlag = false
              }
            }
          }

          val trueResultOutputList: ListBuffer[String] = ListBuffer()
          var k: Int = paramLength
          while (k < dataList.size) {
            var trueResult: String = dataList(k)
            if (StringUtils.isNotBlank(trueResult)) if (trueResult.startsWith("=")) {
              trueResult = trueResult.substring(1)
              if (StringUtils.isNotBlank(trueResult)) trueResultOutputList += (trueResult)
            }
            else if (StringUtil.judgeINumber(trueResult)) {
              /**
                * 验证输入参数
                */
              val inputIndex: Int = trueResult.substring(1, 2).toInt
              val trueInputResult: String = trueResult.substring(3).trim
              if (StringUtils.isNotBlank(trueInputResult) && inputIndex >= 0 && inputIndex < inputObjArr.length) try {
                val resultFlag: Boolean = callBack.inputVerify(inputObjArr, trueInputResult, outputObj, inputIndex, dataList, tempList)
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
        i += 1
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

  /**
    * 获取某个文件夹下的所有文件
    *
    * @param path 文件夹的路径
    * @return
    */
  def getAllLCFileName(path: String): ListBuffer[String] = {
    val packageList: ListBuffer[String] = ListBuffer()
    val file: File = new File(path)
    val tempList: Array[File] = file.listFiles
    var i: Int = 0
    while (i < tempList.length) {
      if (tempList(i).isDirectory) {
        val packageName: String = tempList(i).getName
        if (packageName.startsWith("lc")) {
          packageList += packageName
        }
      }
      i += 1
    }
    return packageList
  }

  /**
    * 调用test
    *
    * @param className
    * @return
    */
  def funcInvoke(className: String): Boolean = {
    try {
      val algorithmClass = Class.forName(className)
      val callBack = algorithmClass.newInstance().asInstanceOf[CallBack]
      return test(callBack)
    } catch {
      case e: NoImplException =>
        throw e
      case e: ClassNotFoundException =>
        e.printStackTrace()
      case e: IllegalAccessException =>
        e.printStackTrace()
      case e: InstantiationException =>
        e.printStackTrace()
      case e: Exception =>
        e.printStackTrace()
    }
    false
  }

  /**
    * 用于实现调用多个方法的列表执行
    *
    * @param callBack
    * @param funcList
    * @param paramList
    * @return
    */
  def funcListTest(callBack: CallBack, funcList: ListBuffer[_], paramList: ListBuffer[_]): ListBuffer[_] = {
    val retList: ListBuffer[Any] = ListBuffer()
    if (funcList != null && funcList.size > 0 && paramList != null && paramList.size > 0) {
      var obj: Any = null
      var algorithmClass: Class[_] = null
      var packageName = callBack.getClass.getPackage().getName()
      var i: Int = 0
      while (i < funcList.size) {
        val funcName = funcList(i).toString
        val params: ListBuffer[_] = paramList(i).asInstanceOf[ListBuffer[_]]
        if (i == 0) {
          //第一个值是构造方法
          try {
            algorithmClass = Class.forName(packageName + "." + funcName)
            val constructors: Array[Constructor[_]] = algorithmClass.getConstructors
            var j = 0
            while (j < constructors.length) {
              val c: Constructor[_] = constructors(j)
              if (params.size == 0 && c.getParameterTypes.length == 0) {
                obj = c.newInstance()
              }
              else { //通过参数个数判断,可能有更好的办法
                if (params.size > 0 && c.getParameterTypes.length == params.size) {
                  var flag = true
                  val inputObjArr = new Array[Any](params.size)
                  var k: Int = 0

                    while (k < params.size) {
                      val parameterName: String = c.getParameterTypes()(k).getName
                      val data = params(k)
                      if (parameterName.equals("int") && data.isInstanceOf[Int]) {
                        inputObjArr(k) = data.toString.toInt
                      }
                      else if (parameterName.equals("long") && data.isInstanceOf[Long]) {
                        inputObjArr(k) = data.toString.toLong
                      }
                      else if (parameterName.equals("double") && data.isInstanceOf[Double]) {
                        inputObjArr(k) = data.toString.toDouble
                      }
                      else if (parameterName.equals("float") && data.isInstanceOf[Float]) {
                        inputObjArr(k) = data.toString.toFloat
                      }
                      else if (parameterName.equals("boolean") && data.isInstanceOf[Boolean]) {
                        inputObjArr(k) = data.toString.toBoolean
                      } else if (parameterName.equals("java.lang.String") && data.isInstanceOf[String]) {
                        inputObjArr(k) = StringUtil.changeStr(data.toString)
                      }
                      else if (parameterName.equals("[I") && data.isInstanceOf[ListBuffer[_]]) {
                        val array = Build.buildArray(data.toString.asInstanceOf[ListBuffer[_]])
                        inputObjArr(k) = array
                      }
                      else if (parameterName.equals("[C") && data.isInstanceOf[ListBuffer[_]]) {
                        val array = Build.buildArrayChar(data.toString.asInstanceOf[ListBuffer[_]])
                        inputObjArr(k) = array
                      }
                      else if (parameterName.equals("[Ljava.lang.String;") && data.isInstanceOf[ListBuffer[_]]) {
                        val array = Build.buildArrayString(data.toString.asInstanceOf[ListBuffer[_]])
                        inputObjArr(k) = array
                      }
                      else if (parameterName.equals("scala.collection.mutable.ListBuffer") && data.isInstanceOf[ListBuffer[_]]) {
                        val list = data.asInstanceOf[ListBuffer[_]]
                        inputObjArr(k) = list
                      }
                      else if (parameterName.equals("com.learn.java.leetcode.base.structure.TreeNode") && data.isInstanceOf[ListBuffer[_]]) {
                        val treeNode = Build.buildBinaryTree(data.asInstanceOf[ListBuffer[_]])
                        inputObjArr(k) = treeNode
                      }
                      else if (parameterName.equals("com.learn.java.leetcode.base.structure.ListNode") && data.isInstanceOf[ListBuffer[_]]) {
                        val listNode = Build.buildListNode(data.asInstanceOf[ListBuffer[_]])
                        inputObjArr(k) = listNode
                      }
                      else if (parameterName.equals("[Lcom.learn.java.leetcode.base.structure.ListNode;") && data.isInstanceOf[ListBuffer[_]]) {
                        val listNode = Build.buildListNodeArray(data.asInstanceOf[ListBuffer[_]])
                        inputObjArr(k) = listNode
                      }
                      else { //可能有未处理的类型
                        flag = false

                      }
                      k += 1

                  }
                  if (flag) {
                    if(inputObjArr.length==1){
                      obj = c.newInstance(inputObjArr(0).asInstanceOf[Object])
                    }else if(inputObjArr.length==2){
                      obj = c.newInstance(inputObjArr(0).asInstanceOf[Object],inputObjArr(1).asInstanceOf[Object])
                    }else if(inputObjArr.length==3){
                      obj = c.newInstance(inputObjArr(0).asInstanceOf[Object],inputObjArr(1).asInstanceOf[Object],inputObjArr(2).asInstanceOf[Object])
                    }else if(inputObjArr.length==4){
                      obj = c.newInstance(inputObjArr(0).asInstanceOf[Object],inputObjArr(1).asInstanceOf[Object],inputObjArr(2).asInstanceOf[Object],inputObjArr(3).asInstanceOf[Object])
                    }else if(inputObjArr.length==5){
                      obj = c.newInstance(inputObjArr(0).asInstanceOf[Object],inputObjArr(1).asInstanceOf[Object],inputObjArr(2).asInstanceOf[Object],inputObjArr(3).asInstanceOf[Object],inputObjArr(4).asInstanceOf[Object])
                    }

                  }
                }
              }
              j += 1
            }
          } catch {
            case e: InstantiationException =>
              e.printStackTrace()
            case e: IllegalAccessException =>
              e.printStackTrace()
            case e: InvocationTargetException =>
              e.printStackTrace()
            case e: ClassNotFoundException =>
              e.printStackTrace()
          }
          //构造函数返回结果
          retList += (null)
        } else {
          val methods = algorithmClass.getDeclaredMethods
          var k = 0
          while (k < methods.length) {
            val method = methods(k)
            if (funcName == method.getName) {
              val parameterTypes = method.getParameterTypes
              val paramLength = parameterTypes.length
              val inputObjArr = new Array[Any](paramLength)
              var j: Int = 0
              while (j < paramLength) {
                val parameterName = parameterTypes(j).getName
                val data = params(j)
                if (parameterName.equals("int")) {
                  inputObjArr(j) = data.toString.toInt
                }
                else if (parameterName.equals("long")) {
                  inputObjArr(j) = data.toString.toLong
                }
                else if (parameterName.equals("double")) {
                  inputObjArr(j) = data.toString.toDouble
                }
                else if (parameterName.equals("float")) {
                  inputObjArr(j) = data.toString.toFloat
                }
                else if (parameterName.equals("boolean")) {
                  inputObjArr(j) = data.toString.toBoolean
                } else if (parameterName.equals("java.lang.String")) {
                  inputObjArr(j) = StringUtil.changeStr(data.toString)
                }
                else if (parameterName.equals("[I")) {
                  val array = Build.buildArray(data.toString)
                  inputObjArr(j) = array
                }
                else if (parameterName.equals("[C")) {
                  val array = Build.buildArrayChar(data.toString)
                  inputObjArr(j) = array
                }
                else if (parameterName.equals("[[I")) {
                  val matrix = Build.buildMatrix(data.toString)
                  inputObjArr(j) = matrix
                }
                else if (parameterName.equals("[Ljava.lang.String;")) {
                  val array = Build.buildArrayString(data.toString)
                  inputObjArr(j) = array
                }
                else if (parameterName.equals("scala.collection.mutable.ListBuffer")) {
                  val list = Build.buildListBuffer(data.toString)
                  inputObjArr(j) = list
                }
                else if (parameterName.equals("com.learn.java.leetcode.base.structure.TreeNode") && data.isInstanceOf[ListBuffer[_]]) {
                  val treeNode = Build.buildBinaryTree(data.asInstanceOf[ListBuffer[_]])
                  inputObjArr(j) = treeNode
                }
                else if (parameterName == "com.learn.java.leetcode.base.structure.ListNode" && data.isInstanceOf[ListBuffer[_]]) {
                  val listNode = Build.buildListNode(data.asInstanceOf[ListBuffer[_]])
                  inputObjArr(j) = listNode
                }
                else if (parameterName == "[Lcom.learn.java.leetcode.base.structure.ListNode;" && data.isInstanceOf[ListBuffer[_]]) {
                  val listNode = Build.buildListNodeArray(data.asInstanceOf[ListBuffer[_]])
                  inputObjArr(j) = listNode
                }
                j += 1
              }
              //调用方法
              try {
                var outputObj: Any = null
                if (inputObjArr.length == 0) {
                  outputObj = method.invoke(obj)
                } else if (inputObjArr.length == 1) {
                  outputObj = method.invoke(obj, inputObjArr(0).asInstanceOf[Object])
                } else if (inputObjArr.length == 2) {
                  outputObj = method.invoke(obj, inputObjArr(0).asInstanceOf[Object], inputObjArr(1).asInstanceOf[Object])
                } else if (inputObjArr.length == 3) {
                  outputObj = method.invoke(obj, inputObjArr(0).asInstanceOf[Object], inputObjArr(1).asInstanceOf[Object], inputObjArr(2).asInstanceOf[Object])
                } else if (inputObjArr.length == 4) {
                  outputObj = method.invoke(obj, inputObjArr(0).asInstanceOf[Object], inputObjArr(1).asInstanceOf[Object], inputObjArr(2).asInstanceOf[Object], inputObjArr(3).asInstanceOf[Object])
                } else if (inputObjArr.length == 5) {
                  outputObj = method.invoke(obj, inputObjArr(0).asInstanceOf[Object], inputObjArr(1).asInstanceOf[Object], inputObjArr(2).asInstanceOf[Object], inputObjArr(3).asInstanceOf[Object], inputObjArr(4).asInstanceOf[Object])
                }

                retList += (outputObj)
              } catch {
                case e: IllegalAccessException =>
                  e.printStackTrace()
                case e: InvocationTargetException =>
                  e.printStackTrace()
              }
            }
            k += 1
          }
        }
        i += 1
      }
    }
    retList
  }

  def compareListsIgnoreOrder[T](list1: List[T], list2: List[T]): Boolean = {
    if (list1 == null && list2 == null) return true
    if (list1 == null || list2 == null) return false
    if (list1.size != list2.size) return false
    val listNew1: List[T] = sortList(list1).asInstanceOf[List[T]]
    val listNew2: List[T] = sortList(list2).asInstanceOf[List[T]]
    val set1: Set[T] =  listNew1.toSet
    val set2: Set[T] =  listNew2.toSet
    set1.equals(set2)
  }


  /**
    * 排序List,返回一个新的List
    * @param list
    * @return
    */
  def sortList(list: List[_]): List[_] = {
    if (list != null && list.size > 0) {
      val o = list(0)
      if (o.isInstanceOf[List[_]]) {
        var i = 0
        var retList = List[List[_]]();
        while (i < list.size) {
          val list1 = list(i).asInstanceOf[List[_]]
          retList = retList :+ sortList(list1)
            i += 1
        }
        return retList
      }
      else if (o.isInstanceOf[String]) {
        return list.asInstanceOf[List[String]].sortWith(_<_)
      }else if (o.isInstanceOf[Int]) {
        return list.asInstanceOf[List[Int]].sortWith(_<_)
      }else if (o.isInstanceOf[Long]) {
        return list.asInstanceOf[List[Long]].sortWith(_<_)
      }else if (o.isInstanceOf[Float]) {
        return list.asInstanceOf[List[Float]].sortWith(_<_)
      }else if (o.isInstanceOf[Double]) {
        return list.asInstanceOf[List[Double]].sortWith(_<_)
      }else if (o.isInstanceOf[Boolean]) {
        return list.asInstanceOf[List[Boolean]].sortWith(_<_)
      }
    }
    return null
  }


  def compareArrays(array1: Array[Int], array2: Array[Int]): Boolean = {
    if (array1 == null && array2 == null) return true
    if (array1 == null || array2 == null) return false
    if (array1.length != array2.length) return false
    var i = 0
    while (i < array1.length) {
      if (array1(i) != array2(i)) return false
        i += 1
    }
    true
  }

  def compareArraysString(array1: Array[String], array2: Array[String]): Boolean = {
    if (array1 == null && array2 == null) return true
    if (array1 == null || array2 == null) return false
    if (array1.length != array2.length) return false
    var i = 0
    while (i < array1.length) {
      if (array1(i) == array2(i)) return false
        i += 1
    }
    true
  }

  def compareArraysChar(array1: Array[Char], array2: Array[Char]): Boolean = {
    if (array1 == null && array2 == null) return true
    if (array1 == null || array2 == null) return false
    if (array1.length != array2.length) return false
    var i = 0
    while (i < array1.length) {
      if (array1(i) != array2(i)) return false
        i += 1
    }
    true
  }

  def compareMatrix(m1: Array[Array[Int]], m2: Array[Array[Int]]): Boolean = {
    if (m1 == null && m2 == null) return true
    if (m1 == null || m2 == null) return false
    if (m1.length != m2.length || m1(0).length != m2(0).length) return false
    var i = 0
    while (i < m1.length) {
      var j = 0
      while (j < m1(i).length) {
        if (m1(i)(j) != m2(i)(j)) return false
          j += 1
      }
        i += 1
    }
    true
  }


  def sort(e: JsonElement): Unit = {
    if (e.isJsonNull) return
    if (e.isJsonPrimitive) return
    if (e.isJsonArray) {
      val a: JsonArray = e.getAsJsonArray
      val it: util.Iterator[JsonElement] = a.iterator
      while (it.hasNext) {
        sort(it.next)
      }
      return
    }
    if (e.isJsonObject) {
      val tm: Map[String, JsonElement] = TreeMap[String, JsonElement]()
      import scala.collection.JavaConversions._
      for (en <- e.getAsJsonObject.entrySet) {
        tm.put(en.getKey, en.getValue)
      }
      import scala.collection.JavaConversions._
      for (en <- tm.entrySet) {
        e.getAsJsonObject.remove(en.getKey)
        e.getAsJsonObject.add(en.getKey, en.getValue)
        sort(en.getValue)
      }
      return
    }
  }

  /**
    * 排序Json字符串
    *
    * @param json
    * @return
    */
  def sortJsonObject(json: String): String = {
    val p: JsonParser = new JsonParser
    val e: JsonElement = p.parse(json)
    sort(e)
    e.toString
  }

  def main(args: Array[String]): Unit = {
    var l1 = List(List(1,2),List(3,5,1))

    var l2 = List(List(3,1,5),List(2,1))

    var b = compareListsIgnoreOrder(l1,l2);

    PrintObj.printObj(b);

    PrintObj.printObj(l1==l2);

  }
}
