package com.learn.scala.leetcode.base.structure

class NestedInteger(_value: Integer,_nestedIntegerList: List[NestedInteger]) {
  private var value:Integer = _value

  private var nestedIntegerList:List[NestedInteger] = _nestedIntegerList

  def this(_value: Int){
    this(_value,null)
  }

  def this(_nestedIntegerList: List[NestedInteger]){
    this(null,_nestedIntegerList)
  }

  def this(){
    this(null,null)
  }

  // Constructor initializes a single integer.


  // @return true if this NestedInteger holds a single integer, rather than a nested list.
  def isInteger: Boolean = value != null

  // @return the single integer that this NestedInteger holds, if it holds a single integer
  // Return null if this NestedInteger holds a nested list
  def getInteger: Integer = value

  // Set this NestedInteger to hold a single integer.
  def setInteger(value: Integer): Unit = {
    this.value = value
  }

  // Set this NestedInteger to hold a nested list and adds a nested integer to it.
  def add(ni: NestedInteger): Unit = {
    if (nestedIntegerList == null) nestedIntegerList = List()

    nestedIntegerList = nestedIntegerList :+ (ni)
  }

  // @return the nested list that this NestedInteger holds, if it holds a nested list
  // Return null if this NestedInteger holds a single integer
  def getList: List[NestedInteger] = nestedIntegerList
}
