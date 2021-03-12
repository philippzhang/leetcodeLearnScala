package com.learn.scala.leetcode.test

class Rational(n:Int,d:Int) {
  require(d!=0)

  private val g = gcd(n.abs,d.abs)
  val number = n/g
  val denom = d/g
  //副构造函数
  def this(n:Int) = this(n,1)
  def + (that:Rational):Rational =
    new Rational(number*that.denom+that.number*denom,denom*that.denom)
  def + (i:Int):Rational =
    new Rational(number+i*denom,denom)
  def - (that:Rational):Rational =
    new Rational(number*that.denom-that.number*denom,denom*that.denom)
  def - (i:Int):Rational =
    new Rational(number-i*denom,denom)
  def * (that:Rational):Rational =
    new Rational(number*that.number,denom*that.denom)
  def * (i:Int):Rational =
    new Rational(i*number,denom)
  def / (that:Rational):Rational =
    new Rational(number*that.denom,denom*that.number)
  def / (i:Int):Rational =
    new Rational(number,denom*i)

  override def toString: String = s"$number/$denom"

  private def gcd(a:Int,b:Int): Int =
    if(b==0)a else gcd(b,a % b)


}

object Rational extends App{
  val d1 = new Rational(1,2)
  val d2 = new Rational(2,3)
  println(d1)
  println(d1+d2)
  val d3 = new Rational(3)
  println(d3)
  println(d2*2)
  //隐示转换
  implicit def intToRational(x:Int) = new Rational(x)
  println(2*d2)



}