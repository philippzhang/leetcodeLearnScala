package com.learn.scala.leetcode.lc0003

import scala.collection.mutable

object Solution {
  def lengthOfLongestSubstring(s: String): Int =
    s.zipWithIndex.foldLeft((0, -1, Map[Char, Int]())) { case ((len, start_pos, map), (char, i)) =>
      val last_pos = map.getOrElse(char, -1)
      if (last_pos >= start_pos) (len max (i - last_pos), last_pos, map + (char -> i))
      else (len max (i - start_pos), start_pos, map + (char -> i))
    }._1

  def lengthOfLongestSubstring2(s: String): Int ={
    //记录字符出现的位置
    val map = new mutable.HashMap[Char,Int]()
    val chs = s.toCharArray
    var m:Int = 0
    var start_pos = 0
    for(i <- 0 until chs.length){
      val c = chs(i)
      if(map.contains(c)){
        //获取下一个位置
        start_pos = Math.max(start_pos,map.get(c).get+1)
      }
      m = Math.max(m,i-start_pos+1)
      map.put(c,i)
    }
    m
  }

}
