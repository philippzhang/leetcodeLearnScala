package com.learn.scala.leetcode.lc0284

class PeekingIterator(var iterator: Iterator[Integer]) // initialize any member here.
  extends Iterator[Integer] {
  private var cache:Integer = null

  // Returns the next element in the iteration without advancing the iterator.
  def peek: Integer = {
    if (cache == null && iterator.hasNext) cache = iterator.next()
    cache
  }

  // hasNext() and next() should behave the same as in the Iterator interface.
  // Override them if needed.
  override def next: Integer = {
    if (cache == null && iterator.hasNext) iterator.next
    else {
      val temp = cache
      cache = null
      temp
    }
  }

  override def hasNext: Boolean = cache != null || iterator.hasNext
}
