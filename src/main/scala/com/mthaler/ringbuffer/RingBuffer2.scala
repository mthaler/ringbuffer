package com.mthaler.ringbuffer

import scala.collection.AbstractIterator

class RingBuffer2[A] private (val capacity: Int, readPos: Int, writePos: Int, _count: Int, elems: Array[Any]) {

  def this(capacity: Int) = this(capacity, readPos = 0, writePos = 0, _count = 0, elems = Array.ofDim(capacity))

  def apply(i: Int): A = {
    if (i >= _count) throw new IndexOutOfBoundsException(i.toString)
    else elems((readPos + i) % capacity).asInstanceOf[A]
  }

  def iterator: Iterator[A] = new AbstractIterator[A] {

    private var current = 0

    def hasNext: Boolean = current < _count

    def next(): A = {
      val res = apply(current)
      current += 1
      res
    }
  }
}
