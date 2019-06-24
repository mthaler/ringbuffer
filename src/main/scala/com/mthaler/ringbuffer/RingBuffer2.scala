package com.mthaler.ringbuffer

class RingBuffer2 private (val capacity: Int, readPos: Int, writePos: Int, count: Int, elems: Array[Any]) {

  def this(capacity: Int) = this(capacity, readPos = 0, writePos = 0, count = 0, elems = Array.ofDim(capacity))
}
