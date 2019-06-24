package com.mthaler.ringbuffer

import org.scalatest.FunSuite

class RingBuffer2Test extends FunSuite {

  test("append") {
    val rb = new RingBuffer2[Int](4) :+ 1 :+ 2 :+ 3 :+ 4
    val it = rb.iterator
    assert(it.next === 1)
    assert(it.next === 2)
    assert(it.next === 3)
    assert(it.next === 4)
    assert(!it.hasNext)
    val rb2 = rb :+ 5
    val it2 = rb2.iterator
    assert(it2.next === 2)
    assert(it2.next === 3)
    assert(it2.next === 4)
    assert(it2.next === 5 )
  }

  test("length") {
    val rb = new RingBuffer2[Int](4) :+ 1 :+ 2 :+ 3
    assert(rb.size === 3)
    val rb2 = rb :+ 4
    assert(rb2.size === 4)
    val rb3 = rb2 :+ 5
    assert(rb3.size === 4)
  }
}
