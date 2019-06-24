package com.mthaler.ringbuffer

import org.scalatest.FunSuite

class RingBufferTest extends FunSuite {

  test("empty") {
    val buf = new RingBuffer[String](4)
    assert(0 === buf.length)
    assert(0 === buf.size)
    assert(buf.isEmpty)
    assert(!buf.iterator.hasNext)
  }

  test("singleElement") {
    val buf = new RingBuffer[String](4)
    buf += "a"
    assert(1 === buf.size)
    assert("a" === buf(0))
    assert(List("a") === buf.toList)
  }

  test("multipleElements") {
    val buf = new RingBuffer[String](4)
    buf ++= List("a", "b", "c")
    assert(3 === buf.size)
    assert("a" === buf(0))
    assert("b" === buf(1))
    assert("c" === buf(2))
    assert(List("a", "b", "c") === buf.toList)
    assert("a" === buf.next)
    assert(2 === buf.size)
    assert("b" === buf.next)
    assert(1 === buf.size)
    assert("c" === buf.next)
    assert(0 === buf.size)
  }

  test("overwriteRollover") {
    val buf = new RingBuffer[String](4)
    buf ++= List("a", "b", "c", "d", "e", "f")
    assert(4 === buf.size)
    assert("c" === buf(0))
    assert(List("c", "d", "e", "f") === buf.toList)
  }

  test("removeWhere") {
    val buf = new RingBuffer[Int](6)
    buf ++= (0 until 10)
    assert(List(4, 5, 6, 7, 8, 9) === buf.toList)
    buf.removeWhere(_ % 3 == 0)
    assert(List(4, 5, 7, 8) === buf.toList)
  }
}

