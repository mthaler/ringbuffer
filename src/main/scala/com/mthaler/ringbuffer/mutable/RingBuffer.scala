package com.mthaler.ringbuffer.mutable

final class RingBuffer[A] private (val capacity: Int, elems: Array[Any]) extends Seq[A] {

  def this(capacity: Int) = this(capacity, elems = Array.ofDim(capacity))

  private var read = 0
  private var write = 0
  private var count_ = 0

  def length = count_

  def clear(): Unit = {
    read = 0
    write = 0
    count_ = 0
  }

  /**
   * Gets the element from the specified index in constant time.
   */
  def apply(i: Int): A = {
    if (i >= count_) throw new IndexOutOfBoundsException(i.toString)
    else elems((read + i) % capacity).asInstanceOf[A]
  }

  /**
   * Overwrites an element with a new value
   */
  def update(i: Int, elem: A): Unit = {
    if (i >= count_) throw new IndexOutOfBoundsException(i.toString)
    else elems((read + i) % capacity) = elem
  }

  /**
   * Adds an element, possibly overwriting the oldest elements in the buffer
   * if the buffer is at capacity.
   */
  def +=(elem: A): Unit = {
    elems(write) = elem
    write = (write + 1) % capacity
    if (count_ == capacity) read = (read + 1) % capacity
    else count_ += 1
  }

  /**
   * Adds multiple elements, possibly overwriting the oldest elements in
   * the buffer. If the given iterable contains more elements that this
   * buffer can hold, then only the last maxSize elements will end up in
   * the buffer.
   */
  def ++=(iter: Iterable[A]): Unit = {
    for (elem <- iter) this += elem
  }

  /**
   * Removes the next element from the buffer
   */
  def next: A = {
    if (read == write) throw new NoSuchElementException
    else {
      val res = elems(read).asInstanceOf[A]
      read = (read + 1) % capacity
      count_ -= 1
      res
    }
  }

  override def iterator = new Iterator[A] {
    var idx = 0
    def hasNext = idx != count_
    def next() = {
      val res = apply(idx)
      idx += 1
      res
    }
  }

  override def drop(n: Int): RingBuffer[A] = {
    if (n >= capacity) clear()
    else read = (read + n) % capacity
    this
  }

  def removeWhere(fn: A=>Boolean): Int = {
    var rmCount_ = 0
    var j = 0
    for (i <- 0 until count_) {
      val elem = apply(i)
      if (fn(elem)) rmCount_ += 1
      else {
        if (j < i) update(j, elem)
        j += 1
      }
    }
    count_ -= rmCount_
    write = (read + count_) % capacity
    rmCount_
  }
}