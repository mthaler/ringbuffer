package com.mthaler.ringbuffer.immutable

import scala.collection._

class RingBuffer2[A] private(val capacity: Int, readPos: Int, writePos: Int, _count: Int, elems: Array[Any])
  extends immutable.Iterable[A] with IterableOps[A, RingBuffer2, RingBuffer2[A]] { self =>

  def this(capacity: Int) = this(capacity, readPos = 0, writePos = 0, _count = 0, elems = Array.ofDim(capacity))

  def apply(i: Int): A = {
    if (i >= _count) throw new IndexOutOfBoundsException(i.toString)
    else elems((readPos + i) % capacity).asInstanceOf[A]
  }

  def appended[B >: A](elem: B): RingBuffer2[B] = {
    val newElems = Array.ofDim[Any](capacity)
    Array.copy(elems, 0, newElems, 0, capacity)
    newElems(writePos) = elem
    val newWritePos = (writePos + 1) % capacity
    val newReadPos = if (_count == capacity) (readPos + 1) % capacity else readPos
    val newCount = if (_count != capacity) _count + 1 else _count
    new RingBuffer2[B](capacity, newReadPos, newWritePos, newCount, newElems)
  }

  @`inline` def :+ [B >: A](elem: B): RingBuffer2[B] = appended(elem)

  def iterator: Iterator[A] = new AbstractIterator[A] {

    private var current = 0

    def hasNext: Boolean = current != _count

    def next(): A = {
      val res = apply(current)
      current += 1
      res
    }
  }

  override def className = "RingBuffer2"

  override val iterableFactory: IterableFactory[RingBuffer2] = new RingBuffer2Factory(capacity)

  override protected def fromSpecific(coll: IterableOnce[A]): RingBuffer2[A] = iterableFactory.from(coll)

  override protected def newSpecificBuilder: mutable.Builder[A, RingBuffer2[A]] = iterableFactory.newBuilder

  override def empty: RingBuffer2[A] = iterableFactory.empty
}

class RingBuffer2Factory(capacity: Int) extends IterableFactory[RingBuffer2] {

  def from[A](source: IterableOnce[A]): RingBuffer2[A] =
    (newBuilder[A] ++= source).result()

  def empty[A]: RingBuffer2[A] = new RingBuffer2[A](capacity)

  def newBuilder[A]: mutable.Builder[A, RingBuffer2[A]] =
    new mutable.ImmutableBuilder[A, RingBuffer2[A]](empty) {
      def addOne(elem: A): this.type = { elems = elems :+ elem; this }
    }
}