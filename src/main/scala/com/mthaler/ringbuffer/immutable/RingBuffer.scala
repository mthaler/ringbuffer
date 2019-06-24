package com.mthaler.ringbuffer.immutable

import scala.collection._

final class RingBuffer[A] private(val capacity: Int, readPos: Int, writePos: Int, _count: Int, elems: Array[Any])
  extends immutable.Iterable[A] with IterableOps[A, RingBuffer, RingBuffer[A]] with IterableFactoryDefaults[A, RingBuffer]
  with StrictOptimizedIterableOps[A, RingBuffer, RingBuffer[A]]{ self =>

  def this(capacity: Int) = this(capacity, readPos = 0, writePos = 0, _count = 0, elems = Array.ofDim(capacity))

  def apply(i: Int): A = {
    if (i >= _count) throw new IndexOutOfBoundsException(i.toString)
    else elems((readPos + i) % capacity).asInstanceOf[A]
  }

  def appended[B >: A](elem: B): RingBuffer[B] = {
    val newElems = Array.ofDim[Any](capacity)
    Array.copy(elems, 0, newElems, 0, capacity)
    newElems(writePos) = elem
    val newWritePos = (writePos + 1) % capacity
    val newReadPos = if (_count == capacity) (readPos + 1) % capacity else readPos
    val newCount = if (_count != capacity) _count + 1 else _count
    new RingBuffer[B](capacity, newReadPos, newWritePos, newCount, newElems)
  }

  @`inline` def :+ [B >: A](elem: B): RingBuffer[B] = appended(elem)

  def iterator: Iterator[A] = view.iterator

  override def view: IndexedSeqView[A] = new IndexedSeqView[A] {

    def length: Int = self._count

    def apply(i: Int): A = self(i)
  }

  override def knownSize: Int = _count

  override def className = "RingBuffer"

  override val iterableFactory: IterableFactory[RingBuffer] = new RingBufferFactory(capacity)
}

class RingBufferFactory(capacity: Int) extends IterableFactory[RingBuffer] {

  def from[A](source: IterableOnce[A]): RingBuffer[A] =
    source match {
      case ringBuffer: RingBuffer[A] if ringBuffer.capacity == capacity => ringBuffer
      case _ => (newBuilder[A] ++= source).result()
    }

  def empty[A]: RingBuffer[A] = new RingBuffer[A](capacity)

  def newBuilder[A]: mutable.Builder[A, RingBuffer[A]] =
    new mutable.ImmutableBuilder[A, RingBuffer[A]](empty) {
      def addOne(elem: A): this.type = { elems = elems :+ elem; this }
    }
}
