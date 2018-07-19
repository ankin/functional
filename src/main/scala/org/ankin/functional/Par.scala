package org.ankin.functional

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}



object Par {

  type Par[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](value: A) extends Future[A] {
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false
    override def isCancelled: Boolean = false
    override def isDone: Boolean = true
    override def get(): A = value
    override def get(timeout: Long, unit: TimeUnit): A = value
  }



  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def mapTwo[A, B, C](parA: Par[A], parB: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
    val a = parA(es).get
    val b = parB(es).get
    UnitFuture(f(a, b))
  }

  def fork[A](a: => Par[A]): Par[A] = (es: ExecutorService) => {
    es.submit(() => a(es).get)
  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = { (a: A) => lazyUnit(f(a)) }

}

