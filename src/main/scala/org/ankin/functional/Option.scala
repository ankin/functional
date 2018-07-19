package org.ankin.functional

sealed trait Option[+A] {
  self =>

  def isEmpty: Boolean

  final def map[B](f: A => B): Option[B] = {
    if (isEmpty) None
    else {
      self match {
        case Some(v) => Some(f(v))
        case None => None
      }
    }
  }

  final def flatMap[B](f: A => Option[B]): Option[B] = {
    if (isEmpty) None
    else {
      self match {
        case Some(v) => f(v)
        case None => None
      }
    }
  }

  final def getOrElse[B >: A](default: => B): B = self match {
    case Some(v) => v
    case None => default
  }

  final def orElse [B >: A](default: => Option[B]): Option[B] = if (isEmpty) None else default

  final def filter(f: A => Boolean): Option[A] = self match {
    case Some(v) => if (f(v)) self else None
    case None => None
  }

}

case class Some[+A](value: A) extends Option [A] {
  override def isEmpty: Boolean = false
}
case object None extends Option[Nothing] {
  override def isEmpty: Boolean = true
}
