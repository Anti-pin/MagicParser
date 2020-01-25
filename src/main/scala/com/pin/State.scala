package com.pin

object State {
  def unit[S, A](a: A): State[S, A] = State { s => (s, a) }
}

case class State[S, +A](run: S => (S, A)) {

  import State._

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (s1, a) = run(s)
    f(a).run(s1)
  })
}
