package common

case class State[S, +A](run: S => (A, S))
