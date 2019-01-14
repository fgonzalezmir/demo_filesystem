package com.rtjvm.scala.oop.filesystem

import com.rtjvm.scala.oop.files.Directory

// Variable output contains the output of the previous command
class State(val root: Directory, val wd: Directory, output: String) {

  def show: Unit =
    println(output)
    println(State.SHELL_TOKEN)

  def setMessage(message: String): State =
    State(root, wd, message)
}


object State {
  val SHELL_TOKEN = "$ "

  def apply(root: Directory, wd: Directory, output: String = ""  ): State =
    new State(root, wd, output)



}
