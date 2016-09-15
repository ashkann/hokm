package ir.ashkan.hokm


object DSL {
  /**
    * Repeatedly performs a piece of code and checks the result until a condition holds
    * @param code The piece of code to execute
    * @param condition The condition to observe
    * @tparam T Type of the result that code returns
    * @return The result for the last execution of code for which the condition was True
    */
  def repeatUntil[T](code: => T)(condition: T => Boolean): T = {
    code match {
      case result if condition(result) => result
      case _ => repeatUntil(code)(condition)
    }
  }

  def repeatUntil2[OUT, I](code: I => (OUT, I))(condition: OUT => Boolean)(initial: I): OUT = {
    val (result, nextInitial) = code(initial)
    if (condition(result))
      result
    else
      repeatUntil2(code)(condition)(nextInitial)
  }
}
