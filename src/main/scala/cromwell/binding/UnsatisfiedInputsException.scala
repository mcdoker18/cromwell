package cromwell.binding

class UnsatisfiedInputsException(message: String, cause: Throwable) extends RuntimeException(message, cause) {
  def this(message: String) = this(message, null)
}
