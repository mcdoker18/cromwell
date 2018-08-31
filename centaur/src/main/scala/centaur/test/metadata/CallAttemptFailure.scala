package centaur.test.metadata

import java.time.OffsetDateTime

import cats.data.Kleisli
import cats.effect._
import cats.instances.option._
import cats.syntax.apply._
import io.circe._
import io.circe.parser._

/**
  * The first failure message of a call. Based on the JMUI FailureMessage:
  *
  * https://github.com/DataBiosphere/job-manager/blob/f83e4284e2419389b7e515720c9d960d2eb81a29/servers/cromwell/jobs/controllers/jobs_controller.py#L155-L162
  */
case class CallAttemptFailure
(
  workflowId: String,
  callFullyQualifiedName: String,
  jobIndex: Int,
  jobAttempt: Int,
  message: String,
  startOption: Option[OffsetDateTime],
  endOption: Option[OffsetDateTime],
  stdoutOption: Option[String],
  stderrOption: Option[String],
  callRootOption: Option[String]
)

object CallAttemptFailure {
  def buildFailures(jsonOption: Option[String]): IO[Vector[CallAttemptFailure]] = {
    jsonOption.map(buildFailures).getOrElse(IO.pure(Vector.empty))
  }

  def buildFailures(json: String): IO[Vector[CallAttemptFailure]] = {
    IO.fromEither(parse(json)) map fromWorkflowJson
  }

  private def fromWorkflowJson(json: Json): Vector[CallAttemptFailure] = {
    for {
      workflowObject <- json.asObject.toVector
      workflowIdJson <- workflowObject("id").toVector
      workflowId <- workflowIdJson.asString.toVector
      callsJson <- workflowObject("calls").toVector
      callsObject <- callsJson.asObject.toVector
      callNameAndJson <- callsObject.toVector
      (callName, callJson) = callNameAndJson
      callAttemptsVector <- callJson.asArray.toVector
      callAttemptJson <- callAttemptsVector
      callAttemptObject <- callAttemptJson.asObject.toVector
      callFailure <- fromCallAttempt(workflowId, callName, callAttemptObject)
    } yield callFailure
  }

  private def fromCallAttempt(workflowId: String,
                              callName: String,
                              jsonObject: JsonObject): Option[CallAttemptFailure] = {

    val jsonObjectInt = for {
      json <- jsonObject.kleisli
      number <- Kleisli.liftF(json.asNumber)
      int <- Kleisli.liftF(number.toInt)
    } yield int

    val jsonObjectString = for {
      json <- jsonObject.kleisli
      string <- Kleisli.liftF(json.asString)
    } yield string

    val jsonObjectOffsetDateTime = for {
      string <- jsonObjectString
      offsetDateTime = OffsetDateTime.parse(string)
    } yield offsetDateTime

    val shardIndexOption = jsonObjectInt("shardIndex")
    val attemptOption = jsonObjectInt("attempt")

    val startOption = jsonObjectOffsetDateTime("start")
    val endOption = jsonObjectOffsetDateTime("end")

    val stdoutOption = jsonObjectString("stdout")
    val stderrOption = jsonObjectString("stderr")
    val callRootOption = jsonObjectString("callRoot")

    val messageOption = for {
      failuresJson <- jsonObject("failures")
      failuresVector <- failuresJson.asArray
      firstFailure <- failuresVector.headOption
      failureObject <- firstFailure.asObject
      failureMessageJson <- failureObject("message")
      failureMessage <- failureMessageJson.asString
    } yield failureMessage

    (shardIndexOption, attemptOption, messageOption) mapN { (shardIndex, attempt, message) =>
      new CallAttemptFailure(
        workflowId = workflowId,
        callFullyQualifiedName = callName,
        jobIndex = shardIndex,
        jobAttempt = attempt,
        message = message,
        startOption = startOption,
        endOption = endOption,
        stdoutOption = stdoutOption,
        stderrOption = stderrOption,
        callRootOption = callRootOption
      )
    }
  }

}
