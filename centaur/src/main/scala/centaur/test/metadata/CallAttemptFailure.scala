package centaur.test.metadata

import java.time.OffsetDateTime

import cats.instances.option._
import cats.syntax.apply._
import io.circe._
import io.circe.parser._
import cats.effect._

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
      workflowIdJson <- workflowObject.kleisli("id").toVector
      workflowId <- workflowIdJson.asString.toVector
      callsJson <- workflowObject.kleisli("calls").toVector
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
    val shardIndexOption = jsonObject.kleisli("shardIndex").flatMap(_.asNumber).flatMap(_.toInt)
    val attemptOption = jsonObject.kleisli("attempt").flatMap(_.asNumber).flatMap(_.toInt)

    val startOption = jsonObject.kleisli("start").flatMap(_.asString).map(OffsetDateTime.parse)
    val endOption = jsonObject.kleisli("end").flatMap(_.asString).map(OffsetDateTime.parse)

    val stdoutOption = jsonObject.kleisli("stdout").flatMap(_.asString)
    val stderrOption = jsonObject.kleisli("stderr").flatMap(_.asString)
    val callRootOption = jsonObject.kleisli("callRoot").flatMap(_.asString)

    val messageOption = for {
      failuresJson <- jsonObject.kleisli("failures")
      failuresVector <- failuresJson.asArray
      firstFailure <- failuresVector.headOption
      failureObject <- firstFailure.asObject
      failureMessageJson <- failureObject.kleisli("message")
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
