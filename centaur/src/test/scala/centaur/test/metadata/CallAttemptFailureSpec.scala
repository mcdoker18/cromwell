package centaur.test.metadata

import org.scalatest.{FlatSpec, Matchers}
import CallAttemptFailureSpec._

class CallAttemptFailureSpec extends FlatSpec with Matchers {

  behavior of "CallAttemptFailure"

  it should "parse metadata" in {
    val res = CallAttemptFailure.buildFailures(failingInSeveralWaysMetadataJson).unsafeRunSync()
    res.size should be(10)
    val firsts = res.take(9)
    val last = res.drop(9).head
    firsts.map(_.callFullyQualifiedName).distinct should contain theSameElementsAs
      Vector("FailingInSeveralWays.ScatterJobThatWillFailSometimes")
    firsts.map(_.jobIndex) should contain theSameElementsInOrderAs (41 to 49)
    last.callFullyQualifiedName should be("FailingInSeveralWays.ReadItBackToMe")
    last.workflowId should be("9e83cb0d-0347-431e-8ee6-48f535060845")
  }

}

object CallAttemptFailureSpec {
  private val failingInSeveralWaysMetadataJson = {
    val resource = classOf[CallAttemptFailureSpec].getResource("failingInSeveralWaysMetadata.json")
    val path = resource.getPath
    better.files.File(path).contentAsString
  }
}
