package centaur.reporting

import cats.effect.IO
import com.typesafe.config.Config
import cromwell.database.slick.{EngineSlickDatabase, MetadataSlickDatabase}
import cromwell.database.sql.tables.{JobKeyValueEntry, MetadataEntry}
import cromwell.database.sql.{EngineSqlDatabase, MetadataSqlDatabase}

import scala.concurrent.ExecutionContext

class CromwellDatabase(rootConfig: Config) {

  private val cromwellConfig = rootConfig.getConfig("cromwell")

  private lazy val engineDatabase: EngineSqlDatabase = EngineSlickDatabase.fromParentConfig(cromwellConfig)
  private lazy val metadataDatabase: MetadataSqlDatabase = MetadataSlickDatabase.fromParentConfig(cromwellConfig)

  def jobKeyValueEntriesIo(workflowExecutionUuidOption: Option[String])
                          (implicit executionContext: ExecutionContext): IO[Seq[JobKeyValueEntry]] = {
    workflowExecutionUuidOption.map(jobKeyValueEntriesIo).getOrElse(IO.pure(Seq.empty))
  }

  def jobKeyValueEntriesIo(workflowExecutionUuid: String)
                          (implicit executionContext: ExecutionContext): IO[Seq[JobKeyValueEntry]] = {
    IO.fromFuture(IO(engineDatabase.queryJobKeyValueEntries(workflowExecutionUuid)))
  }

  def metadataEntriesIo(workflowExecutionUuidOption: Option[String])
                       (implicit executionContext: ExecutionContext): IO[Seq[MetadataEntry]] = {
    workflowExecutionUuidOption.map(metadataEntriesIo).getOrElse(IO.pure(Seq.empty))
  }

  def metadataEntriesIo(workflowExecutionUuid: String)
                       (implicit executionContext: ExecutionContext): IO[Seq[MetadataEntry]] = {
    IO.fromFuture(IO(metadataDatabase.queryMetadataEntries(workflowExecutionUuid)))
  }

}
