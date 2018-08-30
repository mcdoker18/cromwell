package centaur.reporting

import com.typesafe.config.Config

/**
  * Collects all of the parameters to pass to a new ErrorReporter.
  * @param name
  * @param rootConfig
  * @param reporterConfig
  * @param cromwellDatabase
  */
case class ErrorReporterParams
(
  name: String,
  rootConfig: Config,
  reporterConfig: Config,
  cromwellDatabase: CromwellDatabase
)
