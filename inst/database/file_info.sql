SELECT
       'input' as origin,
       report_version.report,
       file_input.report_version,
       file_input.filename,
       file.hash,
       file.size
  FROM file_input
  JOIN report_version
    ON report_version.id = file_input.report_version
  JOIN file
    ON file.hash = file_input.file_hash

UNION SELECT
       'artefact' as origin,
       report_version.report,
       report_version_artefact.report_version,
       file_artefact.filename,
       file.hash,
       file.size
  FROM file_artefact
  JOIN report_version_artefact
    ON report_version_artefact.id = file_artefact.artefact
  JOIN report_version
    ON report_version.id = report_version_artefact.report_version
  JOIN file
    ON file.hash = file_artefact.file_hash

UNION SELECT
       'depends' as origin,
       report_version.report,
       depends.report_version,
       depends.'as' AS filename,
       file.hash,
       file.size
  FROM depends
  JOIN report_version
    ON report_version.id = depends.report_version
  JOIN file_artefact
    ON file_artefact.id = depends.use
  JOIN file
    ON file.hash = file_artefact.file_hash

ORDER BY report_version
