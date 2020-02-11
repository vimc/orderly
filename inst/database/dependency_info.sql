    SELECT
           depends.report_version,
           report_version.report,
           report_version.id,
           file_artefact.filename,
           file_artefact.file_hash
      FROM depends
INNER JOIN file_artefact
        ON depends.use=file_artefact.id
INNER JOIN report_version_artefact
        ON file_artefact.artefact=report_version_artefact.id
INNER JOIN report_version
        ON report_version_artefact.report_version=report_version.id
