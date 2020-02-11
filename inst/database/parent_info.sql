    SELECT
           file_artefact.filename,
           file_artefact.file_hash
      FROM report_version
INNER JOIN report_version_artefact
        ON report_version.id = report_version_artefact.report_version
INNER JOIN file_artefact
        ON file_artefact.artefact = report_version_artefact.id
