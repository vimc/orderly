    SELECT 
           filename,
           file_hash,
           is_pinned,
           is_latest,
           report_version_artefact.report_version
      FROM depends
INNER JOIN report_version
        ON depends.report_version=report_version.id
INNER JOIN file_artefact
        ON depends.use = file_artefact.id
INNER JOIN report_version_artefact
        ON report_version_artefact.id = file_artefact.artefact
