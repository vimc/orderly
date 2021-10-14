SELECT
  ver.id,
  ver.report as name,
  ver.date,
  child.id as child_id,
  child.report as child
FROM
  report_version ver
JOIN depends dep ON dep.report_version = ver.id
JOIN file_artefact artefact ON dep.use = artefact.id
JOIN report_version_artefact ver_artefact ON ver_artefact.id = artefact.artefact
JOIN report_version child ON child.id = ver_artefact.report_version
WHERE ver.id = '%s'
