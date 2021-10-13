SELECT
  ver.id,
  ver.report as name,
  ver.date,
  child.id as child_id,
  child.report as child
FROM
  report_version ver
JOIN report_version_artefact ver_artefact ON ver_artefact.report_version = ver.id
JOIN file_artefact artefact ON ver_artefact.id = artefact.artefact
JOIN depends dep ON dep.use = artefact.id
JOIN report_version child ON child.id = dep.report_version
WHERE ver.id = '%s'
