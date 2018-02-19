CREATE TABLE "report" (
"name" TEXT NOT NULL ,
PRIMARY KEY ("name")
);

CREATE TABLE "report_version" (
"id" TEXT NOT NULL ,
"timestamp" TIMESTAMP NOT NULL ,
"report" TEXT NOT NULL ,
"author" TEXT NOT NULL ,
"displayname" TEXT NOT NULL ,
"comments" TEXT ,
"description" TEXT ,
"script" TEXT NOT NULL ,
"requester" TEXT ,
"main_artefact_file" INTEGER ,
"changelog_entry" TEXT ,
PRIMARY KEY ("id")
);

CREATE TABLE "package" (
"id" TEXT NOT NULL ,
PRIMARY KEY ("id")
);

CREATE TABLE "resource" (
"id"  SERIAL ,
"report_version" TEXT NOT NULL ,
"path" TEXT NOT NULL ,
"hash" TEXT NOT NULL ,
PRIMARY KEY ("id")
);

CREATE TABLE "artefact" (
"id"  SERIAL ,
"report_version" TEXT NOT NULL ,
"type" TEXT NOT NULL ,
"description" TEXT NOT NULL ,
"order" INTEGER NOT NULL ,
PRIMARY KEY ("id")
);

CREATE TABLE "artefact_type" (
"id" TEXT NOT NULL ,
PRIMARY KEY ("id")
);

CREATE TABLE "artefact_file" (
"id"  SERIAL ,
"artefact" INTEGER ,
"path" TEXT NOT NULL ,
"hash" TEXT NOT NULL ,
"order" INTEGER NOT NULL ,
"size" BIGINT NOT NULL ,
"mime_type" TEXT NOT NULL ,
PRIMARY KEY ("id")
);

CREATE TABLE "dependency" (
"id" SERIAL NOT NULL ,
"report_version" TEXT NOT NULL ,
"destination" TEXT NOT NULL ,
"artefact_file" INTEGER NOT NULL ,
PRIMARY KEY ("id")
);

CREATE TABLE "report_version_package" (
"report_version" TEXT NOT NULL ,
"package" TEXT NOT NULL ,
PRIMARY KEY ("report_version", "package")
);

CREATE TABLE "report_data_input" (
"id"  SERIAL NOT NULL ,
"report_version" TEXT NOT NULL ,
"hash" TEXT NOT NULL ,
"name" TEXT NOT NULL ,
PRIMARY KEY ("id")
);

CREATE TABLE "data_input" (
"hash" TEXT NOT NULL ,
"size" BIGINT NOT NULL ,
PRIMARY KEY ("hash")
);

ALTER TABLE "report_version" ADD FOREIGN KEY ("report") REFERENCES "report" ("name");
ALTER TABLE "report_version" ADD FOREIGN KEY ("main_artefact_file") REFERENCES "artefact_file" ("id");
ALTER TABLE "resource" ADD FOREIGN KEY ("report_version") REFERENCES "report_version" ("id");
ALTER TABLE "artefact" ADD FOREIGN KEY ("report_version") REFERENCES "report_version" ("id");
ALTER TABLE "artefact" ADD FOREIGN KEY ("type") REFERENCES "artefact_type" ("id");
ALTER TABLE "artefact_file" ADD FOREIGN KEY ("artefact") REFERENCES "artefact" ("id");
ALTER TABLE "dependency" ADD FOREIGN KEY ("report_version") REFERENCES "report_version" ("id");
ALTER TABLE "dependency" ADD FOREIGN KEY ("artefact_file") REFERENCES "artefact_file" ("id");
ALTER TABLE "report_version_package" ADD FOREIGN KEY ("report_version") REFERENCES "report_version" ("id");
ALTER TABLE "report_version_package" ADD FOREIGN KEY ("package") REFERENCES "package" ("id");
ALTER TABLE "report_data_input" ADD FOREIGN KEY ("report_version") REFERENCES "report_version" ("id");
ALTER TABLE "report_data_input" ADD FOREIGN KEY ("hash") REFERENCES "data_input" ("hash");
