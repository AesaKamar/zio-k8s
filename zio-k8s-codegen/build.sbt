sbtPlugin := true

organization := "com.coralogix"
name         := "zio-k8s-codegen"

scalaVersion := "2.12.17"

Compile / unmanagedSourceDirectories += baseDirectory.value / "src/shared/scala"

scalacOptions ++= Seq("-feature", "-deprecation")

libraryDependencies ++= Seq(
  "dev.zio"             %% "zio"              % "1.0.17",
  "dev.zio"             %% "zio-nio"          % "1.0.0-RC12",
  "io.swagger.parser.v3" % "swagger-parser"   % "2.0.24",
  "io.circe"            %% "circe-core"       % "0.14.2",
  "io.circe"            %% "circe-parser"     % "0.14.2",
  "io.circe"            %% "circe-yaml"       % "0.14.1",
  "org.scalameta"       %% "scalameta"        % "4.4.21",
  "org.scalameta"       %% "scalafmt-dynamic" % "2.7.5",
  "org.atteo"            % "evo-inflector"    % "1.3",
  "org.typelevel"       %% "cats-core"        % "2.9.0",
  "org.typelevel"       %% "cats-effect"      % "3.4.8",
  "co.fs2"              %% "fs2-core"         % "3.6.1",
  "co.fs2"              %% "fs2-io"           % "3.6.1",
  "com.lihaoyi"         %% "pprint"           % "0.8.1"
)
