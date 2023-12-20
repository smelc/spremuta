# Why not use Swagger to consume the GitHub APIs?

Good question! Actually I tried to do that, but failed to
generate the Haskell bindings to GitHub's API.

Version 3.0.51 of [swagger-codegen](https://github.com/swagger-api/swagger-codegen) didn't know about Haskell:

```shell
→ java -jar swagger-codegen-cli.jar generate -i rest-api-description/descriptions-next/ghes-3.10/ghes-3.10.yaml -l haskell-http-client -o hs-gh-rest-api
21:46:35.542 [main] INFO  i.s.c.v.g.t.AbstractTypeScriptClientCodegen - Template folder: null
21:46:35.544 [main] INFO  i.s.c.v.g.t.AbstractTypeScriptClientCodegen - Template engine: io.swagger.codegen.v3.templates.HandlebarTemplateEngine@75f65e45
Exception in thread "main" java.lang.RuntimeException: Can't load config class with name haskell-http-client Available: dart
aspnetcore
csharp
csharp-dotnet2
go
go-server
dynamic-html
html
html2
java
jaxrs-cxf-client
jaxrs-cxf
inflector
jaxrs-cxf-cdi
jaxrs-spec
jaxrs-jersey
jaxrs-di
jaxrs-resteasy-eap
jaxrs-resteasy
java-vertx
micronaut
spring
nodejs-server
openapi
openapi-yaml
kotlin-client
kotlin-server
php
python
python-flask
r
ruby
scala
scala-akka-http-server
swift3
swift4
swift5
typescript-angular
typescript-axios
typescript-fetch
javascript

	at io.swagger.codegen.v3.CodegenConfigLoader.forName(CodegenConfigLoader.java:31)
	at io.swagger.codegen.v3.cli.SwaggerCodegen.main(SwaggerCodegen.java:96)
Caused by: java.lang.ClassNotFoundException: haskell-http-client
	at java.base/jdk.internal.loader.BuiltinClassLoader.loadClass(BuiltinClassLoader.java:581)
	at java.base/jdk.internal.loader.ClassLoaders$AppClassLoader.loadClass(ClassLoaders.java:178)
	at java.base/java.lang.ClassLoader.loadClass(ClassLoader.java:527)
	at java.base/java.lang.Class.forName0(Native Method)
	at java.base/java.lang.Class.forName(Class.java:315)
	at io.swagger.codegen.v3.CodegenConfigLoader.forName(CodegenConfigLoader.java:29)
	... 1 more
```

An earlier version that knew about Haskell was failing, maybe because my target GitHub REST version was too recent:

```shell
→ java -jar swagger-codegen-cli-2.4.21.jar generate -i rest-api-description/descriptions-next/ghes-3.10/ghes-3.10.yaml -l haskell-http-client -o hs-gh-rest-api
[main] INFO io.swagger.parser.Swagger20Parser - reading from rest-api-description/descriptions-next/ghes-3.10/ghes-3.10.yaml
[main] INFO io.swagger.parser.Swagger20Parser - reading from rest-api-description/descriptions-next/ghes-3.10/ghes-3.10.yaml
[main] ERROR io.swagger.parser.SwaggerCompatConverter - failed to read resource listing
com.fasterxml.jackson.core.JsonParseException: Unexpected character ('-' (code 45)) in numeric value: expected digit (0-9) to follow minus sign, for valid numeric value
 at [Source: (StringReader); line: 1, column: 3]
	at com.fasterxml.jackson.core.JsonParser._constructError(JsonParser.java:1851)
	at com.fasterxml.jackson.core.base.ParserMinimalBase._reportError(ParserMinimalBase.java:707)
	at com.fasterxml.jackson.core.base.ParserMinimalBase.reportUnexpectedNumberChar(ParserMinimalBase.java:536)
	at com.fasterxml.jackson.core.json.ReaderBasedJsonParser._handleInvalidNumberStart(ReaderBasedJsonParser.java:1679)
	at com.fasterxml.jackson.core.json.ReaderBasedJsonParser._parseNegNumber(ReaderBasedJsonParser.java:1433)
	at com.fasterxml.jackson.core.json.ReaderBasedJsonParser.nextToken(ReaderBasedJsonParser.java:758)
	at com.fasterxml.jackson.databind.ObjectMapper._readTreeAndClose(ObjectMapper.java:4511)
	at com.fasterxml.jackson.databind.ObjectMapper.readTree(ObjectMapper.java:2940)
	at io.swagger.parser.SwaggerCompatConverter.readResourceListing(SwaggerCompatConverter.java:210)
	at io.swagger.parser.SwaggerCompatConverter.read(SwaggerCompatConverter.java:123)
	at io.swagger.parser.SwaggerParser.read(SwaggerParser.java:96)
	at io.swagger.codegen.config.CodegenConfigurator.toClientOptInput(CodegenConfigurator.java:440)
	at io.swagger.codegen.cmd.Generate.run(Generate.java:297)
	at io.swagger.codegen.SwaggerCodegen.main(SwaggerCodegen.java:35)
[main] WARN io.swagger.codegen.ignore.CodegenIgnoreProcessor - Output directory does not exist, or is inaccessible. No file (.swagger-codegen-ignore) will be evaluated.
Exception in thread "main" java.lang.RuntimeException: missing swagger input or config!
	at io.swagger.codegen.DefaultGenerator.generate(DefaultGenerator.java:766)
	at io.swagger.codegen.cmd.Generate.run(Generate.java:299)
	at io.swagger.codegen.SwaggerCodegen.main(SwaggerCodegen.java:35)
09:49:44 direnv churlin@karak-dum spremuta |main| → ls
```

Anyway this seemed uninteresting and so I preferred continuing the manual way, even though,
in a production project; Swagger would be the way to go. Fortunately, this is a fun, quick and dirty project.
