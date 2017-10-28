(:root-dir
 "@prj-dir@"
 :cache-dir "@prj-dir@/.ensime_cache"
 :scala-compiler-jars
 (
  "@repo@/org/scala-lang/scala-compiler/2.10.6/scala-compiler-2.10.6.jar"
  "@repo@/org/scala-lang/scala-library/2.10.6/scala-library-2.10.6.jar"
  "@repo@/org/scala-lang/scala-reflect/2.10.6/scala-reflect-2.10.6.jar"
  "@repo@/org/scala-lang/scalap/2.10.6/scalap-2.10.6.jar"
  )
 :ensime-server-jars
 (
  "@java-home@/lib/tools.jar"
  "@repo@/org/ensime/monkeys_2.10/2.0.0-M4/monkeys_2.10-2.0.0-M4.jar"
  "@repo@/org/ensime/api_2.10/2.0.0-M4/api_2.10-2.0.0-M4.jar"
  "@repo@/org/ensime/core_2.10/2.0.0-M4/core_2.10-2.0.0-M4.jar"
  "@repo@/org/ensime/jerky_2.10/2.0.0-M4/jerky_2.10-2.0.0-M4.jar"
  "@repo@/org/ensime/json_2.10/2.0.0-M4/json_2.10-2.0.0-M4.jar"
  "@repo@/org/ensime/s-express_2.10/2.0.0-M4/s-express_2.10-2.0.0-M4.jar"
  "@repo@/org/ensime/server_2.10/2.0.0-M4/server_2.10-2.0.0-M4.jar"
  "@repo@/org/ensime/swanky_2.10/2.0.0-M4/swanky_2.10-2.0.0-M4.jar"
  "@repo@/org/ensime/util_2.10/2.0.0-M4/util_2.10-2.0.0-M4.jar"
  "@repo@/javax/activation/activation/1.1/activation-1.1.jar"
  "@repo@/com/typesafe/akka/akka-actor_2.10/2.3.16/akka-actor_2.10-2.3.16.jar"
  "@repo@/com/typesafe/akka/akka-slf4j_2.10/2.3.16/akka-slf4j_2.10-2.3.16.jar"
  "@repo@/org/ow2/asm/asm/5.2/asm-5.2.jar"
  "@repo@/org/ow2/asm/asm-commons/5.2/asm-commons-5.2.jar"
  "@repo@/org/ow2/asm/asm-tree/5.2/asm-tree-5.2.jar"
  "@repo@/org/ow2/asm/asm-util/5.2/asm-util-5.2.jar"
  "@repo@/com/tinkerpop/blueprints/blueprints-core/2.6.0/blueprints-core-2.6.0.jar"
  "@repo@/commons-beanutils/commons-beanutils-core/1.8.0/commons-beanutils-core-1.8.0.jar"
  "@repo@/commons-configuration/commons-configuration/1.6/commons-configuration-1.6.jar"
  "@repo@/commons-digester/commons-digester/1.8/commons-digester-1.8.jar"
  "@repo@/commons-lang/commons-lang/2.4/commons-lang-2.4.jar"
  "@repo@/org/apache/commons/commons-vfs2/2.1/commons-vfs2-2.1.jar"
  "@repo@/com/googlecode/concurrentlinkedhashmap/concurrentlinkedhashmap-lru/1.4.1/concurrentlinkedhashmap-lru-1.4.1.jar"
  "@repo@/com/typesafe/config/1.2.1/config-1.2.1.jar"
  "@repo@/com/googlecode/java-diff-utils/diffutils/1.3.0/diffutils-1.3.0.jar"
  "@repo@/com/lihaoyi/fastparse-utils_2.10/0.4.3/fastparse-utils_2.10-0.4.3.jar"
  "@repo@/com/lihaoyi/fastparse_2.10/0.4.3/fastparse_2.10-0.4.3.jar"
  "@repo@/com/carrotsearch/hppc/0.6.0/hppc-0.6.0.jar"
  "@repo@/com/fasterxml/jackson/core/jackson-annotations/2.6.0/jackson-annotations-2.6.0.jar"
  "@repo@/com/fasterxml/jackson/core/jackson-core/2.6.0/jackson-core-2.6.0.jar"
  "@repo@/com/fasterxml/jackson/core/jackson-databind/2.6.0/jackson-databind-2.6.0.jar"
  "@repo@/org/slf4j/jcl-over-slf4j/1.7.25/jcl-over-slf4j-1.7.25.jar"
  "@repo@/org/codehaus/jettison/jettison/1.3.3/jettison-1.3.3.jar"
  "@repo@/org/slf4j/jul-to-slf4j/1.7.25/jul-to-slf4j-1.7.25.jar"
  "@repo@/org/slf4j/log4j-over-slf4j/1.7.25/log4j-over-slf4j-1.7.25.jar"
  "@repo@/ch/qos/logback/logback-classic/1.2.3/logback-classic-1.2.3.jar"
  "@repo@/ch/qos/logback/logback-core/1.2.3/logback-core-1.2.3.jar"
  "@repo@/org/apache/lucene/lucene-analyzers-common/6.4.2/lucene-analyzers-common-6.4.2.jar"
  "@repo@/org/apache/lucene/lucene-core/6.4.2/lucene-core-6.4.2.jar"
  "@repo@/org/typelevel/macro-compat_2.10/1.1.1/macro-compat_2.10-1.1.1.jar"
  "@repo@/javax/mail/mail/1.4.7/mail-1.4.7.jar"
  "@repo@/io/netty/netty-buffer/4.1.13.Final/netty-buffer-4.1.13.Final.jar"
  "@repo@/io/netty/netty-codec/4.1.13.Final/netty-codec-4.1.13.Final.jar"
  "@repo@/io/netty/netty-codec-http/4.1.13.Final/netty-codec-http-4.1.13.Final.jar"
  "@repo@/io/netty/netty-common/4.1.13.Final/netty-common-4.1.13.Final.jar"
  "@repo@/io/netty/netty-handler/4.1.13.Final/netty-handler-4.1.13.Final.jar"
  "@repo@/io/netty/netty-resolver/4.1.13.Final/netty-resolver-4.1.13.Final.jar"
  "@repo@/io/netty/netty-transport/4.1.13.Final/netty-transport-4.1.13.Final.jar"
  "@repo@/org/scala-refactoring/org.scala-refactoring.library_2.10.6/0.12.0/org.scala-refactoring.library_2.10.6-0.12.0.jar"
  "@repo@/com/orientechnologies/orientdb-client/2.2.24/orientdb-client-2.2.24.jar"
  "@repo@/com/orientechnologies/orientdb-core/2.2.24/orientdb-core-2.2.24.jar"
  "@repo@/com/orientechnologies/orientdb-graphdb/2.2.24/orientdb-graphdb-2.2.24.jar"
  "@repo@/com/orientechnologies/orientdb-server/2.2.24/orientdb-server-2.2.24.jar"
  "@repo@/com/orientechnologies/orientdb-tools/2.2.24/orientdb-tools-2.2.24.jar"
  "@repo@/org/scalamacros/quasiquotes_2.10/2.1.0/quasiquotes_2.10-2.1.0.jar"
  "@repo@/org/scala-debugger/scala-debugger-api_2.10/1.1.0-M3/scala-debugger-api_2.10-1.1.0-M3.jar"
  "@repo@/org/scala-debugger/scala-debugger-macros_2.10/1.1.0-M3/scala-debugger-macros_2.10-1.1.0-M3.jar"
  "@repo@/com/chuusai/shapeless_2.10/2.3.2/shapeless_2.10-2.3.2.jar"
  "@repo@/org/slf4j/slf4j-api/1.7.25/slf4j-api-1.7.25.jar"
  "@repo@/org/xerial/snappy/snappy-java/1.1.0.1/snappy-java-1.1.0.1.jar"
  "@repo@/com/lihaoyi/sourcecode_2.10/0.1.3/sourcecode_2.10-0.1.3.jar"
  "@repo@/stax/stax-api/1.0.1/stax-api-1.0.1.jar"
  )
 :ensime-server-version "2.0.0-M4"
 :name "@prj-name@"
 :java-home "@java-home@"
 :java-flags
 (
  "-Dclassworlds.conf=@maven-root@/libexec/bin/m2.conf"
  "-Dmaven.home=@maven-root@/libexec"
  "-Dmaven.multiModuleProjectDirectory=@prj-dir@"
  "-Xss2m"
  )
 :java-sources ("@java-home@/src.zip")
 :java-compiler-args nil
 :reference-source-roots ("@java-home@/src.zip")
 :scala-version "2.10.6"
 :compiler-args ("-feature" "-deprecation" "-Xlint" "-Ywarn-dead-code" "-Ywarn-numeric-widen" "-Xfuture" "-Ymacro-no-expand")
 :subprojects ((:name
                "@prj-name@"
                :source-roots ("@prj-dir@/src/main/java" "@prj-dir@/src/test/java")
                :targets ("@prj-dir@/target/classes2")
                :test-targets ("@prj-dir@/target/test-classes2")
                :depends-on-modules nil
                :compile-deps
                (
                 "@java-home@/src.zip"
                 "@java-home@/lib/tools.jar"
                 "@java-home@/lib/jconsole.jar"
                 @jar-list@
                 )
                :runtime-deps nil
                :test-deps nil
                :doc-jars
                (
                 "@java-home@/src.zip"
                 "@java-home@/lib/tools.jar"
                 "@java-home@/lib/jconsole.jar"
                 @jar-list@
                 )
                :reference-source-roots
                (
                 "@java-home@/src.zip"
                 "@java-home@/lib/tools.jar"
                 "@java-home@/lib/jconsole.jar"
                 @jar-list@
                 )))
 :projects ((:id
             (:project "@prj-name@" :config "compile")
             :depends nil
             :sources ("@prj-dir@/src/main/java" "@prj-dir@/src/test/java")
             :targets ("@prj-dir@/target/classes2")
             :scalac-options ("-feature" "-deprecation" "-Xlint" "-Ywarn-dead-code" "-Ywarn-numeric-widen" "-Xfuture" "-Ymacro-no-expand")
             :javac-options nil
             :library-jars
             (
              "@java-home@/src.zip"
              "@java-home@/lib/tools.jar"
              "@java-home@/lib/jconsole.jar"
              @jar-list@
              )
             :library-sources
             (
              "@java-home@/src.zip"
              "@java-home@/lib/tools.jar"
              "@java-home@/lib/jconsole.jar"
              @jar-list@
              )
             :library-docs
             (
              "@java-home@/src.zip"
              "@java-home@/lib/tools.jar"
              "@java-home@/lib/jconsole.jar"
              @jar-list@
              ))))
