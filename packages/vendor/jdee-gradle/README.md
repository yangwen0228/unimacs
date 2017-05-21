## Overview

Support for using JDEE in Gradle projects.
This includes:

  * Building software directly from source files, using JDEE.
  * Support for single- and multi- project Gradle builds.
  * Template driven generation of JDEE project files.
  * Optional generation of JDEE class paths from Gradle.


## Requirements

Other required software, not included in this distribution:

  * [gradle-el](https://github.com/vhallac/gradle-el)
  * [jdee](https://github.com/jdee-emacs/jdee)


## Usage

1. Copy the file [jdee-gradle](lisp/jdee-gradle.el) to a directory on your Emacs load-path.
   You can byte-compile it, but that isn't necessary.

1. Modify or create the JDEE project file (prj.el) at the root of your Gradle project
   to include a call to the function `jdee-gradle-set-project`.
   See the examples in the [samples](samples) directory.
   You can use the functions `jdee-gradle-gen-single-project-file`, `jdee-gradle-gen-multi-project-file`,
   and `jdee-gradle-gen-sub-project-file` to add appropriate code to your prj.el file.

You can optionally arrange for Gradle to generate an additional project file that sets various JDEE
class path variables correctly based on the classes and libraries used by Gradle.

1. Put the Gradle source file [jdee.gradle](lib/jdee.gradle) somewhere that can be easily found.
   I put a copy in the project source tree, but you can put it anywhere.

1. Edit the project build.gradle file to include that source file, for example by adding the line

        apply from: "path/to/file/jdee.gradle"

1. Execute the jdee task to create the prj-generated files:

        $ gradlew assemble jdee

   You will need to re-run the jdee task occasionally to keep the generated file up-to-date.

1. Modify or create a JDEE project file to load the file prj-generated.el, if it exists.

