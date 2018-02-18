#/bin/sh

if [ "$1" != "" ]; then
   JAVA_SDK="$1"
fi

PATH="$JAVA_SDK/bin:$PATH"
export PATH

jar -xf "$JAVA_SDK/jre/lib/jce.jar"
jar -0cf uncompressed_jce.jar javax META-INF
rm -r javax META-INF
jvm2ada -jgnat "$JAVA_SDK/jre/lib/rt.jar" \
   "-Luncompressed_jce.jar" \
   "-L$JAVA_SDK/jre/lib/charsets.jar" \
   "-L$JAVA_SDK/jre/lib/jsse.jar"
