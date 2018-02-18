IF NOT "%1" == "" set JAVA_SDK=%1

set PATH=%JAVA_SDK%\bin;%PATH%

jar -xf %JAVA_SDK%\jre\lib\jce.jar
jar -0cf uncompressed_jce.jar javax META-INF
del /q javax META-INF
jvm2ada -jgnat %JAVA_SDK%\jre\lib\rt.jar ^
   -Luncompressed_jce.jar ^
   -L%JAVA_SDK%\jre\lib\charsets.jar ^
   -L%JAVA_SDK%\jre\lib\jsse.jar
