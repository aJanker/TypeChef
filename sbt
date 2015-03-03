#!/bin/sh
java -XX:PermSize=256m -Xmx786m -Xms128m -Xss10m -jar sbt-launch.jar "$@"
