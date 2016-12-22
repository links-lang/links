open Webserver_types

val jslibdir : string Settings.setting
val host_name : string Settings.setting
val port : int Settings.setting

module Webserver : WEBSERVER
