open Webserver_types

val jslib_dir : string option Settings.setting
val host_name : string option Settings.setting
val port : int option Settings.setting

module Webserver : WEBSERVER
