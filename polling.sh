#!/bin/bash

while inotifywait -qqre modify "core"; do
	make
done
