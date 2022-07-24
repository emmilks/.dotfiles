#!/bin/sh

apt list --manual-installed | awk -F "/" '{print $1}' | grep -v 'rstudio\|Listing\|lib' -
