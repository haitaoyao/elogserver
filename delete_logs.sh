#!/bin/bash
set -u -e
##################################################################
# 
# written by haitao.yao @ 2012-05-28.14:41:37
# 
# 
# 
##################################################################
current_dir="$(cd $(dirname $0);pwd)"

log_max_day=30
log_folder='/data/logs/elogserver'
log_expire_time=$(date +%s -d "$log_max_day days ago")

for file_path in $(find $log_folder -type f -name '*.log.*')
do
	file_name=$(basename $file_path)
	file_date=$(echo $file_name|awk -F '.log.' '{print $2}')
	if [ -n "$file_date" ]
	then
		file_time=$(date -d "$file_date" +%s)
		if [ "$file_type" -lt "$log_expire_time" ]
		then
			rm $file_path
			echo "$file_path deleted" 
		fi
	fi
done

