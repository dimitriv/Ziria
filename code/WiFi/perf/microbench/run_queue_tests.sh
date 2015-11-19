# 
# Copyright (c) Microsoft Corporation
# All rights reserved. 
#
# Licensed under the Apache License, Version 2.0 (the ""License""); you
# may not use this file except in compliance with the License. You may
# obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# THIS CODE IS PROVIDED ON AN *AS IS* BASIS, WITHOUT WARRANTIES OR
# CONDITIONS OF ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING WITHOUT
# LIMITATION ANY IMPLIED WARRANTIES OR CONDITIONS OF TITLE, FITNESS FOR
# A PARTICULAR PURPOSE, MERCHANTABLITY OR NON-INFRINGEMENT.
#
# See the Apache Version 2.0 License for specific language governing
# permissions and limitations under the License.
#
#
echo "make sure TS_PROFILE is defined"
echo "starting queue testing..."

for I in {1..100}

echo "Execution time increase test" >> output_q.text
echo "" >> output_q.text

do
	sed s/EXEC_TIME_A/$I/g mbench_queues.blk > temp.blk
	sed -i s/EXEC_TIME_B/$I/g temp.blk
	echo "N="$I >> output_q.text
	echo "" >> output_q.text
	EXTRAOPTS="--no-atom-threads=2 --optimism=1 --atomix-codegen --atom-state-profiling" make temp.perf -B | sed -n -e '/SORA Queue/,/RESERVE/p' >> output_q.text
	echo "" >> output_q.text
done

echo "Balancing test">> output_q.text
echo "" >> output_q.text

for I in {1..100}
do
	sed s/EXEC_TIME_A/1/g mbench_queues.blk > temp.blk
	sed -i s/EXEC_TIME_B/$I/g temp.blk
	EXEC=$(python -c "print 1.0 / "$I)
	echo "A/B="$EXEC >> output_q.text
	echo "" >> output_q.text
	EXTRAOPTS="--no-atom-threads=2 --optimism=1 --atomix-codegen --atom-state-profiling" make temp.perf -B | sed -n -e '/SORA Queue/,/RESERVE/p' >> output_q.text
	echo "" >> output_q.text
done

for I in {1..100}
do
	sed s/EXEC_TIME_A/$I/g mbench_queues.blk > temp.blk
	sed -i s/EXEC_TIME_B/1/g temp.blk
	echo "A/B="$I >> output_q.text
	echo "" >> output_q.text
	EXTRAOPTS="--no-atom-threads=2 --optimism=1 --atomix-codegen --atom-state-profiling" make temp.perf -B | sed -n -e '/SORA Queue/,/RESERVE/p' >> output_q.text
	echo "" >> output_q.text
done

rm temp.blk
echo "done! see output_q.txt"