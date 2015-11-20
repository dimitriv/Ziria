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
echo "make sure TS_PROFILE is undefined"
echo "starting barrier testing..."

for I in {1..100}

echo "Execution time increase test" >> output_b.text
echo "" >> output_b.text

do
	sed s/EXEC_TIME_A/$I/g mbench_barriers.blk > temp_b.blk
	sed -i s/EXEC_TIME_B/$I/g temp_b.blk
	echo "N="$I >> output_b.text
	echo "" >> output_b.text
	EXTRAOPTS="--no-atom-threads=2 --optimism=1 --atomix-codegen --atom-state-profiling" make temp_b.perf -B | sed -n -e '/SORA Queue/,/RESERVE/p' >> output_b.text
	echo "" >> output_b.text
done

echo "Balancing test">> output_b.text
echo "" >> output_b.text

for I in {1..100}
do
	sed s/EXEC_TIME_A/1/g mbench_barriers.blk > temp_b.blk
	sed -i s/EXEC_TIME_B/$I/g temp_b.blk
	EXEC=$(python -c "print 1.0 / "$I)
	echo "A/D="$EXEC >> output_b.text
	echo "" >> output_b.text
	EXTRAOPTS="--no-atom-threads=2 --optimism=1 --atomix-codegen --atom-state-profiling" make temp_b.perf -B | sed -n -e '/SORA Queue/,/RESERVE/p' >> output_b.text
	echo "" >> output_b.text
done

for I in {1..100}
do
	sed s/EXEC_TIME_A/$I/g mbench_barriers.blk > temp_b.blk
	sed -i s/EXEC_TIME_B/1/g temp_b.blk
	echo "A/D="$I >> output_b.text
	echo "" >> output_b.text
	EXTRAOPTS="--no-atom-threads=2 --optimism=1 --atomix-codegen --atom-state-profiling" make temp_b.perf -B | sed -n -e '/SORA Queue/,/RESERVE/p' >> output_b.text
	echo "" >> output_b.text
done

rm temp_b.blk
echo "done! see output_b.txt"