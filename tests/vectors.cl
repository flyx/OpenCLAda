__kernel void add (__global const int2 *a,
                   __global const int2 *b,
                   __global int2 *result) {
	int gid = get_global_id(0);
	result[gid] = a[gid] + b [gid];
}
