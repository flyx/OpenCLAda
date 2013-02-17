__kernel void add (int2 source,
                   __global int2 *result) {
  result[0] = source;
}
