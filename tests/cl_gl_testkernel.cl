__kernel void cl_gl_testkernel(__write_only image2d_t texture)
{

   int x = get_global_id(0);
   int y = get_global_id(1);

	 int w = get_global_size(0)-1;
	 int h = get_global_size(1)-1;

   int2 coords = (int2)(x,y);

	 float red = ((float)x+(float)y)/((float)w+(float)h);

   float4 val = (float4)(red, 0.0f, 0.0f, 1.0f);
   write_imagef(texture, coords, val);

}
