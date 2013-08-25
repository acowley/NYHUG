__constant sampler_t sampler = CLK_NORMALIZED_COORDS_TRUE |
                               CLK_ADDRESS_CLAMP_TO_EDGE  |
                               CLK_FILTER_NEAREST;

__kernel void ripple(read_only image2d_t img, global float3* verts) {
  const size_t i = get_global_id(0);
  const float3 v = verts[i];
  const float2 c = v.xz * 0.5f + 0.5f;
  const float e = read_imagef(img, sampler, c).x;
  verts[i].y = e*0.1f;
}
