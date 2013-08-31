__constant sampler_t sampler = CLK_NORMALIZED_COORDS_TRUE |
                               CLK_ADDRESS_CLAMP_TO_EDGE  |
                               CLK_FILTER_LINEAR;

__kernel void ripple(read_only image2d_t img, global float4* verts) {
  const size_t i = get_global_id(0);
  const float4 v = verts[i];
  const float2 c = v.xz * 0.5f + 0.5f;
  const float e = read_imagef(img, sampler, c).x;
  verts[i] = (float4)(v.x,e*0.1f,v.z,v.w);
}

__constant sampler_t samplernn = CLK_NORMALIZED_COORDS_FALSE |
                                 CLK_ADDRESS_CLAMP_TO_EDGE  |
                                 CLK_FILTER_NEAREST;

__kernel void localMax(read_only image2d_t img, 
                       write_only image2d_t blurred,
                       int2 step,
                       int maskSize) {
  const size_t u = get_global_id(0);
  const size_t v = get_global_id(1);
  const int2 uv = (int2)(u,v);
  float m = 0.0f;
  for(int i = -maskSize; i < maskSize+1; ++i) {
    m = max(m, read_imagef(img, samplernn, uv+i*step).x);
  }
  write_imagef(blurred, uv, m);
}
