#version 150
in vec3 vertexPos;
uniform mat4 proj;
out vec2 texCoord;

void main() {
  float tu = vertexPos.x < 0.0f ? 0.0f : 1.0f;
  float tv = vertexPos.z < 0.0f ? 0.0f : 1.0f;
  texCoord = vec2(tu,tv);
  gl_Position = proj * vec4(vertexPos, 1.0);
}
