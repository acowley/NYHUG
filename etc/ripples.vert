#version 150
in vec3 vertexPos;
uniform mat4 proj;
out vec2 texCoord;
out vec4 vcolor;

void main() {
  vcolor = vec4(0.0f, 0.0f, vertexPos.y > 0.0f ? 1.0f : 0.0f, 0.0f);
  //vcolor = vec4(0.0f,0.0f,vertexPos.y * 100.0f, 0.0f);
  texCoord = vertexPos.xz * 0.5f + 0.5f;
  gl_Position = proj * vec4(vertexPos, 1.0);
}
