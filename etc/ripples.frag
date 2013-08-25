#version 150
uniform sampler2D tex;
in vec2 texCoord;
out vec4 fragColor;
in vec4 vcolor;

void main() {
  fragColor = min(vec4(1.0f), texture(tex, texCoord) + vcolor);
  //fragColor = vec4(1.0);
}
