#version 330

layout(location = 0) in vec4 vertexPosition;
layout(location = 1) in vec3 vertexNormal;

uniform mat4 MV;
uniform mat4 P;
uniform mat4 rot;

out vec3 myNormal;
out vec4 myVertex;
out mat4 MVf;

void main() {
    gl_Position = P * MV * (rot * vertexPosition);
    myNormal = mat3(rot) * vertexNormal;
    myVertex = rot * vertexPosition;
    MVf = MV;
}
