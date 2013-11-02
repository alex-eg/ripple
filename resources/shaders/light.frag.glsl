# version 430 core

in vec3 myNormal;
in vec4 myVertex;
in mat4 MVf;

uniform mat4 MV;
uniform vec4 lightPosition;
uniform vec4 lightColor;

uniform vec4 ambient;
uniform vec4 diffuse;
uniform vec4 specular;
uniform vec4 emission;
uniform float shininess;

out vec4 fragmentColor;

vec4 ComputeLight (const in vec3 direction,
                   const in vec4 lightcolor,
                   const in vec3 normal,
                   const in vec3 halfvec,
                   const in vec4 mydiffuse,
                   const in vec4 myspecular,
                   const in float myshininess) {

    float nDotL = dot(normal, direction);
    vec4 lambert = mydiffuse * lightcolor * max (nDotL, 0.0);

    float nDotH = dot(normal, halfvec);
    vec4 phong = myspecular * lightcolor * pow (max(nDotH, 0.0), myshininess);
    vec4 retval = lambert + phong;
    return retval;
}

void main (void)
{
    vec4 finalcolor = vec4(0.0, 0.0, 0.0, 1.0);

    const vec3 eyepos = vec3(0,0,0);

    vec4 _mypos = MVf * myVertex;
    vec3 mypos = _mypos.xyz / _mypos.w;
    vec3 eyedirn = normalize(eyepos - mypos);

    mat4 MVit = transpose(inverse(MVf));
    vec3 _normal = (MVit*vec4(myNormal,0.0)).xyz;
    vec3 normal = normalize(myNormal);

    vec3 direction;
    if (lightPosition.w == 0) {
        direction = normalize(lightPosition.xyz);
    } else {
        vec3 position = lightPosition.xyz / lightPosition.w;
        direction = normalize (position - mypos) ; // no attenuation
    }
    vec3 halfv = normalize(direction + eyedirn);
    vec4 col = ComputeLight(direction, lightColor, normal, halfv, diffuse, specular, shininess);

    finalcolor += col;

    finalcolor += (ambient + emission);
    fragmentColor = finalcolor;
    // fragmentColor = vec4(.5, .3, .2, 1.0);
}
