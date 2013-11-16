#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <iostream>
#include <string>
#include <iomanip>

/* camera */
glm::vec3 up = glm::vec3(0.0, -1.0, 0.0);
glm::vec3 eye = glm::vec3(0.0, 0.0, 0.0);
glm::vec3 center = glm::vec3(1.0, 0.0, 0.0);

int rotatePitch(char *);
int rotateYaw(char *);
int rotateRoll(char *);

float angleToFloat(char *angle)
{
    return std::stof(std::string(angle));
}

void vec3Print(std::string name, const glm::vec3 &vec)
{
    std::cout << name << ":\t"
              << vec[0] << " " << vec[1] 
              << " " << vec[2] << "\n";
}

void mat3Print(std::string name, const glm::mat3 &mat)
{
    std::cout << name << "\t:\n"
              << mat[0][0] << " " << mat[0][1] << " " << mat[0][2] << "\n"
              << mat[1][0] << " " << mat[1][1] << " " << mat[1][2] << "\n"
              << mat[2][0] << " " << mat[2][1] << " " << mat[2][2] << "\n"
              << "--------------------\n";
}

void printCamera()
{
    std::cout << "Camera:\n";
    vec3Print("Up", up);
    vec3Print("Eye", eye);
    vec3Print("Center", center);
    std::cout << "--------------------\n";
}

int main(int argc, char **argv)
{
    if (argc < 3) {
        std::cout << "Too few arguments\n";
        return 1;
    }
    std::cout << std::setiosflags(std::ios::fixed)
              << std::setprecision(9)
              << std::setw(18)
              << std::left;
    if (std::string(argv[1]) == std::string("yaw")) return rotateYaw(argv[2]);
    if (std::string(argv[1]) == std::string("pitch")) return rotatePitch(argv[2]);
    if (std::string(argv[1]) == std::string("roll")) return rotateRoll(argv[2]);
    std::cout << "Wut?\n";
    return 1;
}
    
int rotatePitch(char *string_angle)
{
    float angle = angleToFloat(string_angle);

    glm::vec3 dir = center - eye;
    dir = glm::normalize(dir);
    vec3Print("Dir", dir);
    glm::vec3 axis = glm::cross(dir, up);
    vec3Print("Axis", axis);
    glm::mat4 rot = glm::rotate(glm::mat4(1.0), angle, axis);
    glm::mat3 rot3(rot);
    mat3Print("Rot", rot3);
    up = rot3 * up;
    vec3Print("New up\t", up);
    dir = rot3 * dir;
    vec3Print("New dir", dir);
    center = dir + eye;
    vec3Print("New center", center);
    return 0;
}

int rotateYaw(char *string_angle)
{
    float angle = angleToFloat(string_angle);
    glm::vec3 dir = center - eye;
    dir = glm::normalize(dir);
    glm::vec3 axis = glm::normalize(up);
    glm::mat4 rot = glm::rotate(glm::mat4(1.0), angle, axis);
    glm::mat3 rot3(rot);
    mat3Print("Rot", rot3);
    dir = rot3 * dir;
    vec3Print("New dir", dir);
    center = dir + eye;
    vec3Print("New center", center);

    return 0;
}

int rotateRoll(char *string_angle)
{
    return 0;
}
