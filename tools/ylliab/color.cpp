#include "color.hpp"
#include <QDebug>
#include <complex>
#include <random>

// we generate colors at a fixed luminance of 85
constexpr float luminance = 85.0;

// parallelogram with minimum(-ish) area that covers the srgb gamut at luminance 80
constexpr std::complex<float> start_pos = {-54.02480553, -26.8951105};
constexpr std::complex<float> first_axis = {87.04113298, 4.56163248};
constexpr std::complex<float> second_axis = {-31.00257534, 108.11882904};

struct Color {
  float component[3];
  float &operator[](size_t i) { return component[i]; }
};

Color lin_srgb_to_srgb(Color srgb) {
  Color rgb;
  for (size_t i = 0; i < 3; i++) {
    // our linear srgb has range 0..100, but conversion needs 0..1
    srgb[i] /= 100.0;
    if (srgb[i] <= 0.0031308) {
      rgb[i] = 12.92 * srgb[i];
    } else {
      rgb[i] = 1.055 * std::pow(srgb[i], 1.0 / 2.4) - 0.055;
    }
  }
  return rgb;
}


constexpr float xyz_to_srgb_matrix[3][3] = {{3.2406, -1.5372, -0.4986},
                                            {-0.9689, 1.8758, 0.0415},
                                            {0.0557, -0.2040, 1.0570}};
Color xyz_to_lin_srgb(Color xyz) {
  Color srgb = {0, 0, 0};
  for (size_t i = 0; i < 3; i++) {
    for (size_t j = 0; j < 3; ++j) {
      srgb[i] += xyz[j] * xyz_to_srgb_matrix[i][j];
    }
  }
  return srgb;
}

// D65 white point
constexpr float x_n = 95.0489;
constexpr float y_n = 100.0;
constexpr float z_n = 108.884;

Color cielab_to_xyz(Color lab) {
  Color xyz;
  float fy = (lab[0] + 16.0) / 116.0;
  float fx = fy + lab[1] / 500.0;
  float fz = fy - lab[2] / 200.0;
  // we ignore the linear part since we only deal with high luminance
  // values
  xyz[0] = x_n * fx * fx * fx;
  xyz[1] = y_n * fy * fy * fy;
  xyz[2] = z_n * fz * fz * fz;
  return xyz;
}

// check whether our color falls within the srgb gamut
bool in_100_range(Color srgb) {
  for (auto comp : srgb.component) {
    if (comp < 0 || comp > 100) {
      return false;
    }
  }
  return true;
}

QColor random_color(size_t seed) {
  Color lab, lin_srgb;
  std::mt19937 rng(seed);
  std::uniform_real_distribution<float> dist(0, 1);
  // superstition
  dist(rng);
  lab[0] = luminance;
  // rejection sampling by generating a random point in the parallelogram
  do {
    auto val = start_pos + first_axis * dist(rng) + second_axis * dist(rng);
    lab[1] = val.real();
    lab[2] = val.imag();
    auto xyz = cielab_to_xyz(lab);
    lin_srgb = xyz_to_lin_srgb(xyz);
  } while (!in_100_range(lin_srgb));
  auto srgb = lin_srgb_to_srgb(lin_srgb);
  auto color = QColor::fromRgbF(srgb[0], srgb[1], srgb[2]);
  return color;
}