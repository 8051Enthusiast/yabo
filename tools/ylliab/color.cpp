#include "color.hpp"
#include <QDebug>
#include <complex>
#include <random>

bool use_dark_random_colors = false;

struct ColorSpaceSlice {
  float luminance;
  // parallelogram with minimum(-ish) area that covers the srgb gamut in CIELAB
  // at luminance
  std::complex<float> start_pos;
  std::complex<float> first_axis;
  std::complex<float> second_axis;
};

static constexpr ColorSpaceSlice LIGHT_SPACE = {85.0,
                                                {-54.02480553, -26.8951105},
                                                {87.04113298, 4.56163248},
                                                {-31.00257534, 108.11882904}};

static constexpr ColorSpaceSlice DARK_SPACE = {70.0,
                                               {-36.02653126, -51.09285043},
                                               {108.65374119, 5.69430129},
                                               {-37.05923352, 121.21529099}};

static ColorSpaceSlice const &current_slice() {
  return use_dark_random_colors ? DARK_SPACE : LIGHT_SPACE;
}

struct Color {
  float component[3];
  float &operator[](size_t i) { return component[i]; }
};

static Color lin_srgb_to_srgb(Color srgb) {
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
static Color xyz_to_lin_srgb(Color xyz) {
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

static Color cielab_to_xyz(Color lab) {
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
static bool in_100_range(Color srgb) {
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
  auto &slice = current_slice();
  lab[0] = slice.luminance;
  // rejection sampling by generating a random point in the parallelogram
  do {
    auto val = slice.start_pos + slice.first_axis * dist(rng) +
               slice.second_axis * dist(rng);
    lab[1] = val.real();
    lab[2] = val.imag();
    auto xyz = cielab_to_xyz(lab);
    lin_srgb = xyz_to_lin_srgb(xyz);
  } while (!in_100_range(lin_srgb));
  auto srgb = lin_srgb_to_srgb(lin_srgb);
  auto color = QColor::fromRgbF(srgb[0], srgb[1], srgb[2]);
  return color;
}

static constexpr std::array<QColor, 16> ansi_colors = {
    QColor(0, 0, 0),      QColor(205, 0, 0),     QColor(0, 205, 0),
    QColor(205, 205, 0),  QColor(0, 0, 238),     QColor(205, 0, 205),
    QColor(0, 205, 205),  QColor(229, 229, 229), QColor(127, 127, 127),
    QColor(255, 0, 0),    QColor(0, 255, 0),     QColor(255, 255, 0),
    QColor(92, 92, 255),  QColor(255, 0, 255),   QColor(0, 255, 255),
    QColor(255, 255, 255)};

QColor ansi_color_16(uint8_t color) { return ansi_colors.at(color); }

static constexpr std::array<uint8_t, 6> col6 = {0x00, 0x5f, 0x87,
                                                0xaf, 0xd7, 0xff};
static constexpr uint8_t base = col6.size();

QColor ansi_color_256(uint8_t color) {
  if (color < 16) {
    return ansi_color_16(color);
  }
  color -= 16;
  if (color < base * base * base) {
    uint8_t r = color / (base * base);
    uint8_t g = (color / base) % base;
    uint8_t b = color % base;
    return QColor(col6[r], col6[g], col6[b]);
  }
  color -= base * base * base;
  auto shade = color * 10 + 8;
  return QColor(shade, shade, shade);
}