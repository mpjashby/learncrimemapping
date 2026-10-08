# Create a London point without downloading data
london_point <- sf::st_sfc(sf::st_point(c(-0.1, 51.5)), crs = 4326)

# This target CRS covers London and should pass the area check
london_bng <- sf::st_transform(london_point, 27700)

# This target CRS covers Georgia, USA, and should attract feedback
london_georgia <- sf::st_transform(london_point, 26967)
