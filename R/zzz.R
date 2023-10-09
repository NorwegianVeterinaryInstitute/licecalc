## global variables

ParameterHunnlusSann <- c(-3.22963, 0.89694)
ParametereMerdCount <- c(-0.11920679, 0.64625410, 0.09369016, 0.19196262, 0.35336360, 0.19125739, -0.11604661)
ParametereMerdZero <- c(0.4520806, -1.3697959, -0.3342084, -0.6574109, -0.2297210, 0.3067283)
thetaHele <- 1.997114
ParametereHeleCount <- c(0.62806465, 0.24380189, 0.05246029, 0.53724600, 0.14725647, 0.29432465, 0.15317690, -0.08139975)
ParametereHeleZero <- c(-1.3095828, -0.7798661, -1.0818692, -0.2903852, -0.5847494, -0.1541673, 0.2945743)
thetaMerd <- 1.693027
myhele <- "Merdvis"

utils::globalVariables(c(
  ParameterHunnlusSann, ParametereMerdCount,
  ParametereMerdZero, thetaHele, ParametereHeleCount,
  ParametereHeleZero, thetaMerd, myhele
))
