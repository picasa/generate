# simulation ####

# Lotka-Volterra
#' @export
#'
simulate_lv <- function(
  parameters,
  start_coords = c(x = 0.01, y = 0.01),
  t_max = 10,
  t_step = 1/100
) {
  deSolve::ode(
    y = start_coords,
    times = seq(0, t_max, by = t_step),
    parms = parameters,
    func = function(t, y, p) {
      with(as.list(c(y, p)), {
        dx = p[1] * x  - p[2]* x * y
        dy = p[3] * x *y - p[4] * y

        list(c(dx, dy))
      })
    }
  ) %>%
    as.data.frame() %>% tibble::tibble()
}

# http://paulbourke.net/fractals/duffing/
# a, "amount of damping",
# b, "amplitude of the periodic driving force",
# w, "angular frequency of the periodic driving force."

#' @export
#'
simulate_duffing <- function(
  parameters,
  start_coords = c(x = 0, y = 0),
  t_max = 10,
  t_step = 1/100
) {
  deSolve::ode(
    y = start_coords,
    times = seq(0, t_max, by = t_step),
    parms = parameters,
    func = function(time, state, p) {
      with(as.list(c(state, p)), {
        dx = y
        dy = x - x^3 - p[1] * y + p[2] * cos(p[3] * time)

        list(c(dx, dy))
      })
    }
  ) %>%
    as.data.frame() %>%
    dplyr::mutate(iteration = dplyr::row_number()) %>% tibble::tibble()
}

#' @export
simulate_lorenz <- function(
  start_coords = c(x = 1, y = 1, z = 1),
  t_max = 10,
  t_step = 1/100
) {
  deSolve::ode(
    y = start_coords,
    times = seq(0, t_max, by = t_step),
    parms = list(
      rho = 28,
      sigma = 10,
      beta = 8/3
    ),
    func = function(time, state, params) {
      with(as.list(c(state, params)), {
        dx = sigma * (y - x)
        dy = x * (rho - z) - y
        dz = x * y - beta * z

        list(c(dx, dy, dz))
      })
    }
  ) %>%
    as.data.frame() %>%
    dplyr::mutate(iteration = dplyr::row_number()) %>% tibble::tibble()
}

#' @export
simulate_quadratic <- function(
  parameters,
  start_coords = c(x = 0, y = 0),
  iterations = 100
) {
  deSolve::ode(
    y = start_coords,
    times = 0:iterations,
    parms = parameters,
    method = "iteration",
    func = function(time, state, p) {
      with(as.list(c(state, p)), {
        dx = p[1] + p[2]*x + p[3]*x^2 + p[ 4]*x*y + p[ 5]*y + p[ 6]*y^2
        dy = p[7] + p[8]*x + p[9]*x^2 + p[10]*x*y + p[11]*y + p[12]*y^2

        list(c(dx, dy))
      })
    }
  ) %>%
    as.data.frame() %>%
    dplyr::mutate(iteration = dplyr::row_number()) %>% tibble::tibble()
}


# simulate ODE system with initial conditions from a LHS design
# TODO : code f parameter to handle different ODE

#' @export
gen_field <- function(p, n, t, s = 1/100, x0 = c(-1,1), y0 = c(0.5,1), f = NULL) {

  # set up a LHS design for initial conditions
  design_initial <- lhs::randomLHS(n = n, k=2) %>%
    tibble::as_tibble() %>% purrr::set_names(c("x","y")) %>%
    dplyr::mutate(
      x = scales::rescale(x, to = x0),
      y = scales::rescale(y, to = y0)) %>%
    dplyr::mutate(i = purrr::map2(x, y, ~ c(x = ..1, y= ..2))) %>%
    dplyr::mutate(id = seq_along(x)) %>% dplyr::select(id, i)

  design <- tidyr::crossing(tibble::tibble(p = list(p)), design_initial)

  # simulate
  data <- design %>%
    dplyr::mutate(trace = furrr::future_map2(p, i, ~ simulate_duffing(
      parameters = ..1, start_coords = ..2, t_max = t, t_step = s)))

  return(data %>% tidyr::unnest(trace))

}

# sample equidistant points on the perimeter of l concentric circles of r radius
#' @export
#'
gen_vessel <- function(x0 = 0, y0 = 0, l = 5, r = 1/100, jitter = 1/500) {
  tibble::tibble(l = 1:l) %>%
    dplyr::mutate(n = l * 10, r = seq(1/40, r * dplyr::n(), len = dplyr::n())) %>%
    dplyr::mutate(data = purrr::map2(n, r, ~ sample_perimeter(n = ..1, r = ..2))) %>%
    dplyr::select(l, data) %>% tidyr::unnest(data) %>%
    dplyr::mutate(x = x + x0, y = y + y0) %>%
    dplyr::mutate(dplyr::across(x:y, ~ jitter(., a = jitter)))
}

