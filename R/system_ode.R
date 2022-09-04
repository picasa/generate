# simulation ####

#' Simulator for the Lotka-Volterra dynamical system
#' @description Describes the [dynamics](https://en.wikipedia.org/wiki/Lotka%E2%80%93Volterra_equations) of biological systems in which two species interact.
#' @param parameters α, β, γ, δ are positive real parameters describing the interaction of the two species. (numeric vector).
#' @param start_coords initial conditions (numeric vector)
#' @param t_max,t_step end time and step of the simulation (numeric).
#' @return a dataframe with time, x, and y columns
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
  ) |>
    as.data.frame() |> tibble::tibble()
}

#

#' Simulator for the Duffing dynamical system
#' @description Describes certain damped and driven oscillators. Coded from http://paulbourke.net/fractals/duffing/.
#' @param parameters a,b,c describe the amount of damping, the amplitude of the periodic driving force, and the angular frequency of the periodic driving force.
#' @param start_coords initial conditions (numeric vector)
#' @param t_max,t_step end time and step of the simulation (numeric).
#' @return a dataframe with time, x, and y columns
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
  ) |>
    as.data.frame() |>
    dplyr::mutate(iteration = dplyr::row_number()) |> tibble::tibble()
}

#' Simulator for the Lorenz dynamical system
#' @description Describes a simplified mathematical model for atmospheric convection. The parameters are set so that the system exhibits a chaotic behavior (σ = 10, ρ = 28, β = 8/3).
#' @param start_coords initial conditions (numeric vector)
#' @param t_max,t_step end time and step of the simulation (numeric).
#' @return a dataframe with time, x, and y columns
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
      sigma = 10,
      rho = 28,
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
  ) |>
    as.data.frame() |>
    dplyr::mutate(iteration = dplyr::row_number()) |> tibble::tibble()
}

#' Simulate discrete time quadratic maps
#' @description Based on the original article by J.C. Sprott (Sprott, J. C. "Automatic Generation of Strange Attractors." Comput. & Graphics 17, 325-332, 1993c). The map used formalized as
#' * xn+1 = a0 + a1 xn + a2 xn2 + a3 xn yn + a4 yn + a5 yn2
#' * yn+1 = a6 + a7 xn + a8 xn2 + a9 xn yn + a10 yn + a11 yn2
#' @param parameters a numeric vector of 12 values
#' @param start_coords initial conditions (numeric vector)
#' @param iterations number of iterations (integer)
#' @return a dataframe with time, x, and y columns
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
  ) |>
    as.data.frame() |>
    dplyr::mutate(iteration = dplyr::row_number()) |> tibble::tibble()
}


#' Simulate an ODE system with initial conditions sampled from a LHS design
#' @param p parameter vector for the ODE system
#' @param n number of particles to be simulated
#' @param t,s end time and step of the simulation (numeric).
#' @param x0,y0 scaling parameters for the initial conditions
#' @export
gen_field <- function(p, n, t, s = 1/100, x0 = c(-1,1), y0 = c(0.5,1)) {

  # TODO : code f parameter to handle different ODE

  # set up a LHS design for initial conditions
  design_initial <- lhs::randomLHS(n = n, k=2) |>
    tibble::as_tibble() |> purrr::set_names(c("x","y")) |>
    dplyr::mutate(
      x = scales::rescale(x, to = x0),
      y = scales::rescale(y, to = y0)) |>
    dplyr::mutate(i = purrr::map2(x, y, ~ c(x = ..1, y= ..2))) |>
    dplyr::mutate(id = seq_along(x)) |> dplyr::select(id, i)

  design <- tidyr::crossing(tibble::tibble(p = list(p)), design_initial)

  # simulate
  data <- design |>
    dplyr::mutate(trace = furrr::future_map2(p, i, ~ simulate_duffing(
      parameters = ..1, start_coords = ..2, t_max = t, t_step = s)))

  return(data |> tidyr::unnest(trace))

}
