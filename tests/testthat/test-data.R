context("data.R")

# TODO: Need some tests that fail (error)

test_that("Understory width factors", {
  expect_equal(
    dim(underWidthFac),
    c(12, 8)
  )
})

test_that("Understory width factor names", {
  expect_equal(
    names(underWidthFac),
    c("Year", "HG", "PRD", "CC", "SoG", "WCVI", "A27", "A2W")
  )
})

test_that("Parameters is a list", {
  expect_equal(
    is.list(pars),
    TRUE
  )
})

test_that("Parameters has names", {
  expect_equal(
    names(pars),
    c("conversion", "SOK", "surface", "macrocystis", "understory")
  )
})

test_that("Parameters has names: conversion", {
  expect_equal(
    names(pars$conversion),
    c("omega", "female")
  )
})

test_that("Parameters has names: SOK", {
  expect_equal(
    names(pars$SOK),
    c("nu", "upsilon", "M")
  )
})

test_that("Parameters has names: surface", {
  expect_equal(
    names(pars$surface),
    c("alpha", "beta")
  )
})

test_that("Parameters has names: macrocystis", {
  expect_equal(
    names(pars$macrocystis),
    c("xi", "gamma", "delta", "epsilon")
  )
})

test_that("Parameters has names: understory", {
  expect_equal(
    names(pars$understory),
    c("varphi", "vartheta", "varrho", "varsigma")
  )
})

test_that("Intensity", {
  expect_equal(
    dim(intensity),
    c(9, 3)
  )
})

test_that("Intensity names", {
  expect_equal(
    names(intensity),
    c("Intensity", "Description", "Layers")
  )
})

test_that("Algae coefficients", {
  expect_equal(
    dim(algaeCoefs),
    c(8, 3)
  )
})

test_that("Algae coefficient names", {
  expect_equal(
    names(algaeCoefs),
    c("AlgaeName", "AlgType", "Coef")
  )
})
