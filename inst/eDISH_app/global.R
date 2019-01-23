# temporarily load deps
library(safetyGraphics)
library(shiny)
library(shinyjs)
library(dplyr)
library(purrr)
library(stringr)
library(DT)

## source modules
source('modules/renderSettings/renderSettingsUI.R')
source('modules/renderSettings/renderSettings.R')

source('modules/renderChart/renderChartUI.R')
source('modules/renderChart/renderChart.R')

source('modules/renderChart/renderEDishChartUI.R')
source('modules/renderChart/renderEDishChart.R')

source('modules/renderChart/renderSafetyHistogramChartUI.R')
source('modules/renderChart/renderSafetyHistogramChart.R')

source('modules/dataUpload/dataUploadUI.R')
source('modules/dataUpload/dataUpload.R')

