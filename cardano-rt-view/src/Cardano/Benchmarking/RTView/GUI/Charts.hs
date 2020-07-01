{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Benchmarking.RTView.GUI.Charts
    ( prepareChartsJS
    -- Charts JS snippets.
    , memoryUsageChartJS
    , cpuUsageChartJS
    , diskUsageChartJS
    , networkUsageChartJS
    -- Charts updaters.
    , updateMemoryUsageChartJS
    , updateCPUUsageChartJS
    , updateDiskUsageChartJS
    , updateNetworkUsageChartJS
    ) where

import           Cardano.Prelude
import           Prelude
                   ( String )

prepareChartsJS :: String
prepareChartsJS = concat
  [ "window.charts = new Map();"
  ]

memoryUsageChartJS :: String
memoryUsageChartJS = concat
  [ "var ctx = document.getElementById(%1).getContext('2d');"
  , "var chart = new Chart(ctx, {"
  , "  type: 'line',"
  , "  data: {"
  , "    labels: [],"
  , "    datasets: [{"
  , "      label: 'Memory usage',"
  , "      backgroundColor: '#FF8000',"
  , "      borderColor: '#FF8000',"
  , "      data: [],"
  , "      fill: false"
  , "    }]"
  , "  },"
  , "  options: {"
  , "    scales: {"
  -- , "      xAxes: [{"
  -- , "         type: 'time'"
  -- , "         time: {"
  -- , "           parser: timeFormat,"
  -- , "           tooltipFormat: 'HH:mm:ss'"
  -- , "         }"
  -- , "      }],"
  , "      yAxes: [{"
  , "        display: true,"
  , "        scaleLabel: {"
  , "          display: true,"
  , "          labelString: 'MB'"
  , "        },"
  , "        ticks: {"
  , "          min: 0,"
  , "          max: 200,"
  , "          stepSize: 50"
  , "        }"
  , "      }]"
  , "    }"
  , "  }"
  , "});"
  , "window.charts.set(%1, chart);"
  ]

cpuUsageChartJS :: String
cpuUsageChartJS = concat
  [ "var ctx = document.getElementById(%1).getContext('2d');"
  , "var chart = new Chart(ctx, {"
  , "  type: 'line',"
  , "  data: {"
  , "    labels: [],"
  , "    datasets: [{"
  , "      label: 'CPU usage',"
  , "      backgroundColor: '#FE2E2E',"
  , "      borderColor: '#FE2E2E',"
  , "      data: [],"
  , "      fill: false"
  , "    }]"
  , "  },"
  , "  options: {"
  , "    scales: {"
  , "      yAxes: [{"
  , "        display: true,"
  , "        scaleLabel: {"
  , "          display: true,"
  , "          labelString: 'Percent'"
  , "        },"
  , "        ticks: {"
  , "          min: 0,"
  , "          max: 100,"
  , "          stepSize: 25"
  , "        }"
  , "      }]"
  , "    }"
  , "  }"
  , "});"
  , "window.charts.set(%1, chart);"
  ]

diskUsageChartJS :: String
diskUsageChartJS = concat
  [ "var ctx = document.getElementById(%1).getContext('2d');"
  , "var chart = new Chart(ctx, {"
  , "  type: 'line',"
  , "  data: {"
  , "    labels: [],"
  , "    datasets: [{"
  , "      label: 'Disk | RD',"
  , "      backgroundColor: '#0080FF',"
  , "      borderColor: '#0080FF',"
  , "      data: [],"
  , "      fill: false"
  , "    },{"
  , "      label: 'Disk | WR',"
  , "      backgroundColor: '#D358F7',"
  , "      borderColor: '#D358F7',"
  , "      data: [],"
  , "      fill: false"
  , "    }]"
  , "  },"
  , "  options: {"
  , "    scales: {"
  , "      yAxes: [{"
  , "        display: true,"
  , "        scaleLabel: {"
  , "          display: true,"
  , "          labelString: 'KB/s'"
  , "        },"
  , "        ticks: {"
  , "          min: 0,"
  , "          stepSize: 50"
  , "        }"
  , "      }]"
  , "    }"
  , "  }"
  , "});"
  , "window.charts.set(%1, chart);"
  ]

networkUsageChartJS :: String
networkUsageChartJS = concat
  [ "var ctx = document.getElementById(%1).getContext('2d');"
  , "var chart = new Chart(ctx, {"
  , "  type: 'line',"
  , "  data: {"
  , "    labels: [],"
  , "    datasets: [{"
  , "      label: 'Network | IN',"
  , "      backgroundColor: '#D7DF01',"
  , "      borderColor: '#D7DF01',"
  , "      data: [],"
  , "      fill: false"
  , "    },{"
  , "      label: 'Network | OUT',"
  , "      backgroundColor: '#00FF80',"
  , "      borderColor: '#00FF80',"
  , "      data: [],"
  , "      fill: false"
  , "    }]"
  , "  },"
  , "  options: {"
  , "    scales: {"
  , "      yAxes: [{"
  , "        display: true,"
  , "        scaleLabel: {"
  , "          display: true,"
  , "          labelString: 'KB/s'"
  , "        },"
  , "        ticks: {"
  , "          min: 0,"
  , "          stepSize: 100"
  , "        }"
  , "      }]"
  , "    }"
  , "  }"
  , "});"
  , "window.charts.set(%1, chart);"
  ]

-- Chart updaters.
-- Please note that after 900 data points (which are collected in every 30 minutes)
-- we remove outdated points. It allows to avoid too compressed, narrow charts.

updateMemoryUsageChartJS
  , updateCPUUsageChartJS
  , updateDiskUsageChartJS
  , updateNetworkUsageChartJS :: String
updateMemoryUsageChartJS  = updateSingleDatasetChartJS
updateCPUUsageChartJS     = updateSingleDatasetChartJS
updateDiskUsageChartJS    = updateDoubleDatasetChartJS
updateNetworkUsageChartJS = updateDoubleDatasetChartJS

updateSingleDatasetChartJS :: String
updateSingleDatasetChartJS = concat
  [ "window.charts.get(%1).data.labels.push(%2);"
  , "var len = window.charts.get(%1).data.labels.length;"
  , "if (len == 900) {"
  , "  window.charts.get(%1).data.datasets[0].data.splice(0, len);"
  , "  window.charts.get(%1).data.labels.splice(0, len);"
  , "}"
  , "window.charts.get(%1).data.datasets[0].data.push(%3);"
  , "window.charts.get(%1).update({duration: 0});"
  ]

updateDoubleDatasetChartJS :: String
updateDoubleDatasetChartJS = concat
  [ "window.charts.get(%1).data.labels.push(%2);"
  , "var len = window.charts.get(%1).data.labels.length;"
  , "if (len == 900) {"
  , "  window.charts.get(%1).data.datasets[0].data.splice(0, len);"
  , "  window.charts.get(%1).data.datasets[1].data.splice(0, len);"
  , "  window.charts.get(%1).data.labels.splice(0, len);"
  , "}"
  , "window.charts.get(%1).data.datasets[0].data.push(%3);"
  , "window.charts.get(%1).data.datasets[1].data.push(%4);"
  , "window.charts.get(%1).update({duration: 0});"
  ]
